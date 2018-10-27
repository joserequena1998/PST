
package body Client_Handler is

	-- + Variable: Número de secuencia del servidor inicializada al
	-- 	 primer valor del tipo Seq_N_T
	Seq_N_Server: CM.Seq_N_T := CM.Seq_N_T'First;
	-- + Variable: EP_H_ACKer
	EP_H_ACKer: LLU.End_Point_Type;
	-- + Variable: Número de secuencia recibido
	Seq_N_Receive: CM.Seq_N_T := 0;
	-- + Procedimento para, una vez recibido un ACK, borrar el mensaje asentido de
	-- 	 Pending_Msgs:
	--	  1. Dar valor a una clave de Pending_Msgs, formada por
	--			 Client_EP_Handler, EP_H_ACKer y número de secuencia recibido
	--	  2. Eliminar la clave de Pending_Msgs

	function "<" (Left, Right : ART.Time) return Boolean is
	begin
		if Left < Right then
			return True;
		else
			return False;
		end if;
	end "<";

	procedure Handler_C (From: in LLU.End_Point_Type;
								To: in LLU.End_Point_Type;
								P_Buffer: access LLU.Buffer_Type) is
		Mess_Type: CM.Message_Type;
		Nick, Comentario: ASU.Unbounded_String;
	begin
		-- + Sacar la cabecera del mensaje
		Mess_Type := CM.Message_Type'Input(P_Buffer);
		-- + MENSAJE SERVER
		if (Mess_Type = CM.Server) then
		--	 1. Solo si el mensaje WELCOME ya ha sido recibido:
			if Wel_rec then
		--			1.1. Extraer todo el contenido del mensaje SERVER
				Server_EP_Handler := LLU.End_Point_Type'Input(P_Buffer);	-- Saco el EP del server
				Seq_N_Receive := CM.Seq_N_T'Input(P_Buffer);		-- n_seq recibido
				Nick := ASU.Unbounded_String'Input(P_Buffer);   -- Luego el siguiente campo a sacar es el nick
				Comentario := ASU.Unbounded_String'Input(P_Buffer);   -- Y por ultimo el comentario
		--			1.2. Si el número de secuencia recibido es <= que el número de secuencia
		--					 del servidor:
					if (Seq_N_Receive <= Seq_N_Server) then
		--					 1.2.1. Enviar un mensaje ACK al servidor formado por el
		--									Client_EP_Handler y número de secuencia recibido
						LLU.Reset(P_Buffer.all);	-- Reinicializo para enviar el mensaje ACK
						CM.Message_Type'Output(P_Buffer, CM.Ack);
						LLU.End_Point_Type'Output(P_Buffer, EP_H_ACKer);
						CM.Seq_N_T'Output(P_Buffer, Seq_N_Receive);
						LLU.Send(Server_EP_Handler, P_Buffer);
		--					 1.2.2. Si el número de secuencia recibido es igual al número de
		--									secuencia del servidor:
						if (Seq_N_Receive = Seq_N_Server) then
		--									1.2.2.1. Mostrar Nick: Comentario por pantalla
							T_IO.Put_Line(ASU.To_String(Nick) & " : " & ASU.To_String(Comentario));
							T_IO.Put(">>");
		--									1.2.2.2. Incrementar el número de secuencia del servidor
							Seq_N_Server := Seq_N_Server + 1;
						end if;
					end if;
			end if;
		-- + MENSAJE ACK
		elsif (Mess_Type = CM.Ack) then
		--	 1. Extraer todo el contenido del mensaje ACK
		LLU.Reset(P_Buffer.all);	-- Reinicializo para recibir el mensaje ACK
		Mess_Type := CM.Message_Type'Input(P_Buffer);
		EP_H_ACKer := LLU.End_Point_Type'Input(P_Buffer);
		Seq_N_Receive := CM.Seq_N_T'Input(P_Buffer);
		--	 2. Ejecutar a través de Protected_Ops el subprograma que
		--		  borra la información del ACK recibido
		end if;
	end Handler_C;

	procedure Timed_Retransmission is
		Current_Time: ART.Time;
		First_Time_Stablished: Boolean;
		First_Time: ART.Time;
		Finish: Boolean;
		Element: RTX.Retransmission_Times_Element_Type;
		Value: RTX.Pending_Msgs_Value_Type;
		Success: Boolean;
		Retransmission_Time: ART.Time;
		Buffer: aliased LLU.Buffer_Type(1024);
		Client_EP_Handler, Server_EP_Handler: LLU.End_Point_Type;
		Seq_N_To_Send: CM.Seq_N_T;
		Nick, Comentario: ASU.Unbounded_String;
	begin
		Current_Time := ART.Clock;		-- Hora actual
		First_Time_Stablished := False;
		Finish := False;

		while not Finish loop
			begin
				Element := RTX.RTP.Get_First(Retransmission_Times);	-- Extraigo el primer elemento de la estructura de rtx
				if Element.Time < Current_Time then		-- Si la hora de rtx es menor que la hora actual
					RTX.RTP.Delete_First(Retransmission_Times);	-- Borro el elemento de la estructura
					RTX.PMP.Get(Pending_Msgs, Element.Pending_Msgs_Key, Value, Success);	-- Miro a ver si estaba en la estructura de mensajes pendientes de Ack
					if Success then	-- si estaba
						if Value.Retransmission_Attempts < Max_Retransmission_Attempts then	-- solo si su numero de rtxes es menor que el maximo permitido
							case Value.Header_Msg is
								when CM.Writer =>	-- Si la cabecera es writer mando el writer
									LLU.Reset(Buffer);
									CM.Message_Type'Output(Buffer'Access, CM.Writer);  -- El tipo de mensaje es writer
									Client_EP_Handler := Element.Pending_Msgs_Key.Sender_EP;
									LLU.End_Point_Type'Output(Buffer'Access, Client_EP_Handler);   -- En este tipo de mensajes meto el EP donde quiero recibir
									Seq_N_To_Send := Element.Pending_Msgs_Key.Seq_N;
									CM.Seq_N_T'Output(Buffer'Access, Seq_N_To_Send);	-- Mando el numero de secuencia
									Nick := Value.Nick;
									ASU.Unbounded_String'Output(Buffer'Access, Nick);  -- El nick del cliente que escribe
									Comentario := Value.Comment;
									ASU.Unbounded_String'Output(Buffer'Access, Comentario);  -- Y el mensaje
									Server_EP_Handler := Element.Pending_Msgs_Key.Receiver_EP;
									LLU.Send(Server_EP_Handler, Buffer'Access);
									--CM.Send_Writer(Element.Pending_Msgs_Key.Receiver_EP,
									--	Element.Pending_Msgs_Key.Sender_EP,
									--	Element.Pending_Msgs_Key.Seq_N,
									--	Value.Nick,
									--	Value.Comment);
								when CM.Logout => -- Si la cabecera es logout mando el logout
									LLU.Reset(Buffer);
									CM.Message_Type'Output(Buffer'Access, CM.Logout);  -- El tipo de mensaje es Logout
									Client_EP_Handler := Element.Pending_Msgs_Key.Receiver_EP;
									LLU.End_Point_Type'Output(Buffer'Access, Client_EP_Handler);   -- En este tipo de mensajes meto el EP donde quiero recibir
									Seq_N_To_Send := Element.Pending_Msgs_Key.Seq_N;
									CM.Seq_N_T'Output(Buffer'Access, Seq_N_To_Send);	-- Mando el numero de secuencia
									Nick := Value.Nick;
									ASU.Unbounded_String'Output(Buffer'Access, Nick);  -- El nick del cliente que escribe
									Server_EP_Handler := Element.Pending_Msgs_Key.Sender_EP;
									LLU.Send(Server_EP_Handler, Buffer'Access);
									--CM.Send_Logout(Element.Pending_Msgs_Key.Receiver_EP,
									--	Element.Pending_Msgs_Key.Sender_EP,
									--	Element.Pending_Msgs_Key.Seq_N,
									--	Value.Nick);
								when others =>
									null;
							end case;
							Value.Retransmission_Attempts := Value.Retransmission_Attempts + 1;	-- El numero de intentos de rtx aumenta
							RTX.PMP.Put(Pending_Msgs, Element.Pending_Msgs_Key, Value);		-- Y lo añadimos a la lista de mensajes pendientes
							Retransmission_Time := ART.Clock + ART.To_Time_Span(Retransmission_Period);

							if not First_Time_Stablished then
								First_Time := Retransmission_Time;
							end if;
							Element.Time := Retransmission_Time;

							-- Element := Retransmissions.Build_Retransmissions_Times_Element(Retransmission_Time,
							--	Element.Pending_Msgs_Key);
							RTX.RTP.Add(Retransmission_Times, Element);
						else
							RTX.PMP.Delete(Pending_Msgs, Element.Pending_Msgs_Key, Success);
						end if;
					end if;
				else
						Finish := True;
				end if;

			exception
				when RTX.RTP.Empty_List =>
					Finish := True;
			end;
		end loop;

		if RTX.PMP.Map_Length(Pending_Msgs) /= 0 then
			PO.Program_Timer_Procedure(Timed_Retransmission'Access, First_Time);
		end if;

	end Timed_Retransmission;

	procedure Manage_Retransmission is
		Retransmission_Time: ART.Time;
		Key: RTX.Pending_Msgs_Key_Type;
		Value: RTX.Pending_Msgs_Value_Type;
		Element: RTX.Retransmission_Times_Element_Type;
		Success: Boolean := False;
		--Client_EP_Handler: LLU.End_Point_Type;
		Nick, Comentario: ASU.Unbounded_String;
	begin
		-- PENDING MSGS
		--RTX.PMP.Get(Pending_Msgs, Element.Pending_Msgs_Key, Value, Success);
		-- + Dar valor a una clave de Pending_Msgs, formada por
		--	 Client_EP_Handler, Server_EP_Handler y número de secuencia recibido
		--Key.Sender_EP := Client_EP_Handler;
		Key.Receiver_EP := Server_EP_Handler;
		Key.Seq_N := Seq_N_Receive;
		-- + Dar valor a un valor de Pending_Msgs, formado por
		--	 cabecera del mensaje, nick del cliente, comentario y
		--   número de intentos a 1
		Value.Header_Msg := CM.Ack;
		Value.Nick := Nick;
		Value.Comment := Comentario;
		Value.Retransmission_Attempts := 1;
		-- + Insertar clave y valor en Pending_Msgs (Put)
		RTX.PMP.Put(Pending_Msgs, Key, Value);

		-- RETRANSMISSION TIMES
		-- + Establecer la hora de la retransmisión como
		--	 ART.Clock + ART.To_Time_Span(Retransmission_Period);
		Retransmission_Time := ART.Clock + ART.To_Time_Span(Retransmission_Period);
		-- + Dar valor a un elemento de Retransmission_Times, formado por
		--	 hora del mensaje y clave de Pending_Msgs
		Element.Time := Retransmission_Time;
		Element.Pending_Msgs_Key := Key;
		-- + Añadir el elemento a Retransmission_Times
		RTX.RTP.Add(Retransmission_Times, Element);

		-- + Programar el subprograma encargado de las retransmisiones para la misma hora
		-- 	 que la utilizada en Retransmission_Times
		PO.Program_Timer_Procedure(Timed_Retransmission'Access, Retransmission_Time);

	end Manage_Retransmission;
end Client_Handler;

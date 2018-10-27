
package body Server_Handler is

	-- Estructuras de datos necesarias
	Active_Clients: ACP.Map;	-- Clientes activos con tabla hash
	Inactive_Clients: ICP.Map;	-- Clientes inactivos con array ABB
	Pending_Msgs: RTX.PMP.Map;
	Retransmission_Times: RTX.RTP.List_Type;

	function "<" (Left, Right : ART.Time) return Boolean is
	begin
		if Left < Right then
			return True;
		else
			return False;
		end if;
	end "<";

	function Time_Image (T: Ada.Calendar.Time) return String is
	begin
		return Gnat.Calendar.Time_IO.Image(T, "%d-%b-%y %T.%i");
	end Time_Image;

	-- Show Clients
	function Parse_EP(EP: in LLU.End_Point_Type) return String is
		EP_Image: ASU.Unbounded_String := ASU.To_Unbounded_String(LLU.Image(EP));
		Position: Integer;
		IP: ASU.Unbounded_String;
		Port: Integer;
	begin
		Position := ASU.Index(EP_Image, ":");
		ASU.Tail(EP_Image, ASU.Length(EP_Image) - (Position + 1));

		Position := ASU.Index(EP_Image, ",");
		IP := ASU.Head(EP_Image, Position - 1);
		ASU.Tail(EP_Image, ASU.Length(EP_Image) - (Position + 1));

		Position := ASU.Index(EP_Image, ":");
		Port := Integer'Value(ASU.To_String(ASU.Tail(EP_Image, ASU.Length(EP_Image) - (Position + 1))));

		return "(" & ASU.To_String(IP) & ":" & Integer'Image(Port) & ")";
	end Parse_EP;

	procedure Show_Active_Clients is
		Cursor_Active_Clients: ACP.Cursor := ACP.First(Active_Clients);
		Active_Client: ACP.Element_Type;
	begin
		T_IO.Put_Line("ACTIVE CLIENTS");
		T_IO.Put_Line("==============");

		while ACP.Has_Element(Cursor_Active_Clients) loop
			Active_Client := ACP.Element(Cursor_Active_Clients);

			T_IO.Put_Line(ASU.To_String(Active_Client.Key) &
				" " &
				Parse_EP(Active_Client.Value.Client_EP_Handler) &
				": " &
				Time_Image(Active_Client.Value.Last_Connection));

			ACP.Next(Cursor_Active_Clients);
		end loop;
		T_IO.New_Line;
	end Show_Active_Clients;

	procedure Show_Inactive_Clients is
		Cursor_Inactive_Clients: ICP.Cursor := ICP.First(Inactive_Clients);
		Inactive_Client: ICP.Element_Type;
	begin
		T_IO.Put_Line("OLD CLIENTS");
		T_IO.Put_Line("==============");

		while ICP.Has_Element(Cursor_Inactive_Clients) loop
			Inactive_Client := ICP.Element(Cursor_Inactive_Clients);

			T_IO.Put_Line(ASU.To_String(Inactive_Client.Key) &
				": " &
				Time_Image(Inactive_Client.Value));

			ICP.Next(Cursor_Inactive_Clients);
		end loop;
		T_IO.New_Line;
	end Show_Inactive_Clients;
	--

  function Unbounded_String_Hash(US: ASU.Unbounded_String) return Hash_Range is
    Total: Natural;
    C: Character;
  begin
    Total := 0;
    for I in 1..ASU.Length(US) loop
      C := ASU.Element(US, I);
      Total := Total + Character'Pos(C);
    end loop;
    return Hash_Range'Mod(Total);
  end Unbounded_String_Hash;

	-- Retransmissions
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
		Current_Time := ART.Clock;

		First_Time_Stablished := False;

		Finish := False;
		while not Finish loop
			begin
				Element := RTX.RTP.Get_First(Retransmission_Times);
				if Element.Time < Current_Time then
					RTX.RTP.Delete_First(Retransmission_Times);
					RTX.PMP.Get(Pending_Msgs, Element.Pending_Msgs_Key, Value, Success);
					if Success then
						if Value.Retransmission_Attempts < Max_Retransmission_Attempts then
							case Value.Header_Msg is
								when CM.Server =>
								LLU.Reset(Buffer);   -- Reseteamos para mandar el Server
               			CM.Message_Type'Output(Buffer'Access, CM.Server);
								Server_EP_Handler := Element.Pending_Msgs_Key.Sender_EP;
								LLU.End_Point_Type'Output(Buffer'Access, Server_EP_Handler);   -- En este tipo de mensajes meto el EP donde quiero recibir
								Seq_N_To_Send := Element.Pending_Msgs_Key.Seq_N;
								CM.Seq_N_T'Output(Buffer'Access, Seq_N_To_Send);	-- Mando el numero de secuencia
								Nick := Value.Nick;
								ASU.Unbounded_String'Output(Buffer'Access, Nick);  -- El nick del cliente que escribe
								Comentario := Value.Comment;
								ASU.Unbounded_String'Output(Buffer'Access, Comentario);  -- Y el mensaje
								Client_EP_Handler := Element.Pending_Msgs_Key.Receiver_EP;
								LLU.Send(Client_EP_Handler, Buffer'Access);
									--CM.Send_Server(Element.Pending_Msgs_Key.Receiver_EP,
									--	Element.Pending_Msgs_Key.Sender_EP,
									--	Element.Pending_Msgs_Key.Seq_N,
									--	Value.Nick,
									--	Value.Comment);
								when others =>
									null;
							end case;
							Value.Retransmission_Attempts := Value.Retransmission_Attempts + 1;
							RTX.PMP.Put(Pending_Msgs, Element.Pending_Msgs_Key, Value);
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

	Client_EP_Handler_To_Send: LLU.End_Point_Type;
	Server_Seq_N: CM.Seq_N_T;
	Nick_To_Send: ASU.Unbounded_String;
	Comment_To_Send: ASU.Unbounded_String;
	procedure Manage_Retransmission is
		Key: RTX.Pending_Msgs_Key_Type;
		Value: RTX.Pending_Msgs_Value_Type;
		Retransmission_Time: ART.Time;
		Element: RTX.Retransmission_Times_Element_Type;
	begin
		-- PENDING MSGS
		-- + Dar valor a una clave de Pending_Msgs, formada por
		--	 Server_EP_Handler, Client_EP_Handler_To_Send y Server_Seq_N
		--Key.Sender_EP := Server_EP_Handler;
		Key.Receiver_EP := Client_EP_Handler_To_Send;
		Key.Seq_N := Server_Seq_N;
		-- + Dar valor a un valor de Pending_Msgs, formado por
		--	 CM.Server, Nick_To_Send y Comment_To_Send
		Value.Header_Msg := CM.Server;
		Value.Nick := Nick_To_Send;
		Value.Comment := Comment_To_Send;
		Value.Retransmission_Attempts := 1;
		-- + Insertar clave y valor en Pending_Msgs
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
	--

	-- Protection
	-- Active Clients
	Active_Client_Nick_To_Put: ASU.Unbounded_String;
	Active_Client_Value_To_Put: Active_Client_Value_Type;
	procedure Active_Clients_Do_Put is
	begin
		ACP.Put(Active_Clients, Active_Client_Nick_To_Put, Active_Client_Value_To_Put);
	end Active_Clients_Do_Put;

	Active_Client_Nick_To_Delete: ASU.Unbounded_String;
	procedure Active_Clients_Do_Delete is
		Success: Boolean;
	begin
		ACP.Delete(Active_Clients, Active_Client_Nick_To_Delete, Success); --
	end Active_Clients_Do_Delete;
	--

	-- Inactive Clients
	Inactive_Client_Nick_To_Put: ASU.Unbounded_String;
	procedure Inactive_Clients_Do_Put is
	begin
		ICP.Put(Inactive_Clients, Inactive_Client_Nick_To_Put, Ada.Calendar.Clock); --
	end Inactive_Clients_Do_Put;
	--

	procedure Send_To_All(Active_Clients: in out ACP.Map; Nick: ASU.Unbounded_String; Comment: ASU.Unbounded_String; No_Send_Nick: ASU.Unbounded_String := ASU.To_Unbounded_String("")) is
		Is_Any_Msg_Sent: Boolean;
		Cursor_Active_Clients: ACP.Cursor := ACP.First(Active_Clients);
		Active_Client: ACP.Element_Type;
		Buffer: aliased LLU.Buffer_Type(1024);
		--Client_EP_Handler, Server_EP_Handler: LLU.End_Point_Type;
		Client_EP_Handler: LLU.End_Point_Type;
		Seq_N_To_Send: CM.Seq_N_T;
	begin
		Is_Any_Msg_Sent := False;
		while ACP.Has_Element(Cursor_Active_Clients) loop
			Active_Client := ACP.Element(Cursor_Active_Clients);
			if ASU.To_String(Active_Client.Key) /= ASU.To_String(No_Send_Nick) then
				LLU.Reset(Buffer);   -- Reseteamos para mandar el Server
				CM.Message_Type'Output(Buffer'Access, CM.Server);
				--LLU.End_Point_Type'Output(Buffer'Access, Server_EP_Handler);   -- En este tipo de mensajes meto el EP donde quiero recibir
				Seq_N_To_Send := Active_Client.Value.Server_Seq_N;
				CM.Seq_N_T'Output(Buffer'Access, Seq_N_To_Send);	-- Mando el numero de secuencia
				ASU.Unbounded_String'Output(Buffer'Access, Nick);  -- El nick del cliente que escribe
				ASU.Unbounded_String'Output(Buffer'Access, Comment);  -- Y el mensaje
				Client_EP_Handler := Active_Client.Value.Client_EP_Handler;
				LLU.Send(Client_EP_Handler, Buffer'Access);
				--CM.Send_Server(Active_Client.Value.Client_EP_Handler, Server_EP_Handler, Active_Client.Value.Server_Seq_N, Nick, Comment);
				Server_Seq_N := Active_Client.Value.Server_Seq_N;
				--if Debug then
				--	T_IO.Put_Line("DB: SERVER (" & Utils.Parse_EP(Server_EP_Handler) &
				--		", " & CM.Seq_N_T'Image(Active_Client.Value.Server_Seq_N) &
				--		", " & ASU.To_String(Nick) &
				--		", " & ASU.To_String(Comment) & ") sent to & " & Utils.Parse_EP(Active_Client.Value.Client_EP_Handler));
				--end if;
				Active_Client.Value.Server_Seq_N := CM.Seq_N_T'Succ(Active_Client.Value.Server_Seq_N);

				Active_Client_Nick_To_Put := Active_Client.Key;
				Active_Client_Value_To_Put := Active_Client.Value;
				PO.Protected_Call(Active_Clients_Do_Put'Access);

				Is_Any_Msg_Sent := True;
				Client_EP_Handler_To_Send := Active_Client.Value.Client_EP_Handler;
				Nick_To_Send := Nick;
				Comment_To_Send := Comment;
				PO.Protected_Call(Manage_Retransmission'Access);
			end if;
			ACP.Next(Cursor_Active_Clients);
		end loop;
	end Send_To_All;

	function Old_Client_To_Bang(Active_Clients: in ACP.Map) return ACP.Element_Type is
		Cursor_Active_Clients: ACP.Cursor := ACP.First(Active_Clients);
		Old_Client: ACP.Element_Type;
		Current_Client: ACP.Element_Type;
	begin
		Old_Client := ACP.Element(Cursor_Active_Clients);

		ACP.Next(Cursor_Active_Clients);
		while ACP.Has_Element(Cursor_Active_Clients) loop
			Current_Client:= ACP.Element(Cursor_Active_Clients);
			if not (Current_Client.Value.Last_Connection > Old_Client.Value.Last_Connection) then
				Old_Client := Current_Client;
			end if;

			ACP.Next(Cursor_Active_Clients);
		end loop;
		return Old_Client;
	end Old_Client_To_Bang;

	-- + Variable: EP_H_ACKer
	EP_H_ACKer: LLU.End_Point_Type;
	-- + Variable: Número de secuencia recibido
	Seq_N_Receive: CM.Seq_N_T;

	Server_Nick: ASU.Unbounded_String := ASU.To_Unbounded_String("server");


	procedure Handler_S (From: in LLU.End_Point_Type;
								To: in LLU.End_Point_Type;
								P_Buffer: access LLU.Buffer_Type) is
		Header_Msg: CM.Message_Type;
		Client_EP_Receive: LLU.End_Point_Type;
		Client_EP_Handler: LLU.End_Point_Type;
		Nick: ASU.Unbounded_String;
		Active_Client_Value: Active_Client_Value_Type;
		Accepted: Boolean;
		Old_Client: ACP.Element_Type;
		Seq_N: CM.Seq_N_T;
		Comment: ASU.Unbounded_String;
		Success: Boolean;
		Test: ASU.Unbounded_String;
		Mess_Type: CM.Message_Type;
		ACK_Seq_N: CM.Seq_N_T;
	begin
		Header_Msg := CM.Message_Type'Input(P_Buffer);
		case Header_Msg is
			when CM.Init =>
				-- + Extraer todo el contenido del mensaje INIT
				LLU.Reset(P_Buffer.all);
			   Client_EP_Receive := LLU.End_Point_Type'Input(P_Buffer);
			   Client_EP_Handler := LLU.End_Point_Type'Input(P_Buffer);
			   Nick := ASU.Unbounded_String'Input(P_Buffer);

				ACP.Get(Active_Clients, Nick, Active_Client_Value, Success);
				if Success then
					if Client_EP_Handler = Active_Client_Value.Client_EP_Handler then
						Accepted := True;
						LLU.Reset(P_Buffer.all);
						CM.Message_Type'Output(P_Buffer, CM.Logout);
						LLU.End_Point_Type'Output(P_Buffer, Client_EP_Receive);
						Boolean'Output(P_Buffer, Accepted);
						--CM.Send_Welcome(Client_EP_Receive, Accepted);
					else
						T_IO.Put_Line("IGNORED, nick already used");

						Accepted := False;
						LLU.Reset(P_Buffer.all);
						CM.Message_Type'Output(P_Buffer, CM.Logout);
						LLU.End_Point_Type'Output(P_Buffer, Client_EP_Receive);
						Boolean'Output(P_Buffer, Accepted);
						--CM.Send_Welcome(Client_EP_Receive, Accepted);
					end if;
				else
					T_IO.Put_Line("INIT received from " & ASU.To_String(Nick) & ": ACCEPTED");

					Active_Client_Nick_To_Put := Nick;
					Active_Client_Value_To_Put.Client_EP_Handler := Client_EP_Handler;
					Active_Client_Value_To_Put.Client_Seq_N := CM.Seq_N_T'First;
					Active_Client_Value_To_Put.Last_Connection := Ada.Calendar.Clock;
					Active_Client_Value_To_Put.Server_Seq_N := CM.Seq_N_T'First;
					begin
						PO.Protected_Call(Active_Clients_Do_Put'Access);
					exception
						when ACP.Full_Map =>
							Old_Client := Old_Client_To_Bang(Active_Clients);
							Send_To_All(Active_Clients, Server_Nick, ASU.To_Unbounded_String(ASU.To_String(Old_Client.Key) & " banned for being idle too long"));

							Active_Client_Nick_To_Delete := Old_Client.Key;
							PO.Protected_Call(Active_Clients_Do_Delete'Access);

							Inactive_Client_Nick_To_Put := Old_Client.Key;
							begin
								PO.Protected_Call(Inactive_Clients_Do_Put'Access);
							exception
								when ICP.Full_Map =>
									null;
							end;
							Active_Client_Nick_To_Put := Nick;
							Active_Client_Value_To_Put.Client_EP_Handler := Client_EP_Handler;
							Active_Client_Value_To_Put.Client_Seq_N := CM.Seq_N_T'First;
							Active_Client_Value_To_Put.Last_Connection := Ada.Calendar.Clock;
							Active_Client_Value_To_Put.Server_Seq_N := CM.Seq_N_T'First;
							PO.Protected_Call(Active_Clients_Do_Put'Access);
					end;

					Accepted := True;
					LLU.Reset(P_Buffer.all);
					CM.Message_Type'Output(P_Buffer, CM.Logout);
					LLU.End_Point_Type'Output(P_Buffer, Client_EP_Receive);
					Boolean'Output(P_Buffer, Accepted);
					Send_To_All(Active_Clients, Server_Nick, ASU.To_Unbounded_String(ASU.To_String(Nick) & " joins the chat"), Nick);
				end if;
			when CM.Writer =>
				--CM.Extract_Writer(P_Buffer, Client_EP_Handler, Seq_N, Nick, Comment);
				LLU.Reset(P_Buffer.all);
				Mess_Type := CM.Message_Type'Input(P_Buffer);
				Client_EP_Handler := LLU.End_Point_Type'Input(P_Buffer);
				Seq_N := CM.Seq_N_T'Input(P_Buffer);
				Nick := ASU.Unbounded_String'Input(P_Buffer);
				Comment := ASU.Unbounded_String'Input(P_Buffer);
				LLU.Send(Client_EP_Handler, P_Buffer);

				ACP.Get(Active_Clients, Nick, Active_Client_Value, Success);
				if Success then
					if Client_EP_Handler = Active_Client_Value.Client_EP_Handler then
						if Seq_N <= Active_Client_Value.Client_Seq_N then
							--CM.Send_Ack(Active_Client_Value.Client_EP_Handler, Server_EP_Handler, Seq_N);
							LLU.Reset(P_Buffer.all);
							Client_EP_Handler := Active_Client_Value.Client_EP_Handler;
							LLU.End_Point_Type'Output(P_Buffer, Client_EP_Handler);
							--LLU.End_Point_Type'Output(P_Buffer, Server_EP_Handler);
							CM.Seq_N_T'Output(P_Buffer, Seq_N);


							if Seq_N = Active_Client_Value.Client_Seq_N then
								T_IO.Put_Line("WRITER received from " & ASU.To_String(Nick) & ": " & ASU.To_String(Comment));

								Active_Client_Nick_To_Put := Nick;
								Active_Client_Value_To_Put.Client_EP_Handler := Active_Client_Value.Client_EP_Handler;
								Active_Client_Value_To_Put.Client_Seq_N := CM.Seq_N_T'Succ(Active_Client_Value.Client_Seq_N);
								Active_Client_Value_To_Put.Last_Connection := Ada.Calendar.Clock;
								Active_Client_Value_To_Put.Server_Seq_N := Active_Client_Value.Server_Seq_N;
								PO.Protected_Call(Active_Clients_Do_Put'Access);

								Send_To_All(Active_Clients, Server_Nick, Comment, Nick);
							end if;
						end if;
					else
						T_IO.Put_Line("WRITER received from unknown client. IGNORED");
					end if;
				else
					T_IO.Put_Line("WRITER received from unknown client. IGNORED");
				end if;
			when CM.Logout =>
				--CM.Extract_Logout(P_Buffer, Client_EP_Handler, Seq_N, Nick);
				LLU.Reset(P_Buffer.all);
				Mess_Type := CM.Message_Type'Input(P_Buffer);
				Client_EP_Handler := LLU.End_Point_Type'Input(P_Buffer);
				Seq_N := CM.Seq_N_T'Input(P_Buffer);
				Nick := ASU.Unbounded_String'Input(P_Buffer);

				ACP.Get(Active_Clients, Nick, Active_Client_Value, Success);
				if Success then
					if Client_EP_Handler = Active_Client_Value.Client_EP_Handler then
						if Seq_N <= Active_Client_Value.Client_Seq_N then
							--CM.Send_Ack(Active_Client_Value.Client_EP_Handler, Server_EP_Handler, Seq_N);
							LLU.Reset(P_Buffer.all);
							Client_EP_Handler := Active_Client_Value.Client_EP_Handler;
							LLU.End_Point_Type'Output(P_Buffer, Client_EP_Handler);
							--LLU.End_Point_Type'Output(P_Buffer, Server_EP_Handler);
							CM.Seq_N_T'Output(P_Buffer, Seq_N);
							if Seq_N = Active_Client_Value.Client_Seq_N then
								T_IO.Put_Line("LOGOUT received from " & ASU.To_String(Nick));

								Active_Client_Nick_To_Delete := Nick;
								PO.Protected_Call(Active_Clients_Do_Delete'Access);
								Inactive_Client_Nick_To_Put := Nick;
								PO.Protected_Call(Inactive_Clients_Do_Put'Access);

								Send_To_All(Active_Clients, Server_Nick, ASU.To_Unbounded_String(ASU.To_String(Nick) & " leaves the chat"), Nick);
							end if;
						end if;
					else
						T_IO.Put_Line("LOGOUT received from unknown client. IGNORED");
					end if;
				else
					null;
				end if;
			when CM.Ack =>
				--CM.Extract_Ack(P_Buffer, EP_H_ACKer, Seq_N);
				LLU.Reset(P_Buffer.all);
				Mess_Type := CM.Message_Type'Input(P_Buffer);
				EP_H_ACKer := LLU.End_Point_Type'Input(P_Buffer);
				Seq_N := CM.Seq_N_T'Input(P_Buffer);

				ACK_Seq_N := Seq_N;
				--PO.Protected_Call(Check_Ack'Access);
			when others =>
				null;
		end case;
	end Handler_S;

end Server_Handler;

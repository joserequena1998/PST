with Chat_Messages;
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Lower_Layer_UDP;
with Ada.Command_Line;
with Ada.Exceptions;
with Client_Handler;    -- Paquete donde hago todo el trabajo basicamente


procedure Chat_Client_3 is
	package T_IO renames Ada.Text_IO;
   package ASU renames Ada.Strings.Unbounded;
   package LLU renames Lower_Layer_UDP;
   package CM renames Chat_Messages;
   package ACL renames Ada.Command_Line;
   package CH renames Client_Handler;

   use type Ada.Strings.Unbounded.Unbounded_String;
   use type Lower_Layer_UDP.End_Point_Type;
   use type Chat_Messages.Message_Type;
   -- Esto es necesario para poder comparar Unbounded_Strings, EPs y Chat_Messages

	Usage_Error: exception;
   Nick_Error: exception;
   Delay_Error: exception;
	Pct_Error: exception;
	Expired_Error: exception;

	Port: Integer;
	Nick, Mensaje: ASU.Unbounded_String;
	Min_Delay, Max_Delay, Fault_Pct : Natural;
	Server_EP, Client_EP_Receive, Client_EP_Handler: LLU.End_Point_Type;
	Expired: Boolean;
	Retrans_INIT: Natural := 0;
	Buffer: aliased LLU.Buffer_Type(1024);
	Mess_Type: CM.Message_Type;
	Acogido: Boolean;

begin
	-- Si el numero de argumentos es distinto de 6, Usage_Error
	if ACL.Argument_Count /= 6 then
      raise Usage_Error;
   end if;

	-- Extraigos los argunentos: hostname puerto nick mindelay maxdelay faultpct
	Port := Natural'Value(ACL.Argument(2));
   Server_EP := LLU.Build(LLU.To_IP(ACL.Argument(1)), Port);   -- Este es el EP del servidor, el Handler
   Nick := ASU.To_Unbounded_String(ACL.Argument(3));
	Min_Delay := Natural'Value(ACL.Argument(4));
	Max_Delay := Natural'Value(ACL.Argument(5));
	Fault_Pct := Natural'Value(ACL.Argument(6));

	-- Excepcion con el nick
	if Nick = "server" then
      raise Nick_Error;
   end if;

	-- Excepciones con los retardos
	if (Max_Delay < Min_Delay) then
		raise Delay_Error;
	end if;

	-- Excepcion con el porcentaje
	if (Fault_Pct < 0 or Fault_Pct > 100) then
		raise Pct_Error;
	end if;

	LLU.Set_Faults_Percent(Fault_Pct);
	LLU.Set_Random_Propagation_Delay(Min_Delay, Max_Delay);

	CH.Plazo_Retransmision := 2 * Duration(Max_Delay) / 1000;	-- Plazo de retrans
	CH.Max_Retransmission_Attempts := 10 + (Fault_Pct / 10) ** 2;	-- Maximas retrans

	LLU.Bind_Any(Client_EP_Receive);    -- En este EP debo recibir el Welcome
	LLU.Bind_Any(Client_EP_Handler, CH.Handler_C'Access);    -- En este EP debo recibir lo que sea

	-- Mando mensajes INIT hasta que reciba el WELCOME que lo asienta y me da respusta o alcanzo max retrax
	loop
		-- Primero debo mandar el mensaje INIT: Tipo mensaje, EP para Welcome, EP para recibir, Nick
	   LLU.Reset(Buffer);
	   CM.Message_Type'Output(Buffer'Access, CM.Init);
	   LLU.End_Point_Type'Output(Buffer'Access, Client_EP_Receive);
	   LLU.End_Point_Type'Output(Buffer'Access, Client_EP_Handler);
	   ASU.Unbounded_String'Output(Buffer'Access, Nick);
	   LLU.Send(Server_EP, Buffer'Access);

		Retrans_INIT := Retrans_INIT + 1;	-- Ya tengo una retrans mas

		-- Debo reinicializar el buffer para recibir el welcome en el
	   LLU.Reset(Buffer);
		LLU.Receive(Client_EP_Receive, Buffer'Access, 10.0, Expired);  -- Debo esperar 10" en el EP Receive
		exit when (not Expired or Retrans_INIT = CH.Max_Retransmission_Attempts);
	end loop;

	if Expired then   -- Si expira el tiempo es que el servidor no esta disponible
      raise Expired_Error;
   end if;

	-- Si no expira el tiempo es que hemos recibido el Welcome
	CH.Wel_rec := True;	-- Estrictamente necesario????? Si para ver luego como el ack del init
	Mess_Type := CM.Message_Type'Input(Buffer'Access);    -- Este tipo de mensaje sera Welcome
	Acogido := Boolean'Input(Buffer'Access);     -- Este campo me indica si soy recibido o no
	if Acogido then
		T_IO.Put_Line("Mini-Chat v3.0: Welcome " & ASU.To_String(Nick));
		-- Establezco el numero de secuencia al primer valor de Seq_N_T
		CH.Seq_N := CM.Seq_N_T'First;
		loop
			T_IO.Put(">>");
			Mensaje := ASU.To_Unbounded_String(T_IO.Get_Line);    -- Leo el mensaje con una llamada bloqueante

			if Mensaje /= ".quit" then
				-- Envio el WRITER
				LLU.Reset(Buffer);   -- Reinicializo para meter mi mensaje
				CM.Message_Type'Output(Buffer'Access, CM.Writer);  -- El tipo de mensaje es writer
				LLU.End_Point_Type'Output(Buffer'Access, Client_EP_Handler);   -- En este tipo de mensajes meto el EP donde quiero recibir
				CM.Seq_N_T'Output(Buffer'Access, CH.Seq_N);	-- Mando el numero de secuencia
				ASU.Unbounded_String'Output(Buffer'Access, Nick);  -- El nick del cliente que escribe
				ASU.Unbounded_String'Output(Buffer'Access, Mensaje);  -- Y el mensaje
				LLU.Send(Server_EP, Buffer'Access);

				CH.Timed_Retransmission;	-- subprograma que gestiona las retransmisiones

				CH.Seq_N := CM.Seq_N_T'Succ(CH.Seq_N);	-- Incremento el n_seq para diferenciar mensajes luego
			end if;
			exit when (Mensaje = ".quit");	-- Si el mensaje es quit nos salimos y mandamos el logout
		end loop;

		-- Mando el mensaje Logout
		LLU.Reset(Buffer);
		CM.Message_Type'Output(Buffer'Access, CM.Logout);
		LLU.End_Point_Type'Output(Buffer'Access, Client_EP_Handler);
		CM.Seq_N_T'Output(Buffer'Access, CH.Seq_N);	-- Mando el numero de secuencia
		ASU.Unbounded_String'Output(Buffer'Access, Nick);  -- El nick del cliente que escribe
		LLU.Send(Server_EP, Buffer'Access);

		CH.Timed_Retransmission;	-- subprograma que gestiona las retransmisiones

	else 	-- Si no estoy acogido:
		T_IO.Put_Line("Mini-Chat v3.0: IGNORED new user " & ASU.To_String(Nick) & ", nick already used");
		--LLU.Finalize;
	end if ;

	LLU.Finalize;

exception
      when Usage_Error =>
         T_IO.Put_Line("Uso: ./chat_client_3 <Server> <puerto> <nick> <Min_Delay> <Max_Delay> <Fault_Pct>");
         LLU.Finalize;
      when Nick_Error =>
         T_IO.Put_Line("El nick introducido no puede ser server");
         LLU.Finalize;
		when Delay_Error =>
			T_IO.Put_Line("Min_Delay debe ser menor que Max_Delay");
			LLU.Finalize;
		when Pct_Error =>
			T_IO.Put_Line("El porcentaje de errores debe ser: 0 < Pct < 100");
			LLU.Finalize;
		when Expired_Error =>
			T_IO.Put_Line("Server unreachable");
			LLU.Finalize;
      when Except:others =>
         T_IO.Put_Line ("Excepción imprevista: " &
                                 Ada.Exceptions.Exception_Name (Except) & " en: " &
                                 Ada.Exceptions.Exception_Message (Except));
         LLU.Finalize;

end Chat_Client_3;

with Ada.Text_IO;
with Ada.Command_Line;
with Lower_Layer_UDP;
with Ada.Strings.Unbounded;
with Ada.Exceptions;
with Chat_Messages;
with Client_Collections;

procedure Chat_Admin is
   package T_IO renames Ada.Text_IO;
   package CC renames Client_Collections;
   package CM renames Chat_Messages;
   package ASU renames Ada.Strings.Unbounded;
   package LLU renames Lower_Layer_UDP;
   package ACL renames Ada.Command_Line;

   use type ASU.Unbounded_String;
   use type LLU.End_Point_Type;
   use type CM.Message_Type;
   -- Esto es necesario para poder comparar Unbounded_Strings, EPs y Chat_Messages

   Usage_Error: exception;

   Buffer: aliased LLU.Buffer_Type(1024);
   Port: Natural;
   Clients: CC.Collection_Type;
   Server_EP, Admin_EP: LLU.End_Point_Type;
   Nick, Mensaje, Password, Lista_Clientes: ASU.Unbounded_String;
   Expired: Boolean := False;
   Mess_Type: CM.Message_Type;
   Opcion: Integer := 0;

begin
   if ACL.Argument_Count /= 3 then
      raise Usage_Error;
   end if;

   Port := Natural'Value(ACL.Argument(2));
   Server_EP := LLU.Build(LLU.To_IP(ACL.Argument(1)), Port);   -- Este es el EP del servidor
   Password := ASU.To_Unbounded_String(ACL.Argument(3));

   LLU.Bind_Any(Admin_EP);    -- El cliente se ata a cualquier EP

   LLU.Reset(Buffer);

   while not Expired and Opcion /= 4 loop     -- Mientras la opcion no sea 4 vamos pidiendo acciones al admin
		T_IO.New_Line;
		T_IO.Put_Line("Options");
		T_IO.Put_Line("1 Show writers collection");
		T_IO.Put_Line("2 Ban writer");
		T_IO.Put_Line("3 Shutdown server");
		T_IO.Put_Line("4 Quit");
		T_IO.New_Line;
		T_IO.Put("Your option? ");
		Opcion := Integer'Value(T_IO.Get_Line);
		case Opcion is
			when 1 =>      -- Si la opcion es uno debemos pedir la lista de clientes
				CM.Message_Type'Output(Buffer'Access, CM.Collection_Request);
				LLU.End_Point_Type'Output(Buffer'Access, Admin_EP);
				ASU.Unbounded_String'Output(Buffer'Access, Password);
				LLU.Send(Server_EP, Buffer'Access);   -- Mandamos el Collection_Request y esperamos 5 segundos

				LLU.Reset(Buffer);  -- Reinicializo el buffer porque estaba lleno y ahora quiero meter cosas
				LLU.Receive(Admin_EP, Buffer'Access, 5.0, Expired);  -- Esperamos 5 segundos
				if not Expired then    -- Si no expira el tiempo es que la contraseña es correcta y nos responde
					Mess_Type:= CM.Message_Type'Input(Buffer'Access);   -- El tipo de mensaje sera collection_data
					Lista_Clientes := ASU.Unbounded_String'Input(Buffer'Access); -- Devuelve todo como Unbounded_String
					T_IO.Put_Line(ASU.To_String(Lista_Clientes));    -- Necesito pasarlo a string
				else
					T_IO.Put("Incorrect password");   -- Si expira el tiempo es que el password es falso y suspendo ejecucion
					LLU.Finalize;
				end if;
			when 2 =>      -- Si la opcion es dos debemos pedir el nick del cliente a banear
				T_IO.Put_Line("Nick to ban?");
				Nick := ASU.To_Unbounded_String(T_IO.Get_Line);

				LLU.Reset(Buffer);
				CM.Message_Type'Output(Buffer'Access, CM.Ban);
				ASU.Unbounded_String'Output(Buffer'Access, Password);
				ASU.Unbounded_String'Output(Buffer'Access, Nick);
				LLU.Send(Server_EP, Buffer'Access);
			when 3 =>      -- Si la opcion es 3 debo decirle al servidor que acabe
				LLU.Reset(Buffer);
				CM.Message_Type'Output(Buffer'Access, CM.Shutdown);
				ASU.Unbounded_String'Output(Buffer'Access, Password);
				LLU.Send(Server_EP, Buffer'Access);

				T_IO.Put_Line("Server shutdown sent");
         when 4 =>
            T_IO.Put_Line("Acabo mi ejecucion");
            LLU.Finalize;
			when others =>
				T_IO.Put_Line("Option must be 1-4");
		end case;
	end loop;

exception
   when Usage_Error =>
      T_IO.Put_Line("Uso: ./chat_admin <Server> <Port> <Password>");
      LLU.Finalize;


end Chat_Admin;

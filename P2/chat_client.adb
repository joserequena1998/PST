with Chat_Messages;
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Lower_Layer_UDP;
with Ada.Command_Line;
with Ada.Exceptions;

procedure Chat_Client is
   package T_IO renames Ada.Text_IO;
   package ASU renames Ada.Strings.Unbounded;
   package LLU renames Lower_Layer_UDP;
   package CM renames Chat_Messages;
   package ACL renames Ada.Command_Line;

   use type Ada.Strings.Unbounded.Unbounded_String;
   use type Lower_Layer_UDP.End_Point_Type;
   use type Chat_Messages.Message_Type;
   -- Esto es necesario para poder comparar Unbounded_Strings, EPs y Chat_Messages

   Usage_Error: exception;

   Buffer: aliased LLU.Buffer_Type(1024);
   Port: Natural;
   EP, Server_EP: LLU.End_Point_Type;
   Nick, Mensaje: ASU.Unbounded_String;
   Expired: Boolean;
   Mess_Type: CM.Message_Type;

begin
   if ACL.Argument_Count /= 3 then
      raise Usage_Error;
   end if;

   Port := Natural'Value(ACL.Argument(2));
   Server_EP := LLU.Build(LLU.To_IP(ACL.Argument(1)), Port);   -- Este es el EP del servidor
   Nick := ASU.To_Unbounded_String(ACL.Argument(3));

   LLU.Bind_Any(EP);    -- El cliente se ata a cualquier EP

   -- Primero debo mandar el mensaje INIT
   LLU.Reset(Buffer);
   CM.Message_Type'Output(Buffer'Access, CM.Init);
   LLU.End_Point_Type'Output(Buffer'Access, EP);
   ASU.Unbounded_String'Output(Buffer'Access, Nick);
   LLU.Send(Server_EP, Buffer'Access);

   --T_IO.Put_Line("Pasa");

   if Nick = "reader" then    -- Si el nick es reader solo me limito a esperar mensajes e imprimir en pantalla
      loop
         LLU.Reset(Buffer);   -- Vacio el buffer para recibir ahi
         LLU.Receive(EP, Buffer'Access, 36000.0, Expired);      -- Me quedo esperando a recibir algo
         if not Expired then
            Mess_Type := CM.Message_Type'Input(Buffer'Access); -- Saco el tipo de mensaje
            if Mess_Type = CM.Server then
               Nick := ASU.Unbounded_String'Input(Buffer'Access); -- Saco el nick del que escribio el mensaje
      			Mensaje := ASU.Unbounded_String'Input(Buffer'Access); -- Saco el mansaje que mando
      			T_IO.Put_Line(ASU.To_String(Nick) & ": " & ASU.To_String(Mensaje));
            end if;
         end if;
      end loop;

   else
      T_IO.Put("Message: ");
      Mensaje := ASU.To_Unbounded_String(T_IO.Get_Line);    -- Leo el mensaje con una llamada bloqueante

      while Mensaje /= ".quit" loop    -- Mientras no ponga .quit voy leyendo mensajes
         LLU.Reset(Buffer);   -- Reinicializo para meter mi mensaje
         CM.Message_Type'Output(Buffer'Access, CM.Writer);  -- El tipo de mensaje es writer
         LLU.End_Point_Type'Output(Buffer'Access, EP);   -- En este tipo de mensajes solo meto el EP
         ASU.Unbounded_String'Output(Buffer'Access, Mensaje);  -- Y el mensaje, el servidor luego buscará el nick por mi EP
         LLU.Send(Server_EP, Buffer'Access);

         T_IO.Put("Message: ");
         Mensaje := ASU.To_Unbounded_String(T_IO.Get_Line);
      end loop;

      if (Mensaje = ".quit") then 	-- Si el comentario es .quit debo mandar el logout y salirme
			LLU.Reset(Buffer);
			CM.Message_Type'Output(Buffer'Access, CM.Logout);
			LLU.End_Point_Type'Output(Buffer'Access, EP);
			LLU.Send(Server_EP, Buffer'Access);
		end if;

   end if;

   LLU.Finalize;

exception
      when Usage_Error =>
         LLU.Finalize;
         T_IO.Put_Line("Uso: ./chat_client <Server> <puerto> <nick>");
      when others =>
         T_IO.Put_Line("Error inesperado");
         LLU.Finalize;

end Chat_Client;

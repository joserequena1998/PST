with Chat_Messages;
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Lower_Layer_UDP;
with Ada.Command_Line;
with Ada.Exceptions;
with Client_Handler;    -- Paquete donde trato los mensajes que me llegan

procedure Chat_Client2 is
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
   Expired_Error: exception;

   Port: Natural;
   Server_EP, Client_EP_Receive, Client_EP_Handler: LLU.End_Point_Type;
   Nick, Mensaje: ASU.Unbounded_String;
   Buffer: aliased LLU.Buffer_Type(1024);
   Expired: Boolean;
   Mess_Type: CM.Message_Type;
   Acogido: Boolean;

begin
   if ACL.Argument_Count /= 3 then
      raise Usage_Error;
   end if;

   Port := Natural'Value(ACL.Argument(2));
   Server_EP := LLU.Build(LLU.To_IP(ACL.Argument(1)), Port);   -- Este es el EP del servidor
   Nick := ASU.To_Unbounded_String(ACL.Argument(3));

   if Nick = "server" then
      raise Nick_Error;
   end if;

   LLU.Bind_Any(Client_EP_Receive);    -- En este EP debo recibir el Welcome
   LLU.Bind_Any(Client_EP_Handler, CH.Handler_C'Access);    -- En este EP debo recibir lo que sea

   -- Primero debo mandar el mensaje INIT: Tipo mensaje, EP para Welcome, EP para recibir, Nick
   LLU.Reset(Buffer);
   CM.Message_Type'Output(Buffer'Access, CM.Init);
   LLU.End_Point_Type'Output(Buffer'Access, Client_EP_Receive);
   LLU.End_Point_Type'Output(Buffer'Access, Client_EP_Handler);
   ASU.Unbounded_String'Output(Buffer'Access, Nick);
   LLU.Send(Server_EP, Buffer'Access);

   -- Debo reinicializar el buffer para recibir el welcome en el
   LLU.Reset(Buffer);
   LLU.Receive(Client_EP_Receive, Buffer'Access, 10.0, Expired);  -- Debo esperar 10" en el EP Receive
   if Expired then   -- Si expira el tiempo es que el servidor no esta disponible
      raise Expired_Error;
   end if;

   -- Si no expira el tiempo es que hemos recibido el Welcome
   Mess_Type := CM.Message_Type'Input(Buffer'Access);    -- Este tipo de mensaje sera Welcome
   Acogido := Boolean'Input(Buffer'Access);     -- Este campo me indica si soy recibido o no
   if Acogido then   -- Si soy recibido pido cadenas de caracteres
      T_IO.Put_Line("Mini-Chat v2.0: Welcome " & ASU.To_String(Nick));
      T_IO.Put(">>");
      Mensaje := ASU.To_Unbounded_String(T_IO.Get_Line);    -- Leo el mensaje con una llamada bloqueante

      while Mensaje /= ".quit" loop    -- Mientras no ponga .quit voy leyendo mensajes
         LLU.Reset(Buffer);   -- Reinicializo para meter mi mensaje
         CM.Message_Type'Output(Buffer'Access, CM.Writer);  -- El tipo de mensaje es writer
         LLU.End_Point_Type'Output(Buffer'Access, Client_EP_Handler);   -- En este tipo de mensajes meto el EP donde quiero recibir
         ASU.Unbounded_String'Output(Buffer'Access, Nick);  -- El nick del cliente que escribe
         ASU.Unbounded_String'Output(Buffer'Access, Mensaje);  -- Y el mensaje
         LLU.Send(Server_EP, Buffer'Access);

         T_IO.Put(">>");
         Mensaje := ASU.To_Unbounded_String(T_IO.Get_Line);
      end loop;

      if (Mensaje = ".quit") then 	-- Si el comentario es .quit debo mandar el logout y salirme
			LLU.Reset(Buffer);
			CM.Message_Type'Output(Buffer'Access, CM.Logout);
			LLU.End_Point_Type'Output(Buffer'Access, Client_EP_Handler);
         ASU.Unbounded_String'Output(Buffer'Access, Nick);  -- El nick del cliente que escribe
			LLU.Send(Server_EP, Buffer'Access);
		end if;
   else  -- Si no soy recibido acabo la ejecucion
      T_IO.Put_Line("Mini-Chat v2.0: IGNORED new user " & ASU.To_String(Nick) & ", nick already used");
      LLU.Finalize;
   end if;

   LLU.Finalize;

exception
      when Usage_Error =>
         T_IO.Put_Line("Uso: ./chat_client2 <Server> <puerto> <nick>");
         LLU.Finalize;
      when Nick_Error =>
         T_IO.Put_Line("El nick introducido no puede ser server");
         LLU.Finalize;
      when Expired_Error =>
         T_IO.Put_Line("Server unreachable");
         LLU.Finalize;
      when Except:others =>
         T_IO.Put_Line ("Excepción imprevista: " &
                                 Ada.Exceptions.Exception_Name (Except) & " en: " &
                                 Ada.Exceptions.Exception_Message (Except));
         LLU.Finalize;

end Chat_Client2;

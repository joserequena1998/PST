with Ada.Text_IO;
with Ada.Command_Line;
with Lower_Layer_UDP;
with Ada.Strings.Unbounded;
with Ada.Exceptions;
with Chat_Messages;
with Client_Collections;

procedure Chat_Server is
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
   Server_EP, Client_EP, Admin_EP: LLU.End_Point_Type;
   Nick, Mensaje, Clients_Image, Password: ASU.Unbounded_String;
   Expired: Boolean := False;
   Mess_Type: CM.Message_Type;
   Unique, Finish: Boolean := False;

begin
   if ACL.Argument_Count /= 2 then
      raise Usage_Error;
   end if;

   Port := Natural'Value(ACL.Argument(1));
   Server_EP := LLU.Build(LLU.To_IP(LLU.Get_Host_Name), Port);
   LLU.Bind(Server_EP);  -- Este es el EP del Server

   loop

      LLU.Reset(Buffer);   -- Reinicializo el buffer
      LLU.Receive(Server_EP, Buffer'Access, 36000.0, Expired);  -- Me pongo a esperar en mi EP
      if not Expired then  -- Mientras no expire el tiempo
         --T_IO.Put_Line("Pasa");
         Mess_Type := CM.Message_Type'Input(Buffer'Access);    -- Saco el tipo de mensaje

         if Mess_Type = CM.Init then      -- Si es un init le digo a los lectores que hay un nuevo usuario y lo añado
            Client_EP := LLU.End_Point_Type'Input(Buffer'Access);
            Nick := ASU.Unbounded_String'Input(Buffer'Access);
            begin
               if Nick /= "reader" then
                  Unique := True;   -- Es true porque solo podemos tener un usuario con el mismo nick
                  CC.Add_Client(Clients, Client_EP, Nick, Unique);     -- Añado el nuevo usuario que no es lector
                  Mensaje := ASU.To_Unbounded_String("server");
                  LLU.Reset(Buffer);
                  CM.Message_Type'Output(Buffer'Access, CM.Server);
                  ASU.Unbounded_String'Output(Buffer'Access, Mensaje);
                  ASU.Unbounded_String'Output(Buffer'Access, Nick & " joins the chat");
                  CC.Send_To_All(Clients, Buffer'Access);
               else
                  Unique := False;   -- Es false porque podemos tener mas de un usuario de nick reader
                  CC.Add_Client(Clients, Client_EP, Nick, Unique);     -- Añado el nuevo usuario que es lector
               end if;
               T_IO.Put_Line("INIT received from " & ASU.To_String(Nick));

            exception
               when CC.Client_Collection_Error =>
               T_IO.Put_Line("INIT received from " & ASU.To_String(Nick) & ". Ignored, nick already used");
            end;

         elsif Mess_Type = CM.Writer then    -- Es un mensaje de un cliente escritor y se reenvia a todos los lectores
            Client_EP:= LLU.End_Point_Type'Input(Buffer'Access);
            Mensaje := ASU.Unbounded_String'Input(Buffer'Access);
            begin
               Nick := CC.Search_Client(Clients, Client_EP);   -- Debo buscar el nick porque no viene en el writer
               LLU.Reset(Buffer);
               CM.Message_Type'Output(Buffer'Access, CM.Server);
               ASU.Unbounded_String'Output(Buffer'Access, Nick);
               ASU.Unbounded_String'Output(Buffer'Access, Mensaje);
               CC.Send_To_All(Clients, Buffer'Access);   -- Lo mando a los clientes
               T_IO.Put_Line(ASU.To_String("WRITER received from " & Nick & ": " & Mensaje));

            exception
               when CC.Client_Collection_Error =>
                  T_IO.Put_Line("WRITER received from unknown client. IGNORED");
            end;

         elsif Mess_Type = CM.Logout then
            Client_EP:= LLU.End_Point_Type'Input(Buffer'Access);
            begin
            Nick := CC.Search_Client(Clients, Client_EP);   -- Borramos el nick del usuario
            CC.Delete_Client(Clients, Nick);
            Mensaje := ASU.To_Unbounded_String("server");
            LLU.Reset(Buffer);      -- Le mandamos el LOGOUT a los clientres lectores como un mensaje SERVER
            CM.Message_Type'Output(Buffer'Access, CM.Server);
            ASU.Unbounded_String'Output(Buffer'Access, Mensaje);
            ASU.Unbounded_String'Output(Buffer'Access, Nick & " leaves the chat");
            CC.Send_To_All(Clients, Buffer'Access);
            T_IO.Put_Line(ASU.To_String("LOGOUT received from " & Nick));  -- Imprimimos que hay un LOGOUT

            exception
               when CC.Client_Collection_Error =>
                  T_IO.Put_Line("LOGOUT received from unknown client. IGNORED");
            end;

         elsif Mess_Type = CM.Collection_Request then    -- Recibimos una peticion de la lista de clientes
   			Admin_EP := LLU.End_Point_Type'Input(Buffer'Access);
   			Password := ASU.Unbounded_String'Input(Buffer'Access);   -- Saco la contraseña para saber si es correcta
   			if Password = ASU.To_Unbounded_String(ACL.Argument(2)) then
               LLU.Reset(Buffer);
   				CM.Message_Type'Output(Buffer'Access, CM.Collection_Data);
   				Clients_Image := ASU.To_Unbounded_String(CC.Collection_Image(Clients)); -- En Clients_Image esta, en modo un unico Unbounded_String la lista
   				ASU.Unbounded_String'Output(Buffer'Access, Clients_Image);
   				LLU.Send(Admin_EP, Buffer'Access);

               T_IO.Put_Line("LIST_REQUEST received");
   			else
               T_IO.Put_Line("LIST_REQUEST received. IGNORED, incorrect password");   -- Si no es correcta no le mando la lsita
   			end if;

         elsif Mess_Type = CM.Ban then       -- Recibimos una peticion de banear a un cliente
            Password := ASU.Unbounded_String'Input(Buffer'Access);
            Nick := ASU.Unbounded_String'Input(Buffer'Access);
            if Password = ASU.To_Unbounded_String(ACL.Argument(2)) then      -- Si la contraseña es incorrecta no se hace nada
               begin
               CC.Delete_Client(Clients, Nick);
               T_IO.Put_Line("BAN received for " & ASU.To_String(Nick));

               exception
                  when CC.Client_Collection_Error =>
                     T_IO.Put_Line("Ban received for " & ASU.To_String(Nick) & ". IGNORED, nick not found");
               end;
            
			else
               T_IO.Put_Line("BAN received. IGNORED, incorrect password");
            end if;

         elsif Mess_Type = CM.Shutdown then  -- Recibimos una peticion para decirle al servidor que acabe
            Password := ASU.Unbounded_String'Input(Buffer'Access);
            if Password = ASU.To_Unbounded_String(ACL.Argument(2)) then  -- Comprobamos como siempres que la contraseña es correcta
               Finish := True;
            else
               T_IO.Put_Line("SHUTDOWN received. IGNORED, incorrect password");
            end if;

         end if;

      end if;

      exit when Finish;
   end loop;

   if Finish then
      T_IO.Put_Line("SHUTDOWN received. Finish server");
      LLU.Finalize;     -- Terminamos con la ejecucion
   end if;

exception
   when Usage_Error =>
      LLU.Finalize;
      T_IO.Put_Line("Uso: chat_server <port> <password>");
   when Except:others =>
      Ada.Text_IO.Put_Line ("Excepción imprevista: " &
                              Ada.Exceptions.Exception_Name (Except) & " en: " &
                              Ada.Exceptions.Exception_Message (Except));
      LLU.Finalize;


end Chat_Server;

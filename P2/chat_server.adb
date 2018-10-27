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
   --Clients_Readers: CC.Collection_Type;
   Server_EP, Client_EP: LLU.End_Point_Type;
   Nick, Mensaje: ASU.Unbounded_String;
   Expired: Boolean := False;
   Mess_Type: CM.Message_Type;
   Unique: Boolean;

begin
   if ACL.Argument_Count /= 1 then
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
                  --Nick := Nick & " joins the chat";
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

         elsif Mess_Type = CM.Writer then
            Client_EP:= LLU.End_Point_Type'Input(Buffer'Access);
            Mensaje := ASU.Unbounded_String'Input(Buffer'Access);
            begin
               Nick := CC.Search_Client(Clients, Client_EP);
               LLU.Reset(Buffer);
               CM.Message_Type'Output(Buffer'Access, CM.Server);
               ASU.Unbounded_String'Output(Buffer'Access, Nick);
               ASU.Unbounded_String'Output(Buffer'Access, Mensaje);
               CC.Send_To_All(Clients, Buffer'Access);
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
         end if;

      end if;

   end loop;

exception
   when Usage_Error =>
      LLU.Finalize;
      T_IO.Put_Line("Uso: chat_server <puerto>");
   when Except:others =>
      Ada.Text_IO.Put_Line ("Excepción imprevista: " &
                              Ada.Exceptions.Exception_Name (Except) & " en: " &
                              Ada.Exceptions.Exception_Message (Except));
      LLU.Finalize;


end Chat_Server;

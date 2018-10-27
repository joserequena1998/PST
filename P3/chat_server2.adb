with Chat_Messages;
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Lower_Layer_UDP;
with Ada.Command_Line;
with Ada.Exceptions;
with Server_Handler;    -- Paquete donde hago todo el trabajo basicamente

procedure Chat_Server2 is
   package T_IO renames Ada.Text_IO;
   package ASU renames Ada.Strings.Unbounded;
   package LLU renames Lower_Layer_UDP;
   package CM renames Chat_Messages;
   package ACL renames Ada.Command_Line;
   package SH renames Server_Handler;

   use type Ada.Strings.Unbounded.Unbounded_String;
   use type Lower_Layer_UDP.End_Point_Type;
   use type Chat_Messages.Message_Type;
   -- Esto es necesario para poder comparar Unbounded_Strings, EPs y Chat_Messages

   Usage_Error: exception;

   Port: Natural;
   Server_EP: LLU.End_Point_Type;
   Finish: Boolean;
   C: Character;

begin
   if ACL.Argument_Count /= 2 then
      raise Usage_Error;
   end if;

   Port := Natural'Value(ACL.Argument(1));
   Server_EP := LLU.Build(LLU.To_IP(LLU.Get_Host_Name), Port);
   LLU.Bind(Server_EP, Server_Handler.Handler_S'Access);

   Finish := False;
   while not Finish loop
      T_IO.Get_Immediate(C);
      if C = 'L' or C = 'l' then
         T_IO.New_Line;
         T_IO.Put_Line("ACTIVE CLIENTS");
         T_IO.Put_Line("==============");
         SH.Print_Map_Activos(SH.Clientes_Activos);
         T_IO.New_Line;
      elsif C = 'O' or C = 'o' then
         T_IO.New_Line;
         T_IO.Put_Line("OLD CLIENTS");
         T_IO.Put_Line("==============");
         SH.Print_Map_Angituos(SH.Clientes_Antiguos);
         T_IO.New_Line;
      elsif C = '0' then
         Finish := True;
      else
         T_IO.New_Line;
         T_IO.Put_Line("1) Key in 'L' or 'l' to watch the active clients list");
         T_IO.Put_Line("2) Key in 'O' or 'o' to watch the inactive clients list");
         T_IO.Put_Line("3) Key in '0' to fall down the server");
         T_IO.New_Line;
      end if;
   end loop;

   if Finish then
      T_IO.New_Line;
      T_IO.Put_Line("Bye!");
      LLU.Finalize;
   end if;

exception
      when Usage_Error =>
         T_IO.Put_Line("Uso: ./chat_server2 <puerto> <max clients>");
         LLU.Finalize;
      when Except:others =>
         T_IO.Put_Line ("Excepción imprevista: " &
                                 Ada.Exceptions.Exception_Name (Except) & " en: " &
                                 Ada.Exceptions.Exception_Message (Except));
         LLU.Finalize;

end Chat_Server2;

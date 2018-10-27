with Chat_Messages;
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Lower_Layer_UDP;
with Ada.Command_Line;
with Ada.Exceptions;
with Server_Handler;    -- Paquete donde hago todo el trabajo basicamente
with Client_Handler;
with Protected_Ops;

procedure Chat_Server_3 is
	package T_IO renames Ada.Text_IO;
	package ASU renames Ada.Strings.Unbounded;
	package LLU renames Lower_Layer_UDP;
	package CM renames Chat_Messages;
	package ACL renames Ada.Command_Line;
	package SH renames Server_Handler;
	package CH renames Client_Handler;
	package PO renames Protected_Ops;

	use type Ada.Strings.Unbounded.Unbounded_String;
   use type Lower_Layer_UDP.End_Point_Type;
   use type Chat_Messages.Message_Type;
   -- Esto es necesario para poder comparar Unbounded_Strings, EPs y Chat_Messages

	Usage_Error: exception;
	Delay_Error: exception;
	Pct_Error: exception;
	Clients_Error: exception;

   Port, Max_Clients, Min_Delay, Max_Delay, Fault_Pct: Natural;
   Server_EP: LLU.End_Point_Type;
   Finish: Boolean;
   C: Character;

begin
	if ACL.Argument_Count /= 5 then
      raise Usage_Error;
   end if;

	-- Extraigos los argunentos: puerto maxclients mindelay maxdelay faultpct
	Port := Natural'Value(ACL.Argument(1));
   Server_EP := LLU.Build(LLU.To_IP(LLU.Get_Host_Name), Port);
   LLU.Bind(Server_EP, Server_Handler.Handler_S'Access);

	Max_Clients := Natural'Value(ACL.Argument(2));
	Min_Delay := Natural'Value(ACL.Argument(3));
	Max_Delay := Natural'Value(ACL.Argument(4));
	Fault_Pct := Natural'Value(ACL.Argument(5));

	-- El numero de clientes no puede ser menor que 2 ni mayor que 50
	if (Max_Clients < 2 or Max_Clients > 50) then
		raise Clients_Error;
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

	Finish := False;
   while not Finish loop
      T_IO.Get_Immediate(C);
      if C = 'L' or C = 'l' then
         T_IO.New_Line;
         T_IO.Put_Line("ACTIVE CLIENTS");
         T_IO.Put_Line("==============");
         SH.PO.Protected_Call(SH.Show_Active_Clients'Access);
         T_IO.New_Line;
      elsif C = 'O' or C = 'o' then
         T_IO.New_Line;
         T_IO.Put_Line("OLD CLIENTS");
         T_IO.Put_Line("==============");
         SH.PO.Protected_Call(SH.Show_Inactive_Clients'Access);
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
		T_IO.Put_Line("Uso: ./chat_server_3 <puerto> <Max_Clients> <Min_Delay> <Max_Delay> <Fault_Pct>");
	   LLU.Finalize;
	when Delay_Error =>
		T_IO.Put_Line("Min_Delay debe ser menor que Max_Delay");
		LLU.Finalize;
	when Pct_Error =>
		T_IO.Put_Line("El porcentaje de errores debe ser: 0 < Pct < 100");
		LLU.Finalize;
	when Clients_Error =>
		T_IO.Put_Line("El numero de clientes debe ser 2 < Max_Clients < 50");
		LLU.Finalize;
	when Except:others =>
	    T_IO.Put_Line ("Excepción imprevista: " &
	                    Ada.Exceptions.Exception_Name (Except) & " en: " &
	                    Ada.Exceptions.Exception_Message (Except));
	    LLU.Finalize;

end Chat_Server_3;

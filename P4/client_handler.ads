with Chat_Messages;
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Lower_Layer_UDP;
with Retransmission;
with Protected_Ops;
with Ada.Real_Time;

package Client_Handler is
	package T_IO renames Ada.Text_IO;
   package ASU renames Ada.Strings.Unbounded;
   package CM renames Chat_Messages;
   package LLU renames Lower_Layer_UDP;
	package RTX renames Retransmission;
	package PO renames Protected_Ops;
	package ART renames Ada.Real_Time;

	use type Ada.Strings.Unbounded.Unbounded_String;
	use type Lower_Layer_UDP.End_Point_Type;
	use type Chat_Messages.Message_Type;
	use type Chat_Messages.Seq_N_T;
	use type Ada.Real_Time.Time_Span;
	-- Esto es necesario para poder comparar Unbounded_Strings, EPs, Chat_Messages y num de seq

	Seq_N: CM.Seq_N_T;
	Pending_Msgs: RTX.PMP.Map;
	Retransmission_Times: RTX.RTP.List_Type;
	Wel_rec: Boolean := False;
	Server_EP_Handler: LLU.End_Point_Type;

	function "<" (Left, Right : ART.Time) return Boolean;

	procedure Handler_C (From: in LLU.End_Point_Type;
							   To: in LLU.End_Point_Type;
							   P_Buffer: access LLU.Buffer_Type);

	procedure Timed_Retransmission;

	Plazo_Retransmision: Duration;
	Retransmission_Period: Duration;
	Max_Retransmission_Attempts: Natural;
end Client_Handler;

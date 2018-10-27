with Chat_Messages;
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Lower_Layer_UDP;
with Retransmission;
with Protected_Ops;
with Ada.Calendar;
with Maps_G_Hash;
with Ordered_Maps_G;
with Gnat.Calendar;
with Gnat.Calendar.Time_IO;
with Ada.Real_Time;

package Server_Handler is
	package T_IO renames Ada.Text_IO;
	package ASU renames Ada.Strings.Unbounded;
	package CM renames Chat_Messages;
	package LLU renames Lower_Layer_UDP;
	package RTX renames Retransmission;
	package PO renames Protected_Ops;
   package AC renames Ada.Calendar;
	package ART renames Ada.Real_Time;

	use type Ada.Strings.Unbounded.Unbounded_String;
	use type Lower_Layer_UDP.End_Point_Type;
	use type Chat_Messages.Message_Type;
	use type Chat_Messages.Seq_N_T;
	use type Ada.Real_Time.Time_Span;
	use type Ada.Calendar.Time;
	-- Esto es necesario para poder comparar Unbounded_Strings, EPs, Chat_Messages y num de seq



	type Active_Client_Value_Type is record
		Client_EP_Handler: LLU.End_Point_Type;
		Client_Seq_N: CM.Seq_N_T;
		Last_Connection: AC.Time;
		Server_Seq_N: CM.Seq_N_T;
	end record;


	HASH_SIZE: constant := 10;
	type Hash_Range is mod HASH_SIZE;

	function "<" (Left, Right : ART.Time) return Boolean;


	function Unbounded_String_Hash(US: ASU.Unbounded_String) return Hash_Range;


	package Active_Clients_Package is new Maps_G_Hash (Key_Type => ASU.Unbounded_String,
																		Value_Type => Active_Client_Value_Type,
																		"=" => ASU."=",
																		Hash_Range => Hash_Range,
																		Hash => Unbounded_String_Hash,
																		Max_Elements => 50);
	package ACP renames Active_Clients_Package;
	procedure Show_Active_Clients;


	-- Inactive Clients: Ordered Map
	package Inactive_Clients_Package is new Ordered_Maps_G (Key_Type => ASU.Unbounded_String,
																			  Value_Type => AC.Time,
																			  "=" => ASU."=",
																			  "<" => ASU."<",
																			  Max_Elements => 150);
	package ICP renames Inactive_Clients_Package;
	procedure Show_Inactive_Clients;


	procedure Handler_S (From: in LLU.End_Point_Type;
								To: in LLU.End_Point_Type;
								P_Buffer: access LLU.Buffer_Type);


	Plazo_Retransmision: Duration;
	Retransmission_Period: Duration;
	Max_Retransmission_Attempts: Natural;

end Server_Handler;

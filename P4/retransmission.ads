with Ada.Strings.Unbounded;
with Ada.Real_Time;
with Lower_Layer_UDP;
with Chat_Messages;
with List_Maps_G;
with Ordered_Lists_G;

package Retransmission is
  package ASU renames Ada.Strings.Unbounded;
  package ART renames Ada.Real_Time;
  package LLU renames Lower_Layer_UDP;
  package CM renames Chat_Messages;

  use type ART.Time;
  use type LLU.End_Point_Type;
  use type CM.Seq_N_T;

  -- Pending_Msgs
	type Pending_Msgs_Key_Type is record
		Sender_EP: LLU.End_Point_Type;
		Receiver_EP: LLU.End_Point_Type;
		Seq_N: CM.Seq_N_T;
	end record;

	type Pending_Msgs_Value_Type is record
      Header_Msg: CM.Message_Type;
		Nick: ASU.Unbounded_String;
		Comment: ASU.Unbounded_String;
      Retransmission_Attempts: Natural := 0;
   end record;

	function Pending_Msgs_Keys_Equals (K1, K2: Pending_Msgs_Key_Type) return Boolean;

	package Pending_Msgs_Package is new List_Maps_G (Key_Type => Pending_Msgs_Key_Type,
		                                              Value_Type => Pending_Msgs_Value_Type,
		                                              "=" => Pending_Msgs_Keys_Equals);
	package PMP renames Pending_Msgs_Package;



	-- Retransmission_Times
	type Retransmission_Times_Element_Type is record
		Time: ART.Time;
		Pending_Msgs_Key: Pending_Msgs_Key_Type; -- ID
	end record;

	function Retransmission_Times_Element_Bigger (E1, E2: Retransmission_Times_Element_Type) return Boolean;

	package Retransmission_Times_Package is new Ordered_Lists_G (Element_Type => Retransmission_Times_Element_Type,
		                                                          ">" => Retransmission_Times_Element_Bigger);
	package RTP renames Retransmission_Times_Package;
end Retransmission;

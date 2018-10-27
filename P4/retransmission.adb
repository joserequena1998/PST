package body Retransmission is

  function Pending_Msgs_Keys_Equals(K1, K2: Pending_Msgs_Key_Type) return Boolean is
  begin
     if (K1.Sender_EP = K2.Sender_EP and K1.Receiver_EP = K2.Receiver_EP and K1.Seq_N = K2.Seq_N) then
        return True;
     else
        return False;
     end if;
  end Pending_Msgs_Keys_Equals;

  function Retransmission_Times_Element_Bigger(E1, E2: Retransmission_Times_Element_Type) return Boolean is
  begin
     if (E1.Time > E2.Time) then
        return True;
     else
        return False;
     end if;
  end Retransmission_Times_Element_Bigger;

end Retransmission;

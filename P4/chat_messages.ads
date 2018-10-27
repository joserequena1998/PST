package Chat_Messages is
   type Message_Type is (Init, Welcome, Writer, Server, Logout, Ack);

   type Seq_N_T is mod Integer'Last;
end Chat_Messages;


package body Client_Handler is

   procedure Handler_C (From: in LLU.End_Point_Type;
                        To: in LLU.End_Point_Type;
                        P_Buffer: access LLU.Buffer_Type) is
      Mess_Type: CM.Message_Type;
      Nick, Comentario: ASU.Unbounded_String;
   begin
      Mess_Type := CM.Message_Type'Input(P_Buffer);   -- Tipo de mensaje
      Nick := ASU.Unbounded_String'Input(P_Buffer);   -- Luego el siguiente campo a sacar es el nick
      Comentario := ASU.Unbounded_String'Input(P_Buffer);   -- Y por ultimo el comentario
      T_IO.New_Line;
      T_IO.Put_Line(ASU.To_String(Nick) & " : " & ASU.To_String(Comentario));
      T_IO.Put(">>");
   end Handler_C;

end Client_Handler;

with Chat_Messages;
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Lower_Layer_UDP;

package Client_Handler is
   package T_IO renames Ada.Text_IO;
   package ASU renames Ada.Strings.Unbounded;
   package CM renames Chat_Messages;
   package LLU renames Lower_Layer_UDP;

   procedure Handler_C (From: in LLU.End_Point_Type;
                        To: in LLU.End_Point_Type;
                        P_Buffer: access LLU.Buffer_Type);

end Client_Handler;

with Chat_Messages;
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Lower_Layer_UDP;
with Ada.Calendar;
with Ada.Exceptions;
with Maps_G;
with Ada.Command_Line;

package Server_Handler is
   package T_IO renames Ada.Text_IO;
   package ASU renames Ada.Strings.Unbounded;
   package LLU renames Lower_Layer_UDP;
   package CM renames Chat_Messages;
   package ACL renames Ada.Command_Line;
   package AC renames Ada.Calendar;

   use type CM.Message_Type;
   use type ASU.Unbounded_String;
   use type LLU.End_Point_Type;
   use type AC.Time;
   -- Esto lo necesito para comparar estos tipos

   No_Elements: exception;

   -- Para la lista de clientes activos necesito en value un record con el EP y fecha de ultima accion
   type Value_Record is record
      EP: LLU.End_Point_Type;
      Time: AC.Time;
   end record;

   -- Creo mi lista de clientes activos
   package Clientes_On is new Maps_G (Key_Type     => ASU.Unbounded_String,
                                      Value_Type   => Value_Record,
                                      Max_Elements => 50,
                                      "="          => ASU."=");

   -- Creo mi lista de clientes antiguos o inactivos
   package Clientes_Off is new Maps_G (Key_Type     => ASU.Unbounded_String,
                                       Value_Type   => AC.Time,   -- Ahora el campo value solo es la ultima accion ya que no nos piden tener el EP
                                       Max_Elements => 150,
                                       "="          => ASU."=");

   Clientes_Activos: Clientes_On.Map;      -- Mi lista de clientes activos
   Clientes_Antiguos: Clientes_Off.Map;    -- Mi lista de clientes antiguos

   function Time_Image (T: AC.Time) return String;

   function Imprimir_LLU (Client_EP: ASU.Unbounded_String) return String;   -- Esto me va a permitir impirimir el EP bien

   procedure Print_Map_Activos (M: Clientes_On.Map);

   procedure Print_Map_Angituos (M: Clientes_Off.Map);

   procedure Handler_S (From: in LLU.End_Point_Type;
                      To: in LLU.End_Point_Type;
                      P_Buffer: access LLU.Buffer_Type);

   procedure Send_To_All (M: in Clientes_On.Map;
                        Nick: in ASU.Unbounded_String;
                        P_Buffer: access LLU.Buffer_Type);

   procedure Throw_Out_One (M: Clientes_On.Map;
                           P_Buffer: access LLU.Buffer_Type;
                           Cliente_Elegido: out Clientes_On.Element_Type);

end Server_Handler;

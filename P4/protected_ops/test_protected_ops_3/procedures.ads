with Ada.Real_Time;
with Maps_G;
With Ada.Strings.Unbounded;

package Procedures is
   package ASU  renames Ada.Strings.Unbounded;

   package Maps is new Maps_G (Key_Type   => ASU.Unbounded_String,
                               Value_Type => ASU.Unbounded_String,
                               "="        => ASU."=");
   A_Map : Procedures.Maps.Map;


   function Image (T: Ada.Real_Time.Time) return String;

   procedure Timed_Procedure;

   procedure Print_Map;

end Procedures;

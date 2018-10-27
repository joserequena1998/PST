with Ada.Text_IO;
with Protected_Ops;
with Ada.Real_Time;

package body Procedures is

   function Image (T: Ada.Real_Time.Time) return String is
      SC: Ada.Real_Time.Seconds_Count;
      TS: Ada.Real_Time.Time_Span;
   begin
      Ada.Real_Time.Split(T, SC, TS);

      return Ada.Real_Time.Seconds_Count'Image(SC) &
        ":" &
        Duration'Image(Ada.Real_Time.To_Duration(TS));
   end Image;


   procedure Timed_Procedure is
      use type Ada.Real_Time.Time;
   begin
      Ada.Text_IO.Put (Image(Ada.Real_Time.Clock));
      Ada.Text_IO.Put_Line(" Timed_Procedure");

      Protected_Ops.Program_Timer_Procedure
        (Procedures.Timed_Procedure'Access,
         Ada.Real_Time.Clock + Ada.Real_Time.Milliseconds(1000));

   end Timed_Procedure;


   procedure Procedure_1 is
   begin
      Ada.Text_IO.Put (Image(Ada.Real_Time.Clock));
      Ada.Text_IO.Put_Line(" Procedure_1");

      delay 3.0;
   end Procedure_1;

   procedure Procedure_2 is
   begin
      Ada.Text_IO.Put (Image(Ada.Real_Time.Clock));
      Ada.Text_IO.Put_Line(" Procedure_2");

      delay 3.0;
   end Procedure_2;


end Procedures;

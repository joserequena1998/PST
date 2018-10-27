with Ada.Real_Time;
with Ada.Text_IO;
with Procedures;
with Protected_Ops;

procedure Test_Protected_Ops is
   use type Ada.Real_Time.Time;

begin

   Ada.Text_IO.Put (Procedures.Image(Ada.Real_Time.Clock));
   Ada.Text_IO.Put_Line (" Start");

   Protected_Ops.Program_Timer_Procedure
     (Procedures.Timed_Procedure'Access,
      Ada.Real_Time.Clock + Ada.Real_Time.Milliseconds(3000));

   delay 5.0;

   Protected_Ops.Protected_Call (Procedures.Procedure_1'Access);

   delay 5.0;

   Protected_Ops.Protected_Call (Procedures.Procedure_2'Access);

   delay 10.0;

   Ada.Text_IO.Put (Procedures.Image(Ada.Real_Time.Clock));
   Ada.Text_IO.Put_Line (" Finish");

end Test_Protected_Ops;

with Ada.Real_Time;
with Ada.Text_IO;
with Procedures;
with Protected_Ops;
with Ada.Strings.Unbounded;

procedure Test_Protected_Ops_2 is
   use type Ada.Real_Time.Time;

   package ASU  renames Ada.Strings.Unbounded;
   package ATIO renames Ada.Text_IO;


   Value   : ASU.Unbounded_String;


begin

   Ada.Text_IO.Put (Procedures.Image(Ada.Real_Time.Clock));
   Ada.Text_IO.Put_Line (" Start");

   -- Program a delete of "www.urjc.es" in 5000ms
   Protected_Ops.Program_Timer_Procedure
     (Procedures.Timed_Procedure'Access,
      Ada.Real_Time.Clock + Ada.Real_Time.Milliseconds(5000));


   Procedures.Maps.Put (Procedures.A_Map,
             ASU.To_Unbounded_String ("facebook.com"),
             ASU.To_Unbounded_String ("69.63.189.16"));

   Procedures.Maps.Put (Procedures.A_Map,
             ASU.To_Unbounded_String ("google.com"),
             ASU.To_Unbounded_String ("66.249.92.104"));

   Procedures.Maps.Put (Procedures.A_Map,
             ASU.To_Unbounded_String ("www.urjc.es"),
             ASU.To_Unbounded_String ("212.128.240.25"));

   Procedures.Maps.Put (Procedures.A_Map,
             ASU.To_Unbounded_String ("latimes.com"),
             ASU.To_Unbounded_String ("69.63.189.11"));

   -- Print_Map will be delayed while the cursor is visiting node
   --  "www.urjc.es" of the linked list
   Procedures.Print_Map;

   Ada.Text_IO.Put (Procedures.Image(Ada.Real_Time.Clock));
   Ada.Text_IO.Put_Line (" Finish");

end Test_Protected_Ops_2;


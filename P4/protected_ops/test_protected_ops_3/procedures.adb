with Ada.Text_IO;
with Protected_Ops;
with Ada.Real_Time;

package body Procedures is
   package ATIO renames Ada.Text_IO;

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
      Success : Boolean;
   begin
      Ada.Text_IO.Put (Image(Ada.Real_Time.Clock));
      Ada.Text_IO.Put_Line(" Timed_Procedure deleting www.urjc.es");

      ATIO.New_Line;
      Maps.Delete (A_Map, ASU.To_Unbounded_String("www.urjc.es"), Success);

   end Timed_Procedure;


   procedure Print_Map is
      C: Procedures.Maps.Cursor := Procedures.Maps.First(Procedures.A_Map);
      use type ASU.Unbounded_String;
   begin
      Ada.Text_IO.Put_Line ("Map");
      Ada.Text_IO.Put_Line ("===");

      while Procedures.Maps.Has_Element(C) loop
         Ada.Text_IO.Put_Line (ASU.To_String(Procedures.Maps.Element(C).Key) & " " &
                                 ASU.To_String(Procedures.Maps.Element(C).Value));

         Procedures.Maps.Next(C);

         if Procedures.Maps.Has_Element(C) and then
           Procedures.Maps.Element(C).Key = ASU.To_Unbounded_String("www.urjc.es") then
            delay 7.0;
         end if;



      end loop;

   end Print_Map;



end Procedures;

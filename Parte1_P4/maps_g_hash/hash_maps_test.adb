with Ada.Text_IO;
With Ada.Strings.Unbounded;
with Ada.Numerics.Discrete_Random;
with Maps_G_Hash;


procedure Hash_Maps_Test is
   package ASU  renames Ada.Strings.Unbounded;
   package ATIO renames Ada.Text_IO;

   HASH_SIZE:   constant := 10;

   type Hash_Range is mod HASH_SIZE;

   function Natural_Hash (N: Natural) return Hash_Range is
   begin
      return Hash_Range'Mod(N);
   end Natural_Hash;


   package Maps is new Maps_G_Hash (Key_Type   => Natural,
                                    Value_Type => Natural,
                                    Max_Elements => 50,
                                    "="        => "=",
                                    Hash_Range => Hash_Range,
                                    Hash => Natural_Hash);


   procedure Print_Map (M : Maps.Map) is
      C: Maps.Cursor := Maps.First(M);
      --Cursor: Maps.Cursor := Maps.First(M);
   begin
      Ada.Text_IO.Put_Line ("Map");
      Ada.Text_IO.Put_Line ("===");

      while Maps.Has_Element(C) loop
         Ada.Text_IO.Put_Line (Natural'Image(Maps.Element(C).Key) & " " &
                               Natural'Image(Maps.Element(C).Value));

         Maps.Next(C);
         --Ada.Text_IO.Put_Line (Natural'Image(Maps.Element(C).Key) & " " &
                               --Natural'Image(Maps.Element(C).Value));
      end loop;
   end Print_Map;




   procedure Do_Put (M: in out Maps.Map; K: Natural; V: Natural) is
   begin
      Ada.Text_IO.New_Line;
      ATIO.Put_Line("Putting" & Natural'Image(K));
      Maps.Put (M, K, V);
      Print_Map(M);
   exception
      when Maps.Full_Map =>
         Ada.Text_IO.Put_Line("Full_Map");
   end Do_Put;


   procedure Do_Get (M: in out Maps.Map; K: Natural) is
      V: Natural;
      Success: Boolean;
   begin
      Ada.Text_IO.New_Line;
      ATIO.Put_Line("Getting" & Natural'Image(K));
      Maps.Get (M, K, V, Success);
      if Success then
         Ada.Text_IO.Put_Line("Value:" & Natural'Image(V));
         Print_Map(M);
      else
         Ada.Text_IO.Put_Line("Element not found!");
      end if;
   end Do_Get;


   procedure Do_Delete (M: in out Maps.Map; K: Natural) is
      Success: Boolean;
   begin
      Ada.Text_IO.New_Line;
      ATIO.Put_Line("Deleting" & Natural'Image(K));
      Maps.Delete (M, K, Success);
      if Success then
         Print_Map(M);
      else
         Ada.Text_IO.Put_Line("Element not found!");
      end if;
   end Do_Delete;



   A_Map : Maps.Map;

begin

   -- First puts
   Do_Put (A_Map, 10, 10);
   Do_Put (A_Map, 20, 20);
   Do_Put (A_Map, 11, 11);
   Do_Put (A_Map, 30, 30);
   Do_Put (A_Map, 21, 21);
   Do_Put (A_Map, 15, 15);
   Do_Put (A_Map, 16, 16);
   Do_Put (A_Map, 40, 40);
   Do_Put (A_Map, 25, 25);
   Do_Put (A_Map, 50, 50);


   -- Now deletes
   Do_Delete (A_Map, 21);
   Do_Delete (A_Map, 15);
   Do_Delete (A_Map, 25);
   Do_Delete (A_Map, 39);


   -- Now gets
   Do_Get (A_Map, 21);
   Do_Get (A_Map, 40);
   Do_Get (A_Map, 50);
   Do_Get (A_Map, 39);


end Hash_Maps_Test;

with Ada.Text_IO;
with Ordered_Lists_G;

procedure Ordered_Lists_Test_2 is
   package T_IO renames Ada.Text_IO;

   package Lists is new Ordered_Lists_G (Element_Type => Integer, ">" => ">");

   A_List: Lists.List_Type;
   Number: Integer;
begin
   Lists.Add(A_List, 5);
   Lists.Add(A_List, 3);
   Lists.Add(A_List, 6);
   Lists.Add(A_List, 4);
   Lists.Add(A_List, 1);
   Lists.Add(A_List, 7);

   loop
      Number := Lists.Get_First(A_List);
      T_IO.Put_Line(Integer'Image(Number));

      Lists.Delete_First(A_List);
   end loop;

exception
   when Lists.Empty_List =>
      T_IO.Put_Line("Empty list");

end Ordered_Lists_Test_2;

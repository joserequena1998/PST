with Ada.Unchecked_Deallocation;

generic
  type Element_Type is private;
  with function ">" (E1, E2: Element_Type) return Boolean;

package Ordered_Lists_G is
  
  type List_Type is limited private;

  procedure Add(List: in out List_Type; Element: in Element_Type);

  Empty_List: exception;

  function Get_First(List: in List_Type) return Element_Type;

  procedure Delete_First(List: in out List_Type);

private
  type Cell;
  type List_Type is access Cell;
  type Cell is record
    Element: Element_Type;
    Next: List_Type;
  end record;
end Ordered_Lists_G;

with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

--
--  TAD genérico de una tabla de símbolos (map) implementada como una lista
--  enlazada no ordenada.
--

generic
   type Key_Type is private;
   type Value_Type is private;
   Max_Elements: in Natural;     -- Numero maximo de elementos que puede tener la lista
   with function "=" (K1, K2: Key_Type) return Boolean;
   type Hash_Range is mod <>;
   with function Hash (K: Key_Type) return Hash_Range;

package Maps_G_Hash is
   package ASU renames Ada.Strings.Unbounded;

   HASH_SIZE:   constant := 10;


   type Map is limited private;

   procedure Get (M       : in out Map;
                  Key     : in  Key_Type;
                  Value   : out Value_Type;
                  Success : out Boolean);


   Full_Map : exception;
   procedure Put (M     : in out Map;
                  Key   : Key_Type;
                  Value : Value_Type);

   procedure Delete (M      : in out Map;
                     Key     : in  Key_Type;
                     Success : out Boolean);


   function Map_Length (M : Map) return Natural;

   --
   -- Cursor Interface for iterating over Map elements
   --
   type Cursor is limited private;
   function First (M: Map) return Cursor;
   procedure Next (C: in out Cursor);
   function Has_Element (C: Cursor) return Boolean;
   type Element_Type is record
      Key:   Key_Type;
      Value: Value_Type;
   end record;
   No_Element: exception;

   -- Raises No_Element if Has_Element(C) = False;
   function Element (C: Cursor) return Element_Type;

private
   -- Linked List
   --type Cell;
   --type Cell_A is access Cell;
   --type Cell is record
     -- Key: Key_Type;
      --Value: Value_Type;
      --Next: Cell_A;
   --end record;
   --type Linked_List is record
     -- P_First: Cell_A;
     -- P_Last: Cell_A; -- Nota: Comentar
   --end record;

  type Cell;
  type Cell_A is access Cell; -- puntero al principio
  type Cell is record
    Key: Key_Type;
    Value: Value_Type;
    Next: Cell_A;
  end record;

   -- Hash Map
   type Map_Array is array (Hash_Range) of Cell_A;
   type Map_Array_A is access Map_Array;
   type Map is record
      P_Array: Map_Array_A := new Map_Array;
      Length: Natural := 0;
   end record;

   -- Cursor
   type Cursor is record
      M: Map;
      Index: Hash_Range;   -- Indice de la tabla hash
      Element_A: Cell_A;   -- Indice de la lista enlazada donde me hallo
   end record;

end Maps_G_Hash;

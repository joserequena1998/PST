with Word_Lists;
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

use Ada.Text_IO;
use Ada.Strings.Unbounded;

package body Word_Lists is
   package T_IO renames Ada.Text_IO;


   procedure Free is new Ada.Unchecked_Deallocation (Cell, Word_List_Type);

-----------------------------------------------------------------------------------------
   procedure Add_Word (List: in out Word_List_Type; Word: in ASU.Unbounded_String) is

      P_List, P_Aux: Word_List_Type;
      Found: Boolean;

   begin
      P_Aux := Null;
      P_List := List;
      Found:= False;

      while P_List /= Null and not Found loop
         if P_List.Word = Word then
            Found := True;
         else
            P_Aux := P_List;
            P_List := P_List.Next;
         end if;
      end loop;

      if Found then
         P_List.Count := P_List.Count + 1;
      else
         if List = Null then
            List := new Cell'(Word, 1, null);
         else
            P_Aux.Next := new Cell'(Word, 1, null);
         end if;
	 end if;

   end Add_Word;

-----------------------------------------------------------------------------------------
   procedure Delete_Word (List: in out Word_List_Type; Word: in ASU.Unbounded_String) is

      P_List, P_Aux: Word_List_Type;
      Found: Boolean;

   begin
      P_Aux := Null;
      P_List := List;
      Found:= False;

      while P_List /= Null and not Found loop
         if P_List.Word = Word then
            Found := True;
         else
            P_Aux := P_List;
            P_List := P_List.Next;
         end if;
      end loop;

      if Found then
         if P_Aux = Null then
            List := List.Next;
         else
            P_Aux.Next := P_List.Next;
         end if;
         Free(P_List);
      else
         raise Word_List_Error;
      end if;

      exception
           when Word_List_Error =>
               T_IO.Put_Line("La palabra introducida no se encuentra en la lista");


   end Delete_Word;

------------------------------------------------------------------------------------------------------
   procedure Search_Word (List: in Word_List_Type; Word: in ASU.Unbounded_String; Count: out Natural) is

      P_List: Word_List_Type;
      Found: Boolean;

   begin

      P_List := List;
      Found:= False;

      while P_List /= Null and not Found loop
         if P_List.Word = Word then
            Count := P_List.Count;
            Found := True;
         end if;
         P_List := P_List.Next;
      end loop;
      -- Si no encontramos la palabra es que no aparece -> su contador es cero
      if not Found then
         Count := 0;
      end if;

   end Search_Word;

-------------------------------------------------------------------------------------------------
   procedure Max_Word (List: in Word_List_Type; Word: out ASU.Unbounded_String; Count: out Natural) is

      P_List: Word_List_Type;

   begin
      if List = Null then     -- Si la lista esta vacia no hay palabra con maximo, salta excepcion
         raise Word_List_Error;
      end if;

      P_List := List;
      -- Empezamos cogiendo como palabra mas encontrada la primera y luego comparamos su count con las demas
      Count := P_List.Count;
      Word := P_List.Word;
      P_List := P_List.Next;

      while P_List /= Null loop
         if P_List.Count > Count then
            Count := P_List.Count;
            Word := P_List.Word;
         end if;
         P_List := P_List.Next;
      end loop;

      exception
           when Word_List_Error =>
               T_IO.Put_Line("No hay ninguna palabra mas encontrada porque la lista esta vacia");


   end Max_Word;

 ----------------------------------------------------------------------------------------
   procedure Print_All (List: in Word_List_Type) is

      P_List: Word_List_Type;

   begin
      if List = Null then     -- Si la lista esta vacia no hay palabras y mostramos que no las hay
         T_IO.Put_Line("No words");
      end if;

      P_List := List;

      while P_List /= Null loop
         T_IO.Put_Line("|" & ASU.To_String(P_List.Word) & "| - " & Natural'Image(P_List.Count));
         P_List := P_List.Next;
      end loop;

   end Print_All;

-----------------------------------------------------------------------------------------------
   procedure Delete_List (List: in out Word_List_Type) is

       P_List: Word_List_Type;

   begin
       if List = Null then     -- Si la lista esta vacia no hay palabras y no podemos borrarlas
          T_IO.Put_Line("No words to delete");
       end if;

       P_List := List;

	  while(List /= Null) loop
		List := P_List.Next;
		Free(P_List);
		P_List := List;
	  end loop;

   end Delete_List;

end Word_Lists;

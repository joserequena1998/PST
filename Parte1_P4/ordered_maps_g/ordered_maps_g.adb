package body Ordered_Maps_G is
   -- Map
   procedure Binary_Search(M: in Map;
                           Key: in Key_Type;
                           Index: out Positive;
                           Found: out Boolean) is
      Left_Index: Natural;
      Right_Index: Natural;
      Middle_Index: Natural;
  begin
      Found := False;
      Left_Index := Min;   -- Apunto al minimo indice
      Right_Index := M.Length;   -- Y al maximo indice
      while not Found and Left_Index <= Right_Index loop -- Si no lo encuentro y el lado izq no llega al derecho
         Middle_Index := (Left_Index + Right_Index) / 2;    --Calculo el indice en mitad de lo que me queda de busqueda
         if M.P_Array(Middle_Index).Key = Key then -- Si justo ahi coincide
            Found := True; -- Lo he encontrado
            Index := Middle_Index;  -- Y el indice es el de la mitad
         else  -- Si no coincide justo ahi, tengo que hacer ese proceso una y otra vez
            if Key < M.P_Array(Middle_Index).Key then   -- Si la clave es menor tiro para el lado izq
               Right_Index := Middle_Index - 1; -- y el indice derecho pasa a ser el inidice medio menos 1, el nuevo tope (nuevo M.Length)
            else  -- Si la clave es mayor tiro para el lado derecho
               Left_Index := Middle_Index + 1;  -- y el indice izq pasa a ser el inidice medio mas 1, el nuevo minimo
            end if;
         end if;
      end loop;

      if not Found then -- si no lo encuentro
         if M.Length = 0 then -- Porque no hay elementos
            Index := Min;  -- Devuelvo el minimo que es 1
         else  -- Si hay elementos
           if Key < M.P_Array(Middle_Index).Key then  -- Si la clave es menor que la del indice medio devuelvo como indice el del medio
             Index := Middle_Index;
           else
             Index := Middle_Index + 1;   -- Si no devuelvo el indice siguiente 
           end if;
         end if;
       end if;

    end Binary_Search;

--------------------------------------------------------------------------------

    procedure Get(M: in out Map;
                Key: in Key_Type;
                Value: out Value_Type;
                Success: out Boolean) is
      Index: Positive;
    begin
      Binary_Search(M, Key, Index, Success);
      if Success then
         Value := M.P_Array(Index).Value;
      end if;
   end Get;

--------------------------------------------------------------------------------

   procedure Put(M: in out Map;
                 Key: in Key_Type;
                 Value: in Value_Type) is
       Index: Positive;
       Found: Boolean;
   begin
       Binary_Search(M, Key, Index, Found);
       if Found then
         M.P_Array(Index).Value := Value;
       else
         if M.Length < Max_Elements then
           for J in reverse Index..M.Length loop
             M.P_Array(J + 1) := M.P_Array(J);
           end loop;
           M.P_Array(Index).Key := Key;
           M.P_Array(Index).Value := Value;
           M.Length := M.Length + 1;
         else
           raise Full_Map;
         end if;
       end if;
     end Put;

--------------------------------------------------------------------------------

     procedure Delete(M: in out Map;
                       Key: in Key_Type;
                       Success: out Boolean) is
       Index: Positive;
     begin
       Binary_Search(M, Key, Index, Success);
       if Success then
         for J in (Index + 1)..M.Length loop
           M.P_Array(J - 1) := M.P_Array(J);
         end loop;
         M.Length := M.Length - 1;
       end if;
     end Delete;

--------------------------------------------------------------------------------

     function Map_Length(M: in Map) return Natural is
     begin
       return M.Length;
     end Map_Length;

     -- Cursor
     function First(M: in Map) return Cursor is
       C: Cursor;
     begin
       C.M := M;

       if M.Length /= 0 then
         C.Element_I := 1;
       else
         C.Element_I := 0;
       end if;

       return C;
     end First;

--------------------------------------------------------------------------------

     procedure Next(C: in out Cursor) is
     begin
       if C.Element_I /= 0 then
         C.Element_I := C.Element_I + 1;
         if C.Element_I > C.M.Length then
           C.Element_I := 0;
         end if;
       end if;
     end Next;

--------------------------------------------------------------------------------

     function Has_Element(C: in Cursor) return Boolean is
     begin
       return C.Element_I /= 0;
     end Has_Element;

--------------------------------------------------------------------------------

     function Element(C: in Cursor) return Element_Type is
     begin
       if C.Element_I /= 0 then
         return (Key => C.M.P_Array(C.Element_I).Key,
                 Value => C.M.P_Array(C.Element_I).Value);
       else
         raise No_Element;
       end if;
     end Element;

end Ordered_Maps_G;

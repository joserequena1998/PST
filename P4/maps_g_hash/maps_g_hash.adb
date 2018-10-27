--
--  TAD genérico de una tabla de símbolos (map) implementada como una lista
--  enlazada no ordenada.
--

package body Maps_G_Hash is


   procedure Free is new Ada.Unchecked_Deallocation(Cell, Cell_A);

   procedure Get (M       : in out Map;
                  Key     : in  Key_Type;
                  Value   : out Value_Type;
                  Success : out Boolean) is
      Index: Hash_Range;
      Found: Boolean;
      P_Aux: Cell_A;
   begin
      Index := Hash(Key);
      Found := False;
      P_Aux := M.P_Array(Index);

	   while not Found and P_Aux /= Null loop
		  if P_Aux.Key = Key then
		    Found := True;
		  else
			 P_Aux := P_Aux.Next;
		  end if;
	   end loop;

	   if Found then
		  Value := P_Aux.Value;
	   end if;

	   Success := Found;
   end Get;

--------------------------------------------------------------------------------

   procedure Put (M     : in out Map;
                  Key   : Key_Type;
                  Value : Value_Type) is
     Index: Hash_Range;
     Found: Boolean;
     P_Aux: Cell_A;
	  --P_New: Cell_A;
   begin
     Index := Hash(Key);
     Found := False;
     P_Aux := M.P_Array(Index);
	  --P_New := new Cell_A_Cell'(Key,Value, Null);


	  if P_Aux = Null then    -- Si P_Aux es null es que no hay ningun elemento
       M.P_Array(Index) := new Cell'(Key,Value, Null);   -- y apunto el el puntero al primero al unico y nuevo elemento
     else  -- Si no es null es que hay al menos uno
       if P_Aux.Key = Key then   -- Si la clave es igual a la del primer elemento
          Found := True;   -- Lo hemos encontrado
       end if;
       if not Found then   -- Si no es igual al primero miramos a ver si es igual al resto
   		 while P_Aux.Next /= Null and not Found loop  -- Mientras que el siguiente nodo no sea null y no lo hayamos encontrado
   		   P_Aux := P_Aux.Next;  -- Apuntamos al siguiente
            if P_Aux.Key = Key then -- Si la clave es igual
               Found := True;    -- lo hemos encontrado
            end if;  -- Si no pues seguimos avanzando
   		 end loop;
          if not Found then   -- Si no esta encontrado en ningun elemento
             P_Aux.Next := new Cell'(Key,Value, Null);   -- Se añade al final
          end if;
       end if;
	  end if;

     if not Found then  -- Si no lo encontramos
        M.Length := M.Length + 1;   -- Aumentamos en uno el numero de elementos
     else
        P_Aux.Value := Value; -- Si lo encontramos solo actualizamos su valor, no sumamos elementos
     end if;

     Ada.Text_IO.Put_Line(Natural'Image(M.Length));

   end Put;

--------------------------------------------------------------------------------
   procedure Delete (M      : in out Map;
                     Key     : in  Key_Type;
                     Success : out Boolean) is
      Index: Hash_Range;
      Found: Boolean;
      P_Aux: Cell_A;
      P_Prev: Cell_A;
   begin
      Index := Hash(Key);
      Found := False;
      P_Aux := M.P_Array(Index);
      P_Prev := null;

      while not Found and P_Aux /= null loop -- si no lo he encontrado y lo que apunto no es null
         if P_Aux.Key = Key then -- si mi clave es igual a la que me meten
            Found := True; -- Lo he encontrado
         else
            P_Prev := P_Aux;  -- Si no, avanzo
            P_Aux := P_Aux.Next;
         end if;
      end loop;

      if Found then  -- Si lo he encontrado lo borro
         if P_Prev = null then   -- Si borramos el primero P_Prev seria null
            if P_Aux.Next = null then  -- Si el siguiente tambien es null es que en esa posicion solo tenia un elemento
               Free(P_Aux);
               M.P_Array(Index) := null;  -- Tengo que apuntar esto a null
            else  -- Si no es null es que hay mas de uno
               M.P_Array(Index) := P_Aux.Next;  -- Luego el puntero al primer elemento deberia apuntar al siguiente elemento del que borro
               Free(P_Aux);
            end if;
         else  -- Si P_Prev no es null es que no estamos borrando el primero
            P_Prev.Next := P_Aux.Next; -- apunto el siguiente del anterior al que estoy al siguiente del que estoy
            Free(P_Aux);   -- y libero donde estoy, lo borro
         end if;
      end if;

      Success := Found;

      if Success then
         M.Length := M.Length - 1;
      end if;

      Ada.Text_IO.Put_Line(Natural'Image(M.Length));

   end Delete;

--------------------------------------------------------------------------------
   function Map_Length (M : Map) return Natural is
   begin
      return M.Length;
   end Map_Length;

   --
   -- Cursor Interface for iterating over Map elements
   --

   function First (M: Map) return Cursor is
     C: Cursor;
	  P_Aux: Cell_A;
	  Found: Boolean;
	  Index: Hash_Range;
   begin
     C.M := M;
	  Found := False;
	  Index := 0;
	  P_Aux := M.P_Array(Index);

     --Ada.Text_IO.Put_Line("Pasa a First");
    if M.Length = 0 then   -- Si no hay elementos devulevo un cursor que no apunte a nada ya que no hay primero
       C.Index := 0;
       C.Element_A := Null;
    else    -- Si hay empiezo la busqueda
      while not Found and Natural(Index) < HASH_SIZE loop -- Mientras no lo encuentre y no llegue al final busco
         --Ada.Text_IO.Put_Line("Pasa al bucle");
         if P_Aux /= Null then   -- Si a lo que apunto es disitnto de null lo he encontrado
            Found := True;
         else  -- si no aumento en uno el indice y vuelvo a apuntar ahi mi puntero
            Index := Index + 1;
			   P_Aux := M.P_Array(Index);
         end if;
      end loop;
    end if;

	  if not Found then -- -si no lo encuentro devuelvo los campos a null
	    C.Index := 0;
		 C.Element_A := Null;
	  else
       --P_Aux.Next := Null;
		 C.Index := Index;
		 C.Element_A := P_Aux;
	  end if;

      return C;
   end First;

--------------------------------------------------------------------------------
   procedure Next(C: in out Cursor) is
      Index : Natural;
      P_Aux: Cell_A;
   begin
      Index := Natural(C.Index);
      P_Aux := C.M.P_Array(C.Index);

      if P_Aux /= Null and C.Element_A.Next /= Null then -- Si a lo que apunto es distinto de null y el siguiente tambien sigo avanzando
         C.Element_A := C.Element_A.Next;
      else
         if Index < 9 then    -- si el indice es menor que 9 es que no estoy en el ultimo
            C.Index := C.Index + 1; --luego sigo avanzando en el array de indices
            Index := Index + 1;
            P_Aux := C.M.P_Array(C.Index);
            while P_Aux = Null and Index <= 9 loop -- Si el nuevo puntero de la nueva posicion es null y no es el ultimo indice
              C.Index := C.Index + 1; -- pasamos al siguiente
              Index := Index + 1;
              P_Aux := C.M.P_Array(C.Index);
            end loop;
            if Index = 10 then   -- Si es 10 el inidice estamos en el ultimo
              C.Element_A := Null;  -- Devolvemos null porque no encontramos siguiente
            else
              C.Element_A := C.M.P_Array(C.Index); -- si no devolvemos la primera posicion
            end if;
         else
            C.Element_A := Null;
         end if;
      end if;

   end Next;

--------------------------------------------------------------------------------
   function Has_Element (C: Cursor) return Boolean is
   begin
      if C.Element_A /= null then  -- Tengo que acceder al campo Element_A del cursor que cuenta el usuario, si es distinto de null es que apuntamos a algo
         return True;   -- Si esta a True digo que si estoy sobre un elemento
      else
         return False;  -- Si no es que esta a False
      end if;
   end Has_Element;

--------------------------------------------------------------------------------
   -- Raises No_Element if Has_Element(C) = False;
   function Element (C: Cursor) return Element_Type is
   begin
      if C.Element_A /= null then
         return (Key => C.Element_A.Key,
                 Value => C.Element_A.Value);
      else
         raise No_Element;
      end if;
   end Element;

end Maps_G_Hash;

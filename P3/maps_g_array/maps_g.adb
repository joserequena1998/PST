with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

package body Maps_G is


   procedure Get (M       : Map;
                  Key     : in  Key_Type;
                  Value   : out Value_Type;
                  Success : out Boolean) is
       I : Integer := 1;
    begin
       Success := False;
       while not Success and I < Max_Elements loop    -- Mientras no encontremos al usuario buscado y no lleguemos al total de usuarios:
          if M.P_Array(I).Full then -- Si el campo Full esta lleno buscamos a ver si es el nick buscado, si no avanzamos igualmente
 			    if M.P_Array(I).Key = Key then -- Si el nick es igual que el que buscamos hemos encontrado al usuario:
 				    Value := M.P_Array(I).Value;  -- Devolvemos el campo Value
 				    Success := True;     -- Y Success a true, lo hemos encontrado
 			    end if;
          end if;
          I := I + 1;   -- si no esta a full lo que hacemos es recorrer el array
       end loop;

    end Get;

--------------------------------------------------------------------------------
    procedure Put (M     : in out Map;
                   Key   : Key_Type;
                   Value : Value_Type) is
 	    I: Integer := 1;
       Success: Boolean := False;
    begin
       if M.Length = Max_Elements then    -- Si estamos a tope elevamos una excepcion
 	        raise Full_Map;
       end if;

       while not Success and I < Max_Elements loop -- Mientras no encontremos al usuario buscado y no lleguemos al total de usuarios:
          if M.P_Array(I).Full then -- Si esta llena la posicion
 		       if M.P_Array(I).Key = Key then  -- Vemos si la clave coincide y actualizamos su valor si es asi
 				    M.P_Array(I).Value := Value;
 				    Success := True;
 			    end if;
             -- No hace falta poner el I + 1 porque se va a actualiaar igualmente al salir del if
          else -- Si no esta llena solo hay que meter los campos nick y value y ponerlo a true el Full
             M.P_Array(I).Value := Value;
             M.P_Array(I).Key := Key;
             M.P_Array(I).Full := True;
             Success := True;
             M.Length := M.Length + 1;
          end if;
          I := I + 1;
       end loop;

    end Put;

--------------------------------------------------------------------------------
   procedure Delete (M      : in out Map;
                     Key     : in  Key_Type;
                     Success : out Boolean) is
      I: Integer := 1;
   begin
      Success := False; -- Inicializamos a False porque no hemos encontrado aun el cliente a borrar

      for I in 1..Max_Elements loop
         if not Success and M.P_Array(I).Full then -- solo debemos buscar si tiene la casilla de Full a True si no el cliente que hay ahi no es valido
            if M.P_Array(I).Key = Key then  -- Si la clave es igual a la que queremos borrar ahi que borrar ese cliente
               Success := True;   -- Hemos encontrado al que hay que borrar, luego Success es True
               M.P_Array(I).Full := False; -- Ponemos su casilla a False porque ese cliente ya no vale
               M.Length := M.Length - 1;   -- Reducimos el numero de clientes total
            end if;
         end if;
      end loop;

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
      I: Integer := 1;
      Success: Boolean;
      C: Cursor;
   begin
      Success := False;
      C.M := M;   -- Cuidado que tenemos que apuntar el cursor a nuestro Map, si no Null address, ya que no estariamos devolviendo nada

      while not Success and I <= Max_Elements loop -- Mientras no encontremos al usuario buscado y no lleguemos al total de usuarios:
         if M.P_Array(I).Full then  -- Si esta lleno el array es que ese es el que buscamos
            Success := True;
   		   C.Element_A := I;  -- El campo Element_A es el indice del array donde estamos
         end if;
         if M.Length = 0 then -- Hay que contemplar que la longitud del map sea 0, cuando la lista esta vacia
         	C.Element_A := 0;   -- No se devuelve indice alguno porque no hay nada
         	Success := True;
      	end if;
         I := I + 1;
      end loop;

      return C;
   end First;

--------------------------------------------------------------------------------
   procedure Next (C: in out Cursor) is
      I: Integer := 1;  -- I debe tener valor 1 que es el primer valor que tenemos
      Success: Boolean;
   begin
      Success := False;

      if C.Element_A = 0 then -- Si la lista esta vacia
      	Success := True; -- Decimos que hemos encontrado el siguiente, aunque en verdad es que esta a cero la lista y el primero seria el siguiente
      else
      	C.Element_A := C.Element_A + 1; -- si no, pasamos al siguiente
      end if;

      while not Success and C.Element_A <= Max_Elements loop
         if C.M.P_Array(C.Element_A).Full then  -- si el campo Full esta a True hemos llegado al siguiente
            Success := True;
         else
            C.Element_A := C.Element_A + 1;   -- si no seguimos
         end if;
      end loop;

      if not Success then  -- si no encontramos ninguno con el campo Full a True es que la lista esta vacia
         C.Element_A := 0;
      end if;

   end Next;

--------------------------------------------------------------------------------
   function Has_Element (C: Cursor) return Boolean is
   begin
      if C.Element_A /= 0 then  -- Tengo que acceder al campo Element_A del cursor que cuenta el usuario, si es distinto de cero es que hay
         return True;   -- Si esta a True digo que si estoy sobre un elemento
      else
         return False;  -- Si no es que esta a False
      end if;
   end Has_Element;


--------------------------------------------------------------------------------
   -- Raises No_Element if Has_Element(C) = False;
   function Element (C: Cursor) return Element_Type is
   begin
      if C.Element_A /= 0 then  -- si esta ocupado sacamos sus campos de value
         return (Key   => C.M.P_Array(C.Element_A).Key,
   				 Value => C.M.P_Array(C.Element_A).Value);
      else
         raise No_Element;
      end if;
   end Element;

end Maps_G;

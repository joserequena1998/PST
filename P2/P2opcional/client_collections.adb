with Ada.Strings.Unbounded;
with Lower_Layer_UDP;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Client_Collections;

package body Client_Collections is
   package T_IO renames Ada.Text_IO;

   use type ASU.Unbounded_String;
	use type Lower_Layer_UDP.End_Point_Type;
-- Esto es necesario para poder comparar EP y Unbounded_Strings

   procedure Free is new Ada.Unchecked_Deallocation(Cell, Cell_A);
-- Procedimiento para liberar memoria mas tarde

-------------------------------------------------------------------------------------------

   procedure Add_Client (Collection: in out Collection_Type;
                         EP: in LLU.End_Point_Type;
                         Nick: in ASU.Unbounded_String;
                         Unique: in Boolean) is
      P_Aux: Cell_A;
      Founded: Boolean;

   begin
      P_Aux := Collection.P_First;
      Founded := False;

      if Nick /= "reader" then   -- Si el nick no es reader no estamos ante un cliente lector
         while P_Aux /= Null loop   -- Nos recorremos la lista
            if P_Aux.Nick = Nick and Unique then  -- Si el nick coincide con alguno de los que hay en la lista ya se eleva excepcion
               raise Client_Collection_Error;
            end if;
            P_Aux := P_Aux.Next;    -- Avanzamos
         end loop;
      end if;

      P_Aux := Collection.P_First;
      Founded := False;
      while P_Aux /= Null and then not Founded loop    -- Vamos buscando en la lista
         if P_Aux.Client_EP = EP then  -- Si el EP es igual al EP que nos ha llegado para añadir el cliente
            Founded := True;
            P_Aux.Nick := Nick;  -- El nick que metemos es el que nos llega 
         end if;
         P_Aux := P_Aux.Next;
      end loop;

      if not Founded then  -- Si no se encuentra hay que crearlo
         P_Aux := new Cell'(EP, Nick, Null);    -- Creamos la nueva celda
         Collection.Total := Collection.Total + 1;
         if Collection.P_First = Null then   -- Si aun no habia nodos:
            Collection.P_First := P_Aux;  -- Se apunta P_First al nuevo creado
         else     -- Si ya habia nodos añado mi celda al principio, luego:
            P_Aux.Next := Collection.P_First;   -- El puntero del nuevo nodo apunta al que era primero antes
            Collection.P_First := P_Aux;  -- El puntero al primer nodo apunta al nuevo nodo creado
         end if;
      end if;

   end Add_Client;

----------------------------------------------------------------------------------------------

   procedure Delete_Client (Collection: in out Collection_Type;
                            Nick: in ASU.Unbounded_String) is
      P_Aux, P_Prev: Cell_A;
      Founded: Boolean;

   begin
      P_Aux := Collection.P_First;
      Founded := False;
      P_Prev := null;
      -- No hace falta inicializar P_Prev a null porque por defecto lo hace Ada y de momento no lo vamos a usar, pero me da un warning asi que lo hago

      while P_Aux /= Null and then not Founded loop   -- Mientras no apunte a null el nodo y no se haya encontrado
         if P_Aux.Nick = Nick then     -- Si el nick es igual es que hemos encontrado el que borrar
            Founded := True;
            if P_Aux = Collection.P_First then  -- Si coinciden el auxiliar y el que apunta al primero es que es el primer elemento de la lista el que hay que borrar
               Collection.P_First := P_Aux.Next;   -- Luego el primer elemento de la lista pasara a ser el siguiente que es al que apunta P_Aux.Next
            else
               P_Prev.Next := P_Aux.Next;    -- Si no coinciden es que no es el primero el que hay que borrar, luego el puntero de prevencion apunta al siguiente nodo del que estamos
            end if;
            Free(P_Aux);   -- Liberamos memoria
            Collection.Total := Collection.Total - 1;    -- Decrementamos el contador total
         else  -- Si no coincide seguimos recorriendo la lista
            P_Prev := P_Aux;  -- El puntero de prevencion apunta a donde estamos ahora
            P_Aux := P_Aux.Next;    -- El puntero que recorre la lista apunta al siguiente nodo
         end if;
      end loop;

      if not Founded then
         raise Client_Collection_Error;
      end if;

   end Delete_Client;

-----------------------------------------------------------------------------------------------

   function Search_Client (Collection: in Collection_Type;
                           EP: in LLU.End_Point_Type)
                           return ASU.Unbounded_String is
      P_Aux: Cell_A;
      Founded: Boolean;
      Nick: ASU.Unbounded_String;

   begin
      P_Aux := Collection.P_First;
      Founded := False;
      Nick := ASU.To_Unbounded_String(" ");

      while P_Aux /= Null and then not Founded loop   -- Mientras la lista no este vacia y no se encuentre el usuario por su EP
         if P_Aux.Client_EP = EP then  -- Si el EP al que apuntamos en ese momento es igual al que buscamos, lo tenemos
            Founded := True;
            Nick := P_Aux.Nick;     -- El nick a devolver en la funcion es ese, el buscado
         end if;
         P_Aux := P_Aux.Next;
      end loop;

      if not Founded then     -- Si no se encuentra nos piden que elevemos una excepcion
         raise Client_Collection_Error;
      end if;

      return (Nick);

   end Search_Client;

------------------------------------------------------------------------------------------------

   procedure Send_To_All (Collection: in Collection_Type;
                          P_Buffer: access LLU.Buffer_Type) is
      P_Aux: Cell_A;

   begin
      if(Collection.P_First = Null) then
			Null;
		end if;   -- Si la lista esta vacia no tiene que hacer nada, no mandar nada

      P_Aux := Collection.P_First;

      for K in 1..Collection.Total loop -- Para cada elemento del total de lectores
         if P_Aux.Nick = "reader" then    -- Solo se lo mando a los clientes con nick lector
            LLU.Send(P_Aux.Client_EP, P_Buffer);
         end if;
         P_Aux := P_Aux.Next;
      end loop;

   end Send_To_All;

------------------------------------------------------------------------------------------------

   function Collection_Image (Collection: in Collection_Type) return String is
      P_Aux: Cell_A;
      Line: ASU.Unbounded_String;
      IP: ASU.Unbounded_String;
      Port: ASU.Unbounded_String;
      ClientEP: ASU.Unbounded_String;  -- Cuidado no confundir con Client_EP que es el end point original
      TotalEP: ASU.Unbounded_String;     -- End point final tras sacar IP y puerto
      Pos: Natural;
   begin
      P_Aux := Collection.P_First;  -- Apuntamos P_Aux al primer nodo de la lista que es P_First

      if (Collection.P_First /= Null) then   -- Si el puntero que apunta al primer elemento estuviese a null es que no habria nada en la lista
         while (P_Aux /= Null) loop    -- Mientras que el puntero que usamos para apuntar a cada elemento no sea null vamos imprimiendo lo que haya
            ClientEP := ASU.To_Unbounded_String(LLU.Image(P_Aux.Client_EP));  -- Esto me pasa el End point a string, ahora hay que trocearlo
            Pos := ASU.Index(ClientEP,", Port: ");    -- Posicion donde encuentro mi puerto
            Port := ASU.Tail(ClientEP, ASU.Length(ClientEP) - Pos - 8);    -- Me quedo con el final, el 8 es lo que ocupa ", Port"
            ASU.Head(ClientEP, Pos - 1);     -- Ahora nos quedamos con la otra parte, donde esta la IP y demas
            Pos := ASU.Index(ClientEP," IP: ");    -- Buscamos el sitio donde esta mi IP
            IP := ASU.Tail(ClientEP, ASU.Length(ClientEP) - Pos - 4);   -- Ahora me quedo con el final para tener mi IP
            if (P_Aux.Nick /= "reader") then -- Solo si el nick es distinto de reader escribimos su nick y ep
               TotalEP := IP & ":" & Port & " " & P_Aux.Nick;
               Line := Line & TotalEP & ASCII.LF;
            end if;
            P_Aux := P_Aux.Next;
         end loop;
      else        -- Si el puntero apunta a null es que no hay lista, lo indicamos
         T_IO.Put_Line("No clients in list");
      end if;

      return ASU.To_String(Line);

   end Collection_Image;

--------------------------------------------------------------------------------------------------------

   function Total (Collection: in Collection_Type) return Natural is
      P_Aux: Cell_A;
      User_Num: Natural := 0;
   begin
      P_Aux := Collection.P_First;  -- Apuntamos P_Aux al primer nodo de la lista que es P_First

      if (Collection.P_First /= Null) then   -- Si el puntero que apunta al primer elemento estuviese a null es que no habria nada en la lista
         while (P_Aux /= Null) loop    -- Mientras que el puntero que usamos para apuntar a cada elemento no este vacio vamos sumando
            User_Num := User_Num + 1;     -- Si no es null a donde apuntamos aumentamos en 1 nuestro total de usuarios
				P_Aux := P_Aux.Next;
         end loop;
      else        -- Si el puntero apunta a null es que no hay lista, lo indicamos
         T_IO.Put_Line("No clients in list");
      end if;

      return (User_Num);

   end Total;

end Client_Collections;

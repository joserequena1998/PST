package body Ordered_Lists_G is
  function Is_Empty(List: in List_Type) return Boolean is
  begin
     if List = null then
        return True;
     else
        return False;
     end if;
  end Is_Empty;

--------------------------------------------------------------------------------
-- Se van añadiendo ordenando de menor a mayor: 1,2,3,6,9,12..
  procedure Add(List: in out List_Type; Element: in Element_Type) is
     Added: Boolean := False;
     P_New: List_Type;
     P_Aux, P_Aux2: List_Type;
  begin
     P_New := new Cell'(Element, Null);

     if Is_Empty(List) then   -- Si la lista esta vacia directamente el puntero que apunta al primero apunta al nuevo nodo
        List := P_New;
     else   -- Si no esta vacia
        if List.Next = null then -- Si el siguiente al primero es null es que solo hay uno
           if Element > List.Element then -- Si el elemento que metemos es mayor que el que hay:
             List.Next := P_New; -- El puntero al siguiente apunta al nuevo nodo
           else  -- Si no hay que meterlo antes del que teniamos
             P_New.Next := List; -- El puntero al siguiente del nuevo apunta al que era el ptrimero
             List := P_New;   -- el primero apunta ahora al nuevo nodo
            end if;
         else     -- Si List.Next no es null es que hay mas de uno
            P_Aux := List; -- P_Aux apunta al nodo mas atrasado
            P_Aux2 := List.Next; -- P_Aux2 apunta al nodo mas adelantado
            if not (Element > P_Aux.Element) then   -- si el elemento a meter es menor que el primero que hay se introduce en el primer lugar
               P_New.Next := P_Aux; -- El puntero al siguiente del nuevo apunta al que era el ptrimero
               List := P_New;   -- el primero apunta ahora al nuevo nodo
            else  -- Si el elemento a meter es mayor que el primero de la lista 
               while P_Aux2 /= Null and then (Element > P_Aux2.Element) loop    -- Si el mas adelantado no apunta a null y el elemento a insertar es mayor que el mas adelantado
                  P_Aux := P_Aux2;  -- El mas atrasado apunta al que era mas adelantado
                  P_Aux2 := P_Aux2.Next;  -- El mas adelantado pasa al siguiente
               end loop;
               if P_Aux2 /= null then  -- Si el mas adelantado no es null no hemos llegado al final, insertamos en medio
                  P_New.Next := P_Aux2;   -- El siguiente del nuevo nodo apunta al mas adelantado
                  P_Aux.Next := P_New;    -- El siguiente del mas atrasado apunta al nuevo nodo
               else  -- Si el mas adelantado es null tenemos que ponerlo en el final
                  P_Aux.Next := P_New;    -- P_Aux era el ultimo que apuntaba a algo, y ahora apunto su siguiente al nuevo, ya qu elo añado al final
               end if;
            end if;
         end if;
     end if;

  end Add;

--------------------------------------------------------------------------------

  function Get_First(List: in List_Type) return Element_Type is
  begin
    if not Is_Empty(List)  then
      return List.Element;
    else
      raise Empty_List;
    end if;
  end Get_First;

--------------------------------------------------------------------------------

  procedure Free is new Ada.Unchecked_Deallocation(Cell, List_Type);
  procedure Delete_First(List: in out List_Type) is
    P_Aux: List_Type;
  begin
    if not Is_Empty(List) then
      P_Aux := List;
      List := List.Next;
      Free(P_Aux);
    else
      raise Empty_List;
    end if;
  end Delete_First;

end Ordered_Lists_G;

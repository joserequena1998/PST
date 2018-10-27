with Chat_Messages;
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Lower_Layer_UDP;
with Ada.Calendar;
with Ada.Exceptions;
with Gnat.Calendar.Time_IO;

package body Server_Handler is
  function Unbounded_String_Hash(US: ASU.Unbounded_String) return Hash_Range is
    Total: Natural;
    C: Character;
  begin
    Total := 0;
    for I in 1..ASU.Length(US) loop
      C := ASU.Element(US, I);
      Total := Total + Character'Pos(C);
    end loop;
    return Hash_Range'Mod(Total);
  end Unbounded_String_Hash;

   function Time_Image (T: Ada.Calendar.Time) return String is
   begin
      return Gnat.Calendar.Time_IO.Image(T, "%d-%b-%y %T.%i");
   end Time_Image;

   function Imprimir_LLU (Client_EP: ASU.Unbounded_String) return String is
      EP, IP, Port, TotalEP: ASU.Unbounded_String;
      Pos: Natural;
   begin
      EP := Client_EP;  -- Si no hago esto me da fallo en el Head
      Pos := ASU.Index(EP,", Port: ");    -- Posicion donde encuentro mi puerto
      Port := ASU.Tail(EP, ASU.Length(EP) - Pos - 8);    -- Me quedo con el final, el 8 es lo que ocupa ", Port"
      ASU.Head(EP, Pos - 1);     -- Ahora nos quedamos con la otra parte, donde esta la IP y demas
      Pos := ASU.Index(EP," IP: ");    -- Buscamos el sitio donde esta mi IP
      IP := ASU.Tail(EP, ASU.Length(EP) - Pos - 4);   -- Ahora me quedo con el final para tener mi IP
      TotalEP := IP & ":" & Port & ASCII.LF;
      return ASU.To_String(TotalEP);
   end Imprimir_LLU;

   procedure Print_Map_Activos (M : Clientes_On.Map) is
      C: Clientes_On.Cursor := Clientes_On.First(M);
      Client_EP: ASU.Unbounded_String;
   begin
      --T_IO.Put_Line ("Pasa al printmap");
      T_IO.Put_Line ("Map");
      T_IO.Put_Line ("===");
      -- C := Clientes_On.First(M); por que esto da error?
      while Clientes_On.Has_Element(C) loop
         Client_EP := ASU.To_Unbounded_String(LLU.Image(Clientes_On.Element(C).Value.EP));
         T_IO.Put_Line ("Client: " & ASU.To_String(Clientes_On.Element(C).Key) & "; " &
                        "Last time activity: " & Time_Image(Clientes_On.Element(C).Value.Time) & "; " &
                        "Client EP: " & Imprimir_LLU(Client_EP));
         Clientes_On.Next(C);
      end loop;
   end Print_Map_Activos;

   procedure Print_Map_Angituos (M : Clientes_Off.Map) is
      C: Clientes_Off.Cursor := Clientes_Off.First(M);
   begin
      T_IO.Put_Line ("Map");
      T_IO.Put_Line ("===");
      -- C := Clientes_Off.First(M);
      while Clientes_Off.Has_Element(C) loop
         T_IO.Put_Line (ASU.To_String(Clientes_Off.Element(C).Key) & " " &
                        Time_Image(Clientes_Off.Element(C).Value));    -- Hago la imagen del EP
         Clientes_Off.Next(C);
      end loop;
   end Print_Map_Angituos;

   procedure Send_To_All (M: in Clientes_On.Map;
                           Nick: in ASU.Unbounded_String;
                           P_Buffer: access LLU.Buffer_Type) is
      Cliente: Clientes_On.Element_Type;
      C: Clientes_On.Cursor := Clientes_On.First(M);
   begin
      --T_IO.Put_Line("Pasa al Send_To_All");
      --C := Clientes_On.First(M); -- Apunto el cursor al primer usuario
      if Clientes_On.Has_Element(C) then  -- Si hay clientes activos les mando el mensaje
         loop
            Cliente := Clientes_On.Element(C);  -- Apunto Cliente al usuario en el que estoy
            if Cliente.Key = Nick then    -- Si el usuario tiene el mismo nick que el que manda el mensaje soy yo y no me lo envio a mi mismo
               Clientes_On.Next(C);
            else        -- Si no si lo mando
               LLU.Send(Cliente.Value.EP, P_Buffer);
               Clientes_On.Next(C);
            end if;
            exit when not Clientes_On.Has_Element(C); -- Nos salimos cuando no haya mas elementos
         end loop;
      else     -- Si no hay elementos en la lista elevo una excepcion
         begin
            raise No_Elements; -- Si hago esto tengo que hacer el trato de excepciones

         exception
            when No_Elements =>
               T_IO.Put_Line("At this moment, the active clients list is empty");
         end;
      end if;
   end Send_To_All;


   procedure Throw_Out_One (M: Clientes_On.Map; P_Buffer: access LLU.Buffer_Type; Cliente_Elegido:  out Clientes_On.Element_Type) is
      C: Clientes_On.Cursor := Clientes_On.First(M);      -- Pongo el iterador en el primer elemento
      Cliente_Siguiente: Clientes_On.Element_Type;
      Success: Boolean;
      Mensaje, Nick : ASU.Unbounded_String;
   begin
      --C := Clientes_On.First(M);      -- Pongo el iterador en el primer elemento, aqui me da error
      if Clientes_On.Has_Element(C) then       -- Si la lista no esta vacia buscamos a quien eliminar
         Cliente_Elegido := Clientes_On.Element(C);   -- El cliente elegido de momento es el primero
         Clientes_On.Next(C); -- Paso al siguiente
         Cliente_Siguiente := Clientes_On.Element(C);    -- Cojo el siguiente para compararlo
         loop
            if Cliente_Elegido.Value.Time < Cliente_Siguiente.Value.Time then     -- Comparo el cliente elegido con el que le sigue, si es mayor
               -- Cliente_Elegido no me cambia ya que sigue siendo el mismo
               Clientes_On.Next(C); -- Corro el cursor para ver el siguiente
               if Clientes_On.Has_Element(C) then  -- Si el siguiente tiene algun elemento:
                  Cliente_Siguiente := Clientes_On.Element(C); -- Apunto el cursor al siguiente cliente
               end if;
            else     -- Si el del siguiente es mayor es que lleva mas tiempo inactivo
               Cliente_Elegido := Cliente_Siguiente; -- El elegido es el siguiente
               Clientes_On.Next(C); -- Corro el cursor para ver el siguiente
               if Clientes_On.Has_Element(C) then  -- Si el siguiente tiene algun elemento:
                  Cliente_Siguiente := Clientes_On.Element(C); -- Apunto el cursor al siguiente cliente
               end if;
            end if;
            exit when not Clientes_On.Has_Element(C); -- Nos salimos cuando el siguiente nodo sea cero
         end loop;
      end if;
      Clientes_On.Delete(Clientes_Activos, Cliente_Elegido.Key, Success);   -- Debo borrar de clientes activos el elegido
      Clientes_Off.Put(Clientes_Antiguos, Cliente_Elegido.Key, Cliente_Elegido.Value.Time); -- Y lo añado a los clientes antiguos
   end Throw_Out_One;

   procedure Handler_S (From: in LLU.End_Point_Type;
                        To: in LLU.End_Point_Type;
                        P_Buffer: access LLU.Buffer_Type) is
      Max_Clients: Natural;
      Mess_Type: CM.Message_Type;
      Nick, Mensaje, Nick_Banned: ASU.Unbounded_String;
      Values: Value_Record;
      Client_EP_Receive, Client_EP_Handler: LLU.End_Point_Type;
      Success, Acogido: Boolean;
      Time_Value: AC.Time;
      Cliente_Elegido: Clientes_On.Element_Type;

   begin
      Max_Clients := Natural'Value(ACL.Argument(2));  -- Veo cual es el maximo numero de clientes que meto

      Mess_Type := CM.Message_Type'Input(P_Buffer);  -- Segun el tipo de mensaje puedo tener tres opciones: Init, Writer o Logout

      if Mess_Type = CM.Init then
         -- Primero debemos comprobar que el numero de clientes que hay no sea el tope
         if Clientes_On.Map_Length(Clientes_Activos) < Max_Clients then -- Si el numero de clientes activos es menor que el tope no pasa nada debemos buscar si esta ya en la lista => Get
            Client_EP_Receive := LLU.End_Point_Type'Input(P_Buffer);
				Client_EP_Handler := LLU.End_Point_Type'Input(P_Buffer); -- Los EPs no los necesito aun pero debo sacarlos para poder sacar el nick que va al final
				Nick := ASU.Unbounded_String'Input(P_Buffer);
            Clientes_On.Get(Clientes_Activos, Nick, Values, Success);
            if Success then   -- Si lo hemos encontrado en la lista de activos Success vale true y ya hay un usuario usando ese nick
               Acogido := False;
               LLU.Reset(P_Buffer.all);  -- Cuidado que tengo que resetear a lo que apunta P_Buffer que es un puntero a mi buffer
               CM.Message_Type'Output(P_Buffer, CM.Welcome); -- Le mando el Welcome diciendole que no es bienvenido
               Boolean'Output(P_Buffer, Acogido);
               LLU.Send(Client_EP_Receive, P_Buffer); -- Cuidado que hay que mandarlo al ep receive
               T_IO.Put_Line("INIT received from " & ASU.To_String(Nick) & ".IGNORED, nick already used");
            else     -- Si no lo hemos encontrado hay que añadirlo a l lalista
               Values.EP := Client_EP_Handler;
               Values.Time := AC.Clock;
               Clientes_On.Put(Clientes_Activos, Nick, Values);   -- Values esta formado por nuestros dos campos de Value_Record

               Acogido := True;
               LLU.Reset(P_Buffer.all);  -- Primero hay que enviar el Welcome a Client_EP_Receive
					CM.Message_Type'Output(P_Buffer, CM.Welcome);
					Boolean'Output(P_Buffer, Acogido);
					LLU.Send(Client_EP_Receive, P_Buffer);
               T_IO.Put_Line("INIT received from " & ASU.To_String(Nick) & ": ACCEPTED");

               LLU.Reset(P_Buffer.all);   -- Reseteamos para mandar el Server informando del nuevo cliente
               CM.Message_Type'Output(P_Buffer, CM.Server);
               Mensaje := ASU.To_Unbounded_String("server");
               ASU.Unbounded_String'Output(P_Buffer, Mensaje);
               ASU.Unbounded_String'Output(P_Buffer, Nick & " joins the chat");

               -- Aqui debo enviar el mensaje server a todos los clientes activos
               Send_To_All(Clientes_Activos, Nick, P_Buffer);
            end if;
         else     -- Si el numero de clientes activos no es menor es que no podemos soportar mas clients y hay que echar a uno
            Client_EP_Receive := LLU.End_Point_Type'Input(P_Buffer);
				Client_EP_Handler := LLU.End_Point_Type'Input(P_Buffer); -- Los EPs no los necesito aun pero debo sacarlos para poder sacar el nick que va al final
				Nick := ASU.Unbounded_String'Input(P_Buffer);
            Clientes_On.Get(Clientes_Activos, Nick, Values, Success);
            Throw_Out_One(Clientes_Activos, P_Buffer, Cliente_Elegido);

            -- Este es el server que le envio al usuario baneado
            LLU.Reset(P_Buffer.all);
            CM.Message_Type'Output(P_Buffer, CM.Server);
            Mensaje := ASU.To_Unbounded_String("server");
            ASU.Unbounded_String'Output(P_Buffer, Mensaje);
            ASU.Unbounded_String'Output(P_Buffer, ASU.To_Unbounded_String("You are banned for being idle too long. You must leave this chat"));
            LLU.Send(Cliente_Elegido.Value.EP, P_Buffer);

            -- Este es el server que le envio al resto de usuarios informandoles
            LLU.Reset(P_Buffer.all);
            CM.Message_Type'Output(P_Buffer, CM.Server);
            Mensaje := ASU.To_Unbounded_String("server");
            ASU.Unbounded_String'Output(P_Buffer, Mensaje);
            Nick_Banned := Cliente_Elegido.Key;
            ASU.Unbounded_String'Output(P_Buffer, Nick_Banned & " banned for being idle too long");
            Send_To_All(Clientes_Activos, Nick_Banned, P_Buffer);

            T_IO.Put_Line("User " & ASU.To_String(Nick_Banned) & " has been banned for being idle too long");

            if not Success then
               Values.EP := Client_EP_Handler;
               Values.Time := AC.Clock;
               Clientes_On.Put(Clientes_Activos, Nick, Values);   -- Values esta formado por nuestros dos campos de Value_Record

               Acogido := True;
               LLU.Reset(P_Buffer.all);  -- Primero hay que enviar el Welcome a Client_EP_Receive
               CM.Message_Type'Output(P_Buffer, CM.Welcome);
               Boolean'Output(P_Buffer, Acogido);
               LLU.Send(Client_EP_Receive, P_Buffer);
               T_IO.Put_Line("INIT received from " & ASU.To_String(Nick) & ": ACCEPTED");

               LLU.Reset(P_Buffer.all);   -- Reseteamos para mandar el Server informando del nuevo cliente
               CM.Message_Type'Output(P_Buffer, CM.Server);
               Mensaje := ASU.To_Unbounded_String("server");
               ASU.Unbounded_String'Output(P_Buffer, Mensaje);
               ASU.Unbounded_String'Output(P_Buffer, Nick & " joins the chat");

               -- Aqui debo enviar el mensaje server a todos los clientes activos
               Send_To_All(Clientes_Activos, Nick, P_Buffer);
            end if;

         end if;
      elsif Mess_Type = CM.Writer then
         Client_EP_Handler := LLU.End_Point_Type'Input(P_Buffer);
         Nick := ASU.Unbounded_String'Input(P_Buffer);
         Mensaje := ASU.Unbounded_String'Input(P_Buffer);   -- Saco los campos y
         Clientes_On.Get(Clientes_Activos, Nick, Values, Success);   -- Miro a ver si esta en mi lista de activos
         if Success then   -- Si esta debo enviar el mensaje a todos los demas y actualizar su campo value con EP pero sobre todo por la hora de envio del mensaje
            Values.EP := Client_EP_Handler;
            Values.Time := AC.Clock;   -- Lo mas importante es actualizar este que es el determinante a l ahora de banear
            Clientes_On.Put(Clientes_Activos, Nick, Values);   -- Values esta formado por nuestros dos campos de Value_Record

            LLU.Reset(P_Buffer.all);
            CM.Message_Type'Output(P_Buffer, CM.Server);
            ASU.Unbounded_String'Output(P_Buffer, Nick);
            ASU.Unbounded_String'Output(P_Buffer, Mensaje);
            Send_To_All(Clientes_Activos, Nick, P_Buffer);
            T_IO.Put_Line(ASU.To_String("WRITER received from " & Nick & ": " & Mensaje));
         else     -- Si no esta lo añado a la li sta de antiguos/no activos
            Time_Value := AC.Clock;    -- El campo que guardo junto al nick es la hora cuando mandaron algo por ult vez
            Clientes_Off.Put(Clientes_Antiguos, Nick, Time_Value);   -- Lo añado a clientes antiguos/no activos
            T_IO.Put_Line("WRITER received from unknown client. IGNORED");
         end if;
      elsif Mess_Type = CM.Logout then
         Client_EP_Handler := LLU.End_Point_Type'Input(P_Buffer);
         Nick := ASU.Unbounded_String'Input(P_Buffer);
         Clientes_On.Get(Clientes_Activos, Nick, Values, Success);   -- Buscamos el cliente en la lista de activos
         if Success then   -- Si lo encontramos debemos borrar de activos y pasar a antiguos
            Clientes_On.Delete(Clientes_Activos, Nick, Success);   -- Borramos de activos al cliente
            Time_Value := AC.Clock;    -- El campo que guardo junto al nick es la hora cuando mandaron algo por ult vez
            Clientes_Off.Put(Clientes_Antiguos, Nick, Time_Value);

            Mensaje := ASU.To_Unbounded_String("server");
            LLU.Reset(P_Buffer.all);      -- Le mandamos el LOGOUT a los clientres lectores como un mensaje SERVER
            CM.Message_Type'Output(P_Buffer, CM.Server);
            ASU.Unbounded_String'Output(P_Buffer, Mensaje);
            ASU.Unbounded_String'Output(P_Buffer, Nick & " leaves the chat");
            T_IO.Put_Line(ASU.To_String("LOGOUT received from " & Nick));  -- Imprimimos que hay un LOGOUT
            Send_To_All(Clientes_Activos, Nick, P_Buffer);
         else
            T_IO.Put_Line("LOGOUT received from unknown client. IGNORED");
         end if;
      end if;
   end Handler_S;

end Server_Handler;

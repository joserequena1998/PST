with Ada.Text_IO;
With Ada.Strings.Unbounded;
with Ordered_Maps_G;

procedure Ordered_Maps_Test is
  package ASU  renames Ada.Strings.Unbounded;
  package ATIO renames Ada.Text_IO;

  package Ordered_Maps is new Ordered_Maps_G (Key_Type   => ASU.Unbounded_String,
    Value_Type => ASU.Unbounded_String,
    "=" => ASU."=",
    "<" => ASU."<",
    Max_Elements => 150);

  Value   : ASU.Unbounded_String;
  Success : Boolean;

  A_Map : Ordered_Maps.Map;

  procedure Print_Map (M : Ordered_Maps.Map) is
    C: Ordered_Maps.Cursor := Ordered_Maps.First(M);
  begin
    ATIO.Put_Line ("Map");
    ATIO.Put_Line ("===");

    while Ordered_Maps.Has_Element(C) loop
      ATIO.Put_Line (ASU.To_String(Ordered_Maps.Element(C).Key) & " " &
      ASU.To_String(Ordered_Maps.Element(C).Value));
      Ordered_Maps.Next(C);
    end loop;
  end Print_Map;

begin
  ATIO.New_Line;
  ATIO.Put_Line ("Longitud de la tabla de símbolos: " &
  Integer'Image(Ordered_Maps.Map_Length(A_Map)));
  Print_Map (A_Map);

  Ordered_Maps.Put (A_Map,
  ASU.To_Unbounded_String ("www.urjc.es"),
  ASU.To_Unbounded_String ("212.128.240.25"));

  ATIO.New_Line;
  ATIO.Put_Line ("Longitud de la tabla de símbolos: " &
  Integer'Image(Ordered_Maps.Map_Length(A_Map)));
  Print_Map(A_Map);

  ATIO.New_Line;
  Ordered_Maps.Get (A_Map, ASU.To_Unbounded_String ("www.urjc.es"), Value, Success);
  if Success then
    ATIO.Put_Line ("Get: Dirección IP de www.urjc.es: " &
    ASU.To_String(Value));
  else
    ATIO.Put_Line ("Get: NO hay una entrada para la clave www.urjc.es");
  end if;

  Ordered_Maps.Put (A_Map, ASU.To_Unbounded_String ("google.com"),
  ASU.To_Unbounded_String ("66.249.92.104"));

  ATIO.New_Line;
  ATIO.Put_Line ("Longitud de la tabla de símbolos: " &
  Integer'Image(Ordered_Maps.Map_Length(A_Map)));
  Print_Map(A_Map);

  Ordered_Maps.Put (A_Map,
  ASU.To_Unbounded_String ("facebook.com"),
  ASU.To_Unbounded_String ("69.63.189.16"));

  ATIO.New_Line;
  ATIO.Put_Line ("Longitud de la tabla de símbolos: " &
  Integer'Image(Ordered_Maps.Map_Length(A_Map)));
  Print_Map(A_Map);

  Ordered_Maps.Put (A_Map,
  ASU.To_Unbounded_String ("facebook.com"),
  ASU.To_Unbounded_String ("69.63.189.11"));

  ATIO.New_Line;
  ATIO.Put_Line ("Longitud de la tabla de símbolos: " &
  Integer'Image(Ordered_Maps.Map_Length(A_Map)));
  Print_Map(A_Map);

  Ordered_Maps.Put (A_Map,
  ASU.To_Unbounded_String ("apple.com"),
  ASU.To_Unbounded_String ("17.142.160.59"));

  ATIO.New_Line;
  ATIO.Put_Line ("Longitud de la tabla de símbolos: " &
  Integer'Image(Ordered_Maps.Map_Length(A_Map)));
  Print_Map(A_Map);

  Ordered_Maps.Put (A_Map,
  ASU.To_Unbounded_String ("zalando.com"),
  ASU.To_Unbounded_String ("130.211.9.113"));

  ATIO.New_Line;
  ATIO.Put_Line ("Longitud de la tabla de símbolos: " &
  Integer'Image(Ordered_Maps.Map_Length(A_Map)));
  Print_Map(A_Map);

  ATIO.New_Line;
  Ordered_Maps.Get (A_Map, ASU.To_Unbounded_String ("www.urjc.es"), Value, Success);
  if Success then
    ATIO.Put_Line ("Get: Dirección IP de www.urjc.es: " &
    ASU.To_String(Value));
  else
    ATIO.Put_Line ("Get: NO hay una entrada para la clave www.urjc.es");
  end if;

  ATIO.New_Line;
  Ordered_Maps.Delete (A_Map, ASU.To_Unbounded_String("google.com"), Success);
  if Success then
    ATIO.Put_Line ("Delete: BORRADO google.com");
  else
    ATIO.Put_Line ("Delete: google.com no encontrado");
  end if;

  ATIO.New_Line;
  ATIO.Put_Line ("Longitud de la tabla de símbolos: " &
  Integer'Image(Ordered_Maps.Map_Length(A_Map)));
  Print_Map(A_Map);

  ATIO.New_Line;
  Ordered_Maps.Delete (A_Map, ASU.To_Unbounded_String("www.urjc.es"), Success);
  if Success then
    ATIO.Put_Line ("Delete: BORRADO www.urjc.es");
  else
    ATIO.Put_Line ("Delete: www.urjc.es no encontrado");
  end if;

  ATIO.New_Line;
  ATIO.Put_Line ("Longitud de la tabla de símbolos: " &
  Integer'Image(Ordered_Maps.Map_Length(A_Map)));
  Print_Map (A_Map);

  ATIO.New_Line;
  Ordered_Maps.Delete (A_Map, ASU.To_Unbounded_String("bbb.bbb.bbb"), Success);
  if Success then
    ATIO.Put_Line ("Delete: BORRADO bbb.bbb.bbb");
  else
    ATIO.Put_Line ("Delete: bbb.bbb.bbb no encontrado");
  end if;

  ATIO.New_Line;
  ATIO.Put_Line ("Longitud de la tabla de símbolos: " &
  Integer'Image(Ordered_Maps.Map_Length(A_Map)));
  Print_Map (A_Map);
end Ordered_Maps_Test;

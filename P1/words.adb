with Ada.Strings.Unbounded;
with Word_Lists;
with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Strings.Maps;
with Ada.Characters.Handling;

use Word_Lists;

procedure Words is
   package T_IO renames Ada.Text_IO;
   package ACL renames Ada.Command_Line;
   package ASU renames Ada.Strings.Unbounded;

   procedure Print_Menu is
   begin
      T_IO.New_Line(1);
      T_IO.Put_Line("Options");
      T_IO.Put_Line("1 Add word");
      T_IO.Put_Line("2 Delete word");
      T_IO.Put_Line("3 Search word");
      T_IO.Put_Line("4 Show all words");
      T_IO.Put_Line("5 Quit");
      T_IO.Put_Line("6 Delete List");
      T_IO.New_Line;
      T_IO.Put("Your option? ");
   end Print_Menu;

----------------------------------------------------------------------------------------
   procedure Tramitar_Opcion(List: in out Word_Lists.Word_List_Type; Opcion: Integer) is
      Word: ASU.Unbounded_String;
      Count: Natural;
   begin
      case Opcion is
         when 1 =>
            T_IO.Put("Word? ");
            Word := ASU.To_Unbounded_String(T_IO.Get_Line);
            Word_Lists.Add_Word(List, Word);
            T_IO.Put_Line("Word |" & ASU.To_String(Word) & "| added");
         when 2 =>
            T_IO.Put("Word? ");
            Word := ASU.To_Unbounded_String(T_IO.Get_Line);
            Word_Lists.Delete_Word(List, Word);
            T_IO.Put_Line("Word |" & ASU.To_String(Word) & "| deleted");
         when 3 =>
            T_IO.Put("Word? ");
            Word := ASU.To_Unbounded_String(T_IO.Get_Line);
            Word_Lists.Search_Word(List, Word, Count);
            T_IO.Put_Line("|" & ASU.To_String(Word) & "| - " & Natural'Image(Count));
         when 4 => Word_Lists.Print_All(List);
         when 5 => Null;
         when 6 => Word_Lists.Delete_List(List);
         when others => T_IO.Put_Line("Option must be 1 - 6");
      end case;

      T_IO.New_Line;

   end Tramitar_Opcion;

----------------------------------------------------------------------------------------------
   procedure Get_Words_From_Line(List:in out Word_Lists.Word_List_Type; Line: in ASU.Unbounded_String) is
        LineLeft: ASU.Unbounded_String;
        LineLenght: Natural;
        Pos : Natural;
        Finished: Boolean;

   begin
        LineLeft := Line;
        LineLenght := ASU.Length(LineLeft);
        Finished := False;

		   while LineLenght > 0 and not Finished loop
		      Pos := ASU.Index(LineLeft, Ada.Strings.Maps.To_Set(" ,.-")); -- Con la extension 1, sin la extension seria solo Pos := ASU.Index(LineLeft, " ");

		      if Pos = 1 then
		         ASU.Tail(LineLeft, LineLenght - 1);
		         LineLenght := LineLenght - 1;
		      elsif Pos = 0 then
		         Word_Lists.Add_Word(List, LineLeft);
		         Finished := True;
		      else
		         Word_Lists.Add_Word(List, ASU.Head(LineLeft, Pos - 1));
		         ASU.Tail(LineLeft, LineLenght - Pos);
		         LineLenght := LineLenght - Pos;
		      end if;

		   end loop;

   end Get_Words_From_Line;

------------------------------------------------------------------------------------------
   procedure Get_All_Words(List: in out  Word_Lists.Word_List_Type; File_Name: in ASU.Unbounded_String) is
        File: Ada.Text_IO.File_Type;
        Finish: Boolean;
        Line: ASU.Unbounded_String;

   begin
   	Ada.Text_IO.Open(File, Ada.Text_IO.In_File, ASU.To_String(File_Name));
   	Finish := False;
   	while not Finish loop
      begin
	 	Line := ASU.To_Unbounded_String(Ada.Characters.Handling.To_Lower(Ada.Text_IO.Get_Line(File)));   -- Con la extension 2, sin ella seria Line := ASU.To_Unbounded_String(Ada.Text_IO.Get_Line(File));
	 	Get_Words_From_Line(List, Line);
      exception
	 	when Ada.IO_Exceptions.End_Error =>
	    	Finish := True;
      end;
     end loop;

   Ada.Text_IO.Close(File);

   end Get_All_Words;

----------------------------------------------------------------------------------------------
   procedure Comprobar_Opciones(Opcion: in String; MostrarLista:in out Boolean; ModoInteractivo:in out Boolean) is
        Usage_Error: exception;

   begin
      if Opcion = "-i" then
         ModoInteractivo := True;
      elsif Opcion = "-l" then
         MostrarLista := True;
      else
         raise  Usage_Error;
      end if;
   end Comprobar_Opciones;


   Usage_Error: exception;
   File_Name: ASU.Unbounded_String;

   List : Word_List_Type;
   Word: ASU.Unbounded_String;
   Count: Natural;
   MostrarLista, ModoInteractivo: Boolean;
   OpcionMenu: Integer;

begin

   List := Null;
   MostrarLista := False;
   ModoInteractivo := False;

   if ACL.Argument_Count = 1 then
      File_Name := ASU.To_Unbounded_String(ACL.Argument(1));
   elsif ACL.Argument_Count = 2 then
      File_Name := ASU.To_Unbounded_String(ACL.Argument(2));
      Comprobar_Opciones(ACL.Argument(1), MostrarLista, ModoInteractivo);
   elsif ACL.Argument_Count = 3 then
      Comprobar_Opciones(ACL.Argument(1), MostrarLista, ModoInteractivo);
      Comprobar_Opciones(ACL.Argument(2), MostrarLista, ModoInteractivo);
      File_Name := ASU.To_Unbounded_String(ACL.Argument(3));
   else
      raise Usage_Error;
   end if;

   Get_All_Words(List, File_Name);

   if MostrarLista then
      Word_Lists.Print_All(List);
   end if;


   if ModoInteractivo then
      loop
         Print_Menu;
         OpcionMenu := Integer'Value(T_IO.Get_Line);
         Tramitar_Opcion(List, OpcionMenu);
         exit when OpcionMenu = 5;
      end loop;
   end if;

   Word_Lists.Max_Word(List, Word, Count);
   T_IO.New_Line;
   T_IO.Put_Line("The most frequent word: |" & ASU.To_String(Word) & "| - " & Natural'Image(Count));
   T_IO.New_Line;


exception
   when Usage_Error =>
      Ada.Text_IO.Put_Line("Use: ");
      Ada.Text_IO.Put_Line("       " & ACL.Command_Name & "[-i] [-l] <file>");

end Words;

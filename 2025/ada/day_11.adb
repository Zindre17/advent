with Ada.Strings.Hash;
with Ada.Strings;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Vectors;

with Ada.Text_IO; use Ada.Text_IO;

procedure Day_11 is
  package Exceptions is
    E : exception;
  end;
  use Exceptions;

  subtype Name_Type is String(1 .. 3);
  package String_Vectors is new
    Ada.Containers.Vectors
      (Index_Type => Natural,
       Element_Type => Name_Type);

  subtype String_Vector is String_Vectors.Vector;

  package String_Vector_Vectors is new
    Ada.Containers.Vectors
      (Index_Type => Natural,
       Element_Type => String_Vector,
       "=" => String_Vectors."=");

  subtype String_Vector_Vector is String_Vector_Vectors.Vector;

  package Strings_Hashed_Maps is new
    Ada.Containers.Indefinite_Hashed_Maps
      (Key_Type => String,
       Element_Type => String_Vector,
       Hash => Ada.Strings.Hash,
       Equivalent_Keys => "=",
       "=" => String_Vectors."=");

  subtype Strings_Map is Strings_Hashed_Maps.Map;

  type Node_Type is record
    Name : Name_Type;
    Connections : String_Vector;
  end record;
 
  function Parse_Node(Line : String) return Node_Type is
    function Take_Name(Offset : Integer) return Name_Type is
      Start : Integer := Line'First + Offset;
    begin
      return Line(Start .. Start + 2);
    end;

    Name : String := Take_Name(0);
    Connection_Count : Integer := (Line'Length - 4) / 4;
    Connections : String_Vector;
  begin
    for I in 0 .. Connection_Count - 1 loop
      Connections.Append(Take_Name(5 + (4 * I)));
    end loop;

    return (Name, Connections);
  end;

  Input_Filename : constant String := "day11.txt";
  Input_File : File_Type;

  Connections : Strings_map;
  Node : Node_Type;
  Paths : String_Vector_Vector;

  function Remaining_Paths return Boolean is
  begin
    for Path of Paths loop
      if Path.Last_Element /= "out" then
        return True;
      end if;
    end loop;
    return False;
  end;

  function Resume_Unfinished_Path return String_Vector is
  begin
    for Path of Paths loop
      if Path.Last_Element /= "out" then
        return Path;
      end if;
    end loop;
    raise E;
  end;

  function Get_Current_Position(Path : String_Vector) return Name_Type is
  begin
    return Path.Last_Element;
  end;

  function Clone(V : String_Vector) return String_Vector is
    C : String_Vector;
  begin
    for E of V loop
      C.Append(E);
    end loop;
    return C;
  end;

  procedure Put_Line(V :String_Vector) is
    Index : Natural := 0;
  begin
    for E of V loop
      if Index /= 0 then
        Put(", ");
      end if;
      Put(E);
      Index := Index + 1;
    end loop;
    New_Line;
  end;
begin
  Open(Input_File, In_File, Input_Filename);
  while not End_Of_File(Input_File) loop
    Node := Parse_Node(Get_Line(Input_File));
    Connections.Include(Node.Name, Node.Connections);
  end loop;
  Close(Input_File);

  Node := ("you", Connections("you"));
  declare
    Path : String_Vector;
  begin
    Path.Append("you");
    Paths.Append(Path);
  end;

  while Remaining_Paths loop
    declare
      Path : String_Vector := Resume_Unfinished_Path;
      Current_Position : Name_Type := Get_Current_Position(Path);
    begin
      Paths.Delete(String_Vector_Vectors.Find_Index(Paths, Path));
      Put("Current path: ");
      Put_Line(Path);
      for Option of Connections(Current_Position) loop
        Put_Line(" - Option: " & Option);
        declare
          New_Path : String_Vector := Clone(Path);
        begin
          New_Path.Append(Option);
          Paths.Append(New_Path);
        end;
      end loop;
    end;
  end loop;

  for Path of Paths loop
    Put_Line(Path);
  end loop;

  Put_Line("Path that lead from 'you' to 'out':" & Paths.Length'Image);
end;

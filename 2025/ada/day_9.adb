with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Containers.Vectors;
use Ada.Containers;

procedure Day_9 is
  type Long is mod 2 ** 64;
  type Point_Record is record
    X : Integer;
    Y : Integer;
  end record;

  package Point_Vectors is new Vectors(Positive, Point_Record);

  Input_File_Name : constant String := "day9.txt";
  Input_File : File_Type;

  Points : Point_Vectors.Vector;
  Largest_Area : Long := 0;

  function Parse_Point(Line : String) return Point_Record is
    Split_Index : Integer := Index(Line, ",");
    X           : Integer := Integer'Value(Line(Line'First .. Split_Index - 1));
    Y           : Integer := Integer'Value(Line(Split_Index + 1 .. Line'Last));
  begin
    return (X,Y);
  end;

  function Calc_Area_Between_Points(A, B : Point_Record) return Long is
    DX : Integer := Abs(A.X - B.X) + 1;
    DY : Integer := Abs(A.Y - B.Y) + 1;
  begin
    return Long(DX) * Long(DY);
  end;
begin
  Open(Input_File, In_File, Input_File_Name);

  while not End_Of_File(Input_File) loop
    Points.Append(Parse_Point(Get_Line(Input_File)));
  end loop;

  for A of Points loop
    for B of Points loop
      declare
        Current_Area : Long := Calc_Area_Between_Points(A,B);
      begin
        if Current_Area > Largest_Area then
          Largest_Area := Current_Area;
        end if;
      end;
    end loop;
  end loop;

  Put_Line("Max area:" & Largest_Area'Image);
  Close(Input_File);
end;

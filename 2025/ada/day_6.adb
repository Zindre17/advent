with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Containers.Vectors;

procedure Day_6 is
  type Long is mod 2 ** 64;
  type Long_Array is array (Integer range <>) of Long;

  package Char_Vectors is new Ada.Containers.Vectors(Index_Type => Natural, Element_Type => Character);
  subtype Char_Vector is Char_Vectors.Vector;

  Input_File_Name : constant String := "day6.txt";
  Input_File : File_Type;

  Line_Count : Integer := 0;

  Task_Operators : Char_Vector;

  procedure Count_Lines is
  begin
    while not End_Of_File(Input_File) loop
      Skip_Line(Input_File);
      Line_Count := Line_Count + 1;
    end loop;
    Reset(Input_File);
  end;

  procedure Parse_Operators is
  begin
    Set_Line(File => Input_File, To => Positive_Count(Line_Count));
    declare
      Line : constant String := Get_Line(Input_File);
    begin
      for C of Line loop
        if C /= ' ' then
          Task_Operators.Append(C);
        end if;
      end loop;
    end;
    Reset(Input_File);
  end;

  procedure Parse_Next_Long(Line : String; Offset : in out Integer; Value : out Long) is
    Cursor_Position : Integer := Offset;
    Cursor : Character;
    Value_Start: Integer := 0;
  begin
    while Cursor_Position <= Line'Last loop
      Cursor := Line(Cursor_Position);
      if Value_Start = 0 and Cursor /= ' ' then
        Value_Start := Cursor_Position;
      end if;
      if Value_Start > 0 and Cursor = ' ' then
        exit;
      end if;
      Cursor_Position := Cursor_Position + 1;
    end loop;
    Value := Long'Value(Line(Value_Start .. Cursor_Position - 1));
    Offset := Cursor_Position;
  end;
begin
  Open(Input_File, In_File, Input_File_Name);
  Count_Lines;
  Parse_Operators;

  declare
    Task_Results : Long_Array(1 .. Integer(Task_Operators.Length)) := (others => 0);
    Sum : Long := 0;
  begin
    for I in 1 .. Line_Count - 1 loop
      Set_Line(Input_File, Positive_Count(I));
      declare
        Line : String := Get_Line(Input_File);
        Offset : Integer := 1;
        Value : Long;
      begin
        for E in Task_Results'Range loop
          declare
            Operator : constant Character := Task_Operators(E - 1);
          begin
            Parse_Next_Long(Line, Offset, Value);
            if I = 1 or Operator = '+' then
              Task_Results(E) := Task_Results(E) + Value;
            else
              Task_Results(E) := Task_Results(E) * Value;
            end if;
          end;
        end loop;
      end;
    end loop;

    for Result of Task_Results loop
      Sum := Sum + Result;
    end loop;
    Put_Line("Sum of task results:" & Sum'Image);
  end;
  Close(Input_File);
end;

with Ada.Text_IO; use Ada.Text_IO;

procedure Day_3 is
  type Long is mod 2 ** 128;
  type Index_And_Value is record
    Index : Integer;
    Value : Integer;
  end record;

  Input_File_Name : constant String := "day3.txt";
  Input_File : File_Type;

  Max_Joltage_2 : Long := 0;
  Max_Joltage_12 : Long := 0;

  function Find_Largest_Joltage(Batteries : String) return Index_And_Value is
    Result : Index_And_Value := (0,0);
    Index : Integer renames Result.Index;
    Value : Integer renames Result.Value;

    Current_Largest : Integer := 0;
    Current_Battery : Integer;
  begin
    for I in Batteries'Range loop
      Current_Battery := Integer'Value(Batteries(I) & "");

      if Value < Current_Battery then
        Value := Current_Battery;
        Index := I;
      end if;
    end loop;

    return Result;
  end;

  function Find_X_Largest_Joltage(Batteries : String;
                                  X         : Integer) return Long is
    Joltage_Sum : Long := 0;
    Highest_Index_And_Joltage : Index_And_Value;
    Start_Index : Integer := Batteries'First;
    End_Offset : Integer;
  begin
    for I in 1 .. X loop
      End_Offset := X - I;
      Highest_Index_And_Joltage := Find_Largest_Joltage(Batteries(Start_Index .. Batteries'Last - End_Offset));
      Start_Index:= Highest_Index_And_Joltage.Index + 1;
      Joltage_Sum := Joltage_Sum + Long(10 ** End_Offset) * Long(Highest_Index_And_Joltage.Value);
    end loop;
    return Joltage_Sum;
  end;

begin
  Open(Input_File, In_File, Input_File_Name);
  while not End_Of_File(Input_File) loop
    declare
      Line: constant String := Get_Line(Input_File);
    begin
      Max_Joltage_2 := Max_Joltage_2 + Find_X_Largest_Joltage(Line, 2);
      Max_Joltage_12 := Max_Joltage_12 + Find_X_Largest_Joltage(Line, 12);
    end;
  end loop;
  Put_Line("Max Joltage( 2 batteries):" & Max_Joltage_2'Image);
  Put_Line("Max Joltage(12 batteries):" & Max_Joltage_12'Image);
  Close(Input_File);
end;

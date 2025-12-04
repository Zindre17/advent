With Ada.Text_IO;
With Ada.Strings.Fixed;

procedure Day_2 is
  package IO renames Ada.Text_IO;
  type Long is mod 2 ** 64;

  Input_File_Name : constant String := "day2.txt";
  Input_File : IO.File_Type;

  Invalid_Id_Sum : Long := 0;

  Current_Character : Character;

  Range_Start_Digits : Integer := 0;
  Range_End_Digits : Integer := 0;
  Range_Start_Buffer : String(1 .. 20);
  Range_End_Buffer : String(1 .. 20);

  function Range_Start return String is
  begin
    return Range_Start_Buffer(Range_Start_Buffer'First .. Range_Start_Digits);
  end;
  function Range_End return String is
  begin
    return Range_End_Buffer(Range_End_Buffer'First .. Range_End_Digits);
  end;

begin
  IO.Open(Input_File, IO.In_File, Input_File_Name);
  while not IO.End_Of_File(Input_File) loop
    Range_End_Digits := 0;
    Range_Start_Digits := 0;
    loop
      IO.Get(Input_File, Current_Character);
      exit when Current_Character = '-';
      Range_Start_Digits := Range_Start_Digits + 1;
      Range_Start_Buffer(Range_Start_Digits) := Current_Character;
    end loop;

    loop
      exit when IO.End_Of_File(Input_File);
      IO.Get(Input_File, Current_Character);
      exit when Current_Character = ',';
      Range_End_Digits := Range_End_Digits + 1;
      Range_End_Buffer(Range_End_Digits) := Current_Character;
    end loop;


    for Id in Long'Value(Range_Start) .. Long'Value(Range_End) loop
      declare
        Id_String : String := Ada.Strings.Fixed.Trim(Id'Image, Ada.Strings.Left);
        Half_Digits : Integer;
      begin
        if Id_String'Length rem 2 = 0 then
          Half_Digits := Id_String'Length / 2;
          declare
            Part_One : String := Id_String(Id_String'First .. Half_Digits);
            Part_Two : String := Id_String(Half_Digits + 1 .. Id_String'Last);
          begin
            if Part_One = Part_Two then
              Invalid_Id_Sum := Invalid_Id_Sum + Id;
            end if;
          end;
        end if;
      end;
    end loop;
  end loop;
  IO.Put_Line("Sum of invalid IDs:" & Invalid_Id_Sum'Image);
  IO.New_Line;
  IO.Close(Input_File);
end;

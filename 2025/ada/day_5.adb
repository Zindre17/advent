with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

procedure Day_5 is
  type Long is mod 2 ** 64;
  type Int_Array is array (Integer range <>) of Long;
  type Range_Record is record
    First : Long;
    Last : Long;
  end record;
  type Range_Array is array (Integer range <>) of Range_Record;

  Input_File_Name : constant String := "day5.txt";
  Input_File : File_Type;

  Range_String_Count : Integer := 0;
  Ingredient_Count : Integer := 0;

  Fresh : Boolean;
  Fresh_Ingredient_Count : Integer := 0;
begin
  Open(Input_File, In_File, Input_File_Name);
  loop
    declare
      Line : constant String := Get_Line(Input_File);
    begin
      exit when Line = "";
      Range_String_Count := Range_String_Count + 1;
    end;
  end loop;
  while not End_Of_File(Input_File) loop
    declare
      Line : constant String := Get_Line(Input_File);
    begin
      Ingredient_Count := Ingredient_Count + 1;
    end;
  end loop;

  Reset(Input_File);

  declare
    Ranges : Range_Array(1 .. Range_String_Count);
    Ingredients : Int_Array(1 .. Ingredient_Count);
  begin
    for I in Ranges'Range loop
      declare
        Line : constant String := Get_Line(Input_File);
        Index_Of_Dash : Integer := Index(Source => Line,
                                         Pattern => "-",
                                         From => 1);
      begin
        Ranges(I) := (First => Long'Value(Line(1 .. Index_Of_Dash - 1)),
                      Last => Long'Value(Line(Index_Of_Dash + 1 .. Line'Last)));
      end;
    end loop;

    Skip_Line(Input_File);

    for I in Ingredients'Range loop
      declare
        Line : constant String := Get_Line(Input_File);
      begin
        Ingredients(I) := Long'Value(Line);
      end;
    end loop;

    for I in Ingredients'Range loop
      Fresh := False;
      for J in Ranges'Range loop
        if Ingredients(I) >= Ranges(J).First and Ingredients(I) <= Ranges(J).Last then
          Fresh := True;
        end if;
      end loop;

      if Fresh then
        Fresh_Ingredient_Count := Fresh_Ingredient_Count + 1;
      end if;
    end loop;

    Put_Line("Fresh ingredients:" & Fresh_Ingredient_Count'Image);
  end;


  Close(Input_File);
end;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;

procedure Day_5 is
  type Long is mod 2 ** 64;
  type Int_Array is array (Integer range <>) of Long;
  type Range_Record is record
    First : Long;
    Last : Long;
  end record;
  type Range_Array is array (Integer range <>) of Range_Record;

  package Range_Vectors is new Ada.Containers.Vectors(Index_Type => Positive, Element_Type => Range_Record);
  use Range_Vectors;

  Input_File_Name : constant String := "day5.txt";
  Input_File : File_Type;

  Range_String_Count : Integer := 0;
  Ingredient_Count : Integer := 0;

  Fresh : Boolean;
  Fresh_Ingredient_Count : Integer := 0;

  Processed_Ranges : Vector;

  function Min(A : Long; B : Long) return Long is
  begin
    if A < B then
      return A;
    else
      return B;
    end if;
  end;

  function Max(A : Long; B : Long) return Long is
  begin
    if A > B then
      return A;
    else
      return B;
    end if;
  end;

  function To_String(R : Range_Record) return String is
  begin
    return R.First'Image & " -" & R.Last'Image;
  end;

  function Intersects(A : Range_Record; B : Range_Record) return Boolean is
  begin
    return (A.First <= B.Last and A.Last >= B.First)
        or (B.First <= A.Last and B.Last >= A.First)
        or (A.First >= B.First and A.Last <= B.Last)
        or (B.First >= A.First and B.Last <= A.Last);
  end;
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

    for Current_Range of Ranges loop
      declare
        Did_Update : Boolean := False;
      begin
        for R of Processed_Ranges loop
          if Intersects(Current_Range, R) then
            R.First := Min(Current_Range.First, R.First);
            R.Last := Max(Current_Range.Last, R.Last);
            Did_Update := True;
            exit;
          end if;
        end loop;

        if not Did_Update then
          Processed_Ranges.Append(Current_Range);
        end if;
      end;
    end loop;

    for I in 1 .. Processed_Ranges.Length loop
      for J in 1 .. Processed_Ranges.Length loop
        declare
          A : Range_Record := Processed_Ranges(Positive(I));
          B : Range_Record := Processed_Ranges(Positive(J));
        begin
          if I /= J and Intersects(A, B) then
            Processed_Ranges(Positive(I)).First := Min(A.First, B.First);
            Processed_Ranges(Positive(I)).Last := Max(A.Last, B.Last);
            Processed_Ranges(Positive(J)).First := 0;
            Processed_Ranges(Positive(J)).Last := 0;
          end if;
        end;
      end loop;
    end loop;

    for I in reverse 1 ..Processed_Ranges.Length loop
      if Processed_Ranges(Positive(I)).First = 0 and Processed_Ranges(Positive(I)).Last = 0 then
        Processed_Ranges.Delete(Positive(I));
      end if;
    end loop;

    declare
      Id_Count : Long := 0;
    begin
      for R of Processed_Ranges loop
        Id_Count := Id_Count + (R.Last - R.First) + 1;
      end loop;
      Put_Line("Fresh ingredients:" & Fresh_Ingredient_Count'Image);
      Put_Line("Fresh ingredient IDs:" & Id_Count'Image);
    end;
  end;


  Close(Input_File);
end;

with Ada.Text_IO;

procedure Day_1 is
  type Dial_Digit is mod 100;

  Input_File_Name : constant String := "day1.txt";
  Input_File : Ada.Text_IO.File_Type;

  Direction_Character : Character;
  Clicks : Integer;

  Current_Position : Dial_Digit := 50;
  Direction : Dial_Digit;
  Landed_On_Zero_Counter : Integer := 0;
  Touched_Zero_Counter : Integer := 0;

  procedure RotateOneClick is
  begin
    Clicks := Clicks - 1;
    Current_Position := Current_Position + Direction;
    if Current_Position = 0 then
      Touched_Zero_Counter := Touched_Zero_Counter + 1;
    end if;
  end;

begin
  Ada.Text_IO.Open(Input_File, Ada.Text_IO.In_File, Input_File_Name);
  while not Ada.Text_IO.End_Of_File(Input_File) loop
    declare
      Line : String := Ada.Text_IO.Get_Line(Input_File);
    begin
      Direction_Character := Line(Line'First);
      Clicks := Integer'Value(Line(Line'First + 1 .. Line'Last));
    end;

    Direction := (if Direction_Character = 'L' then -1 else 1);

    while Clicks > 0 loop
      RotateOneClick;
    end loop;

    if Current_Position = 0 then
      Landed_On_Zero_Counter := Landed_On_Zero_Counter + 1;
    end if;

  end loop;
  Ada.Text_IO.Close(Input_File);

  Ada.Text_IO.Put_Line("Landed on zero" & Landed_On_Zero_Counter'Image & " times.");
  Ada.Text_IO.Put_Line("Passed zero" & Touched_Zero_Counter'Image & " times.");

end;

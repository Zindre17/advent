with Ada.Text_IO; use Ada.Text_IO;

procedure Day_7 is
  Input_File_Name : constant String := "day7.txt";
  Input_File : File_Type;

  Splits : Integer := 0;
begin
  Open(Input_File, In_File, Input_File_Name);
  declare
    Current_Line : String := Get_Line(Input_File);
    Previous_Line : String(Current_Line'Range);
  begin
    while not End_Of_File(Input_File) loop
      Previous_Line := Current_Line;
      Current_Line := Get_Line(Input_File);

      for I in Current_Line'Range loop
        if Previous_Line(I) = 'S' then
          Current_Line(I) := '|';
        elsif Previous_Line(I) = '|' then
          if Current_Line(I) = '^' then
            Splits := Splits + 1;
            if I > 1 and Current_Line(I - 1) = '.' then
              Current_Line(I - 1) := '|';
            end if;
            if I < Current_Line'Last and Current_Line(I + 1) = '.' then
              Current_Line(I + 1) := '|';
            end if;
          else
            Current_Line(I) := '|';
          end if;
        end if;
      end loop;
    end loop;

    Put_Line("Splits:" & Splits'Image);
  end;
  Close(Input_File);
end;

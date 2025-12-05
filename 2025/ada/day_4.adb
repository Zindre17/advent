with Ada.Text_IO; use Ada.Text_IO;

procedure Day_4 is
  Input_File_Name : constant String := "day4.txt";
  Input_File : File_Type;
  Start_Line : Count;
  Line_Count : Integer := 0;
  Line_Length : Integer;

  Accessible_Rolls_Of_Paper : Integer := 0;
  Total_Accessible_Rolls_Of_Paper : Integer := 0;
  Previous_Total : Integer := 1; -- Some other value than 0
begin
  Open(Input_File, In_File, Input_File_Name);
  Start_Line := Line(Input_File);
  while not End_Of_File(Input_File) loop
    declare
      Line : constant String := Get_Line(Input_File);
    begin
      Line_Count := Line_Count + 1;
      Line_Length := Integer(Line'Last);
    end;
  end loop;
  Reset(Input_File);

  declare
    type String_Array is array (1 .. Line_Count) of String(1 .. Line_Length);
    Grid : String_Array;
    Current_Line : Integer := 1;
  begin
    while not End_Of_File(Input_File) loop
      declare
        Line : constant String := Get_Line(Input_File);
      begin
        Grid(Current_Line) := Line;
        Current_Line := Current_Line + 1;
      end;
    end loop;

    for I in Grid'Range loop
      declare
        Row : String := Grid(I);
      begin
        for J in Row'Range loop
          declare
            Cell : Character := Row(J);
            Neighbour_Count : Integer := 0;
            Neighbour_To_Check : Character;
            procedure Bump_Neighbour_Count is
            begin
              Neighbour_Count := Neighbour_Count + 1;
            end;

            procedure Check_Neighbour is
            begin
              if Neighbour_To_Check = '@' then
                Bump_Neighbour_Count;
              end if;
            end;
          begin
            if Cell = '@' then
              -- West
              if J > 1 then
                Neighbour_To_Check := Row(J - 1);
                Check_Neighbour;
              end if;
              -- North-West
              if J > 1 and I > 1 then
                Neighbour_To_Check := Grid(I - 1)(J - 1);
                Check_Neighbour;
              end if;
              -- North
              if I > 1 then
                Neighbour_To_Check := Grid(I - 1)(J);
                Check_Neighbour;
              end if;
              -- North East
              if I > 1 and J < Line_Length then
                Neighbour_To_Check := Grid(I - 1)(J + 1);
                Check_Neighbour;
              end if;
              -- East
              if J < Line_Length then
                Neighbour_To_Check := Row(J + 1);
                Check_Neighbour;
              end if;
              -- South East
              if I < Line_Count and J < Line_Length then
                Neighbour_To_Check := Grid(I + 1)(J + 1);
                Check_Neighbour;
              end if;
              -- South
              if I < Line_Count then
                Neighbour_To_Check := Grid(I + 1)(J);
                Check_Neighbour;
              end if;
              -- South West
              if I < Line_Count and J > 1 then
                Neighbour_To_Check := Grid(I + 1)(J - 1);
                Check_Neighbour;
              end if;

              if Neighbour_Count < 4 then
                Accessible_Rolls_Of_Paper := Accessible_Rolls_Of_Paper + 1;
              end if;
            end if;
          end;
        end loop;
      end;
    end loop;
    while Total_Accessible_Rolls_Of_Paper /= Previous_Total loop
      Previous_Total := Total_Accessible_Rolls_Of_Paper;

      for I in Grid'Range loop
        declare
          Row : String := Grid(I);
        begin
          for J in Row'Range loop
            declare
              Cell : Character := Row(J);
              Neighbour_Count : Integer := 0;
              Neighbour_To_Check : Character;
              procedure Bump_Neighbour_Count is
              begin
                Neighbour_Count := Neighbour_Count + 1;
              end;

              procedure Check_Neighbour is
              begin
                if Neighbour_To_Check = '@' then
                  Bump_Neighbour_Count;
                end if;
              end;
            begin
              if Cell = '@' then
                -- West
                if J > 1 then
                  Neighbour_To_Check := Row(J - 1);
                  Check_Neighbour;
                end if;
                -- North-West
                if J > 1 and I > 1 then
                  Neighbour_To_Check := Grid(I - 1)(J - 1);
                  Check_Neighbour;
                end if;
                -- North
                if I > 1 then
                  Neighbour_To_Check := Grid(I - 1)(J);
                  Check_Neighbour;
                end if;
                -- North East
                if I > 1 and J < Line_Length then
                  Neighbour_To_Check := Grid(I - 1)(J + 1);
                  Check_Neighbour;
                end if;
                -- East
                if J < Line_Length then
                  Neighbour_To_Check := Row(J + 1);
                  Check_Neighbour;
                end if;
                -- South East
                if I < Line_Count and J < Line_Length then
                  Neighbour_To_Check := Grid(I + 1)(J + 1);
                  Check_Neighbour;
                end if;
                -- South
                if I < Line_Count then
                  Neighbour_To_Check := Grid(I + 1)(J);
                  Check_Neighbour;
                end if;
                -- South West
                if I < Line_Count and J > 1 then
                  Neighbour_To_Check := Grid(I + 1)(J - 1);
                  Check_Neighbour;
                end if;

                if Neighbour_Count < 4 then
                  Grid(I)(J) := '.';
                  Total_Accessible_Rolls_Of_Paper := Total_Accessible_Rolls_Of_Paper + 1;
                end if;
              end if;
            end;
          end loop;
        end;
      end loop;
    end loop;
  end;

  Put_Line("Accessible rolls of paper:" & Accessible_Rolls_Of_Paper'Image);
  Put_Line("Total accessible rolls of paper:" & Total_Accessible_Rolls_Of_Paper'Image);
  Close(Input_File);
end;

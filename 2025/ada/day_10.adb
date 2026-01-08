with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Machines; use Machines;

procedure Day_10 is
  Input_File_Name : constant String := "day10.txt";
  Input_File : File_Type;

  Line_Count : Natural := 0;

  procedure Count_Lines is
  begin
    while not End_Of_File(Input_File) loop
      Skip_Line(Input_File);
      Line_Count := Line_Count + 1;
    end loop;
    Reset(Input_File);
  end;

  function Parse_Indicator(Line : String) return String is
    Start_Index, End_Index : Natural;
  begin
    for I in Line'Range loop
      if Line(I) = '[' then
        Start_Index := I + 1;
      elsif Line(I) = ']' then
        End_Index := I - 1;
        exit;
      end if;
    end loop;

    return Line(Start_Index .. End_Index);
  end;

  function Parse_Buttons(Line : String) return Button_Vector is
    Result : Button_Vector;

    S, E : Natural;
    C : Natural := Ada.Strings.Fixed.Count(Line, "(");
    Offset : Natural := 1;
    function Parse_Button(Span : String) return Button is
      Content : String := Span(Span'First + 1 .. Span'Last - 1);
      Result : Int_Vector;
      C : Natural := Ada.Strings.Fixed.Count(Span, ",");
      O : Natural := Content'First;
      E : Natural := 0;
    begin
      for I in 1 .. C loop
        E := Index(Content, ",", O);
        Result.Append(Integer'Value(Content(O .. E - 1)));
        O := E + 1;
      end loop;

      Result.Append(Integer'Value(Content(O .. Content'Last)));

      return (Toggles => Result);
    end;
  begin
    E := 0;
    for I in 1 .. C loop
      S := Index(Line, "(", E + 1);
      E := Index(Line, ")", S);
      Result.Append(Parse_Button(Line(S .. E)));
    end loop;

    return Result;
  end;

  function Parse_Machine return Machine_Type is
    Line : String := Get_Line(Input_File);
    Indicators : String := Parse_Indicator(Line);
  begin
    return (Indicator_Count => Indicators'Length,
            Desired_State => Indicators,
            State => <>,
            Buttons => Parse_Buttons(Line),
            Presses => <>);
  end;

  Sum_Of_Button_Presses : Natural := 0;
  Current_Line : Natural := 0;
begin
  Open(Input_File, In_File, Input_File_Name);
  Count_Lines;
  while not End_Of_File(Input_File) loop
    declare
      M : Machine_Type := Parse_Machine;
      Idx : Natural := 0;
    begin
      Current_Line := Current_Line + 1;
      Put_Line("Prosessing machine" & Current_Line'Image & "/" & Line_Count'Image);
      Put_Line("Powered state:" & M.Desired_State);
      declare
        Solution : Power_Sequence := Power_Machine(M);
      begin
        Put_Line("State: " & Solution.State);
        Put("Presses:");
        for I of Solution.Presses loop
          Put(I'Image);
        end loop;
        New_Line;
        Sum_Of_Button_Presses := Sum_Of_Button_Presses + Natural(Solution.Presses.Length);
        Put_Line("Sum:" & Sum_Of_Button_Presses'Image);
      end;
    end;
  end loop;
  Put_Line("Sum of button presses:" & Sum_Of_Button_Presses'Image);
  Close(Input_File);
end;

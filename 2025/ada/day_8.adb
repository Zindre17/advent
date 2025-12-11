with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Elementary_Functions;
with Ada.Containers.Vectors;
with Ada.Strings;
with Ada.Strings.Fixed;

procedure Day_8 is
  type Point is record
    X : Integer;
    Y : Integer;
    Z : Integer;
  end record;

  function To_String(P : Point) return String is
  begin
    return "X:" & P.X'Image & ", Y:" & P.Y'Image & ", Z:" & P.Z'Image;
  end;

  type Two_Ints is record
    A: Integer;
    B: Integer;
  end record;

  package Point_Vectors is new Ada.Containers.Vectors(Index_Type => Positive, Element_Type => Point);
  package Int_Vectors is new Ada.Containers.Vectors(Index_Type => Positive, Element_Type => Integer);
  package Circuit_Vectors is new Ada.Containers.Vectors(Index_Type => Positive, Element_Type => Int_Vectors.Vector, "=" =>  Int_Vectors."=");
  function Circuit_Sorter(L, R : Int_Vectors.Vector) return Boolean is
  begin
    return Integer(L.Length) > Integer(R.Length);
  end;
  package Circuit_Sorting is new Circuit_Vectors.Generic_Sorting("<" => Circuit_Sorter);
  package Two_Ints_Vectors is new Ada.Containers.Vectors(Index_Type => Positive, Element_Type => Two_Ints);

  function Distance(A : Point; B : Point) return Float is
    DX : Integer := B.X - A.X;
    DY : Integer := B.Y - A.Y;
    DZ : Integer := B.Z - A.Z;
  begin
    return  Ada.Numerics.Elementary_Functions.Sqrt(Float(DX) ** 2 + Float(DY) ** 2 + Float(DZ) ** 2);
  end;

  function Parse_Point(Line : String) return Point is
    X, Y, Z : Integer;
    Next_Index : Integer;
    Offset : Integer := 1;
  begin
    Next_Index := Ada.Strings.Fixed.Index(Line, ",");
    X := Integer'Value(Ada.Strings.Fixed.Trim(Line(Offset .. Next_Index - 1), Ada.Strings.Left));
    Offset := Next_Index + 1;
    Next_Index := Ada.Strings.Fixed.Index(Line, ",", Offset);
    Y := Integer'Value(Ada.Strings.Fixed.Trim(Line(Offset .. Next_Index - 1), Ada.Strings.Left));
    Offset := Next_Index + 1;
    Z := Integer'Value(Ada.Strings.Fixed.Trim(Line(Offset .. Line'Last), Ada.Strings.Left));

    return (X,Y,Z);
  end;

  Input_File_Name : constant String := "day8.txt";
  Input_File : File_Type;

  Shortest_Distance : Float;
  Current_Distance : Float;
  From : Integer;
  To : Integer;
  Connections : Two_Ints_Vectors.Vector;
  Points : Point_Vectors.Vector;
  Circuits : Circuit_Vectors.Vector;

  function Connection_Exists(A : Integer; B : Integer) return Boolean is
  begin
    for Connection of Connections loop
      if (Connection.A = A and Connection.B = B)
        or (Connection.A = B and Connection.B = A) then
        return True;
      end if;
    end loop;
    return False;
  end;

  function Contains(Values: Int_Vectors.Vector; Value : Integer) return Boolean is
  begin
    for Val of Values loop
      if Val = Value then
        return True;
      end if;
    end loop;
    return False;
  end;

  function Is_Connected(A : Int_Vectors.Vector; B : Int_Vectors.Vector) return Boolean is
  begin
    for X of A loop
      for Y of B loop
        if X = Y then
          return True;
        end if;
      end loop;
    end loop;
    return False;
  end;
begin
  Open(Input_File, In_File, Input_File_Name);
  while not End_Of_File(Input_File) loop
    Points.Append(Parse_Point(Get_Line(Input_File)));
  end loop;

  for X in 1 .. 1000 loop
    Shortest_Distance := Float'Last;
    for I in Points.First_Index .. Points.Last_Index loop
      for J in Points.First_Index .. Points.Last_Index loop
        if I /= J then
          Current_Distance := Distance(Points(I), Points(J));
          if (not Connection_Exists(I,J)) and Current_Distance < Shortest_Distance then
            Shortest_Distance := Current_Distance;
            From := I;
            To := J;
          end if;
        end if;
      end loop;
    end loop;
    Connections.Append((From,To));
  end loop;

  for Connection of Connections loop
    declare
      Is_New_Circuit : Boolean := True;
    begin
      for Circuit of Circuits loop
        if Contains(Circuit, Connection.A) or Contains(Circuit, Connection.B) then
          if not Contains(Circuit, Connection.A) then
            Circuit.Append(Connection.A);
          elsif not Contains(Circuit, Connection.B) then
            Circuit.Append(Connection.B);
          end if;
          Is_New_Circuit := False;
          exit;
        end if;
      end loop;
      if Is_New_Circuit then
        declare
          New_Circuit : Int_Vectors.Vector;
        begin
          New_Circuit.Append(Connection.A);
          New_Circuit.Append(Connection.B);
          Circuits.Append(New_Circuit);
        end;
      end if;
    end;
  end loop;

  for I in reverse Circuits.First_Index .. Circuits.Last_Index loop
    for J in reverse Circuits.First_Index .. I - 1 loop
      if I /= J then
        if Is_Connected(Circuits(I), Circuits(J)) then
          for E of Circuits(I) loop
            if not Contains(Circuits(J), E) then
              Circuits(J).Append(E);
            end if;
          end loop;
          Circuits.Delete(I);
        end if;
      end if;
    end loop;
  end loop;

  Close(Input_File);

  declare
    Score : Integer :=  1;
  begin
    for I in Circuits.First_Index .. Circuits.First_Index + 2 loop
      Score := Score * Integer(Circuits(I).Length);
    end loop;
    Put_Line("Score:" & Score'Image);
  end;

end;

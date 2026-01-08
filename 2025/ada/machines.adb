
package body Machines is
  procedure Toggle_Indicator(State : in out String; I : Natural) is
    Idx : Natural := State'First + I;
  begin
    if State(Idx) = '.' then
      State(Idx) := '#';
    else
      State(Idx) := '.';
    end if;
  end;

  procedure Push_Button(M : in Machine_Type;
                        P : in out Power_Sequence;
                        B : Button) is
  begin
    for I of B.Toggles loop
      Toggle_Indicator(P.State, I);
    end loop;
    P.Presses.Append(M.Buttons.Find_Index(B));
  end;

  function Is_Powered(State : String; Desired_State : String) return Boolean is
  begin
    for I in State'Range loop
      if State(I) /= Desired_State(I) then
        return False;
      end if;
    end loop;
    return true;
  end;

  function Clone(P : Power_Sequence) return Power_Sequence is
    State : String(1 .. P.Indicator_Count);
    Presses : Int_Vector;
  begin
    for I in P.State'Range loop
      State(I) := P.State(I);
    end loop;
    for I of P.Presses loop
      Presses.Append(I);
    end loop;
    return (Indicator_Count => P.Indicator_Count,
            State => State,
            Presses => Presses);
  end;

  function Power_Machine(M : Machine_Type) return Power_Sequence is
    subtype A_Power_Sequence is
      Power_Sequence(M.Indicator_Count);

    package Power_Sequence_Vectors is new
      Ada.Containers.Vectors(Natural, A_Power_Sequence);

    subtype Power_Sequence_Vector is Power_Sequence_Vectors.Vector;

    Alternatives : Power_Sequence_Vector;
  begin
    Alternatives.Append((Indicator_Count => M.Indicator_Count, Presses => <>, State => <>));
    while not Power_Sequence_Vectors.Is_Empty(Alternatives) loop
      declare
        Current_Sequence : A_Power_Sequence := Alternatives.First_Element;
      begin
        Alternatives.Delete(Alternatives.Find_Index(Current_Sequence));
        for B of M.Buttons loop
          declare
            Next_Sequence : A_Power_Sequence := Clone(Current_Sequence);
          begin
            Push_Button(M, Next_Sequence, B);
            if Is_Powered(Next_Sequence.State, M.Desired_State) then
              return Next_Sequence;
            end if;
            Alternatives.Append(Next_Sequence);
          end;
        end loop;
      end;
    end loop;
    raise E;
  end;
end;

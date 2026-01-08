with Ada.Containers.Vectors;

package Machines is
  package Exceptions is
    E : exception;
  end;
  use Exceptions;

  package Int_Vectors is new Ada.Containers.Vectors(Natural, Integer);
  subtype Int_Vector is Int_Vectors.Vector;

  type Button is record
    Toggles : Int_Vector;
  end record;
  package Button_Vectors is new Ada.Containers.Vectors(Natural, Button);
  subtype Button_Vector is Button_Vectors.Vector;

  type Machine_Type(Indicator_Count : Natural) is record
    Desired_State : String(1 .. Indicator_Count);
    Buttons : Button_Vector;
    State : String(1 .. Indicator_Count) := (others => '.');
    Presses : Int_Vector;
  end record;

  type Power_Sequence(Indicator_Count : Natural) is record
    Presses : Int_Vector;
    State : String(1 .. Indicator_Count) := (others => '.');
  end record;

  function Power_Machine(M : Machine_Type) return Power_Sequence;
end;

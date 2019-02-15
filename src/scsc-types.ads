package SCSC.Types
   with SPARK_Mode => On
is

   type Point is
   record
      X : Integer;
      Y : Integer;
   end record;

   function Distance (P1 : Types.Point;
                      P2 : Types.Point) return Natural;

   type Angle is digits 4 range -360.0 .. 360.0;

   function Difference (Start, Stop : Angle) return Angle;

   type Unit_Type is (Invalid, Px, Em);

   type Length (Unit : Unit_Type) is tagged
   record
      case Unit is
         when Px =>
            Px : Integer;
         when Em =>
            Em : Float;
         when others =>
            null;
      end case;
   end record;

   function Image (L : Length) return String;

   Invalid_Length : Length := (Unit => Invalid);

end SCSC.Types;

package SCSC.Types
   with SPARK_Mode => On
is

   type Point is tagged private;

   function P (X : Integer;
               Y : Integer) return Point;

   function X (P : Point) return Integer;
   function Y (P : Point) return Integer;

   function Distance (P1 : Types.Point;
                      P2 : Types.Point) return Natural;

   type Angle_Base is digits 10 range -360.0 .. 359.9999999999;
   type Angle is new Angle_Base range 0.0 .. Angle_Base'Last;

   overriding
   function "+" (Left, Right : Angle) return Angle;
   --  Modular addition of Angles

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

private

   type Point is tagged
   record
      X : Integer;
      Y : Integer;
   end record;

   function P (X : Integer;
               Y : Integer) return Point is (X => X, Y => Y);

   function X (P : Point) return Integer is (P.X);
   function Y (P : Point) return Integer is (P.Y);

end SCSC.Types;

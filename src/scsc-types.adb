with Ada.Text_IO;
with Ada.Strings.Fixed;
with SCSC.Math;

package body SCSC.Types
   with SPARK_Mode => Off
is

   function Image (L : Length) return String
   is
      package FIO is new Ada.Text_IO.Float_IO (Float);
      Buffer : String (1..50);
      use Ada.Strings;
      use Ada.Strings.Fixed;
   begin
      case L.Unit is
         when Px =>
            return L.Px'Image;
         when Em =>
            FIO.Put (To => Buffer, Item => L.Em, Aft => 3, Exp => 0); 
            return Trim (Buffer, Both) & "em";
         when others =>
            return "INVALID";
      end case;
   end Image;

   --------------
   -- Distance --
   --------------

   function Distance (P1 : Types.Point;
                      P2 : Types.Point) return Natural
   is
      X_Len  : constant Integer := P1.X - P2.X;
      Y_Len  : constant Integer := P1.Y - P2.Y;
      use Math;
   begin
      return Integer (Sqrt (Float (X_Len ** 2 + Y_Len ** 2)));
   end Distance;

   ----------------
   -- Difference --
   ----------------

   function Difference (Start, Stop : Angle) return Angle
      is (if Start < Stop then Stop - Start else 360.0 - Start + Stop);

end SCSC.Types;

with Ada.Numerics.Generic_Elementary_Functions;

package body SCSC.Math
   with SPARK_Mode => Off
is
   package EF is new Ada.Numerics.Generic_Elementary_Functions (Types.Angle_Base);
   package FM is new Ada.Numerics.Generic_Elementary_Functions (Float);

   function Sin (X : Types.Angle) return Float
   is
      Result : Types.Angle_Base;
      use type Types.Angle_Base;
   begin
      return Float (EF.Sin (X => Types.Angle_Base (X), Cycle => 360.0));
   end Sin;

   function Cos (X : Types.Angle) return Float
   is
   begin
      return Float (EF.Cos (X => Types.Angle_Base (X), Cycle => 360.0));
   end Cos;

   function Arctan (X : Types.Angle) return Types.Angle
   is
   begin
      return Types.Angle (EF.Arctan (Y => Types.Angle_Base (X), Cycle => 360.0));
   end Arctan;

   function Sqrt (X : Float) return Float is (FM.Sqrt (X));

   function Arctan (Y     : Float;
                    X     : Float;
                    Cycle : Float) return Float is (FM.Arctan (Y, X, Cycle));

   function Arcsin (X     : Float;
                    Cycle : Float) return Float is (FM.Arcsin (X, Cycle));

end SCSC.Math;

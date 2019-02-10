with Ada.Numerics.Generic_Elementary_Functions;

package body SCSC.Math
   with SPARK_Mode => Off
is
   package EF is new Ada.Numerics.Generic_Elementary_Functions (Types.Angle);
   package FM is new Ada.Numerics.Generic_Elementary_Functions (Float);

   function Sin (X : Types.Angle) return Types.Angle
   is
   begin
      return EF.Sin (X => X, Cycle => 360.0);
   end Sin;

   function Cos (X : Types.Angle) return Types.Angle
   is
   begin
      return EF.Cos (X => X, Cycle => 360.0);
   end Cos;

   function Arctan (X : Types.Angle) return Types.Angle
   is
   begin
      return EF.Arctan (Y => X, Cycle => 360.0);
   end Arctan;

   function Sqrt (X : Float) return Float is (FM.Sqrt (X));

   function Arctan (Y     : Float;
                    X     : Float;
                    Cycle : Float) return Float is (FM.Arctan (Y, X, Cycle));

end SCSC.Math;

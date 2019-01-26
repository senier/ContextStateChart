with Ada.Numerics.Generic_Elementary_Functions;

package body SCSC.Math
   with SPARK_Mode => Off
is
   package EF is new Ada.Numerics.Generic_Elementary_Functions (Types.Angle);

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

end SCSC.Math;

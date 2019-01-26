with SCSC.Types;

package SCSC.Math
   with SPARK_Mode => On
is
   
   function Sin (X : Types.Angle) return Types.Angle
   with
      Global => null;
   
   function Cos (X : Types.Angle) return Types.Angle
   with
      Global => null;

end SCSC.Math;

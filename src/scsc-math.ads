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

   function Arctan (X : Types.Angle) return Types.Angle
   with
      Global => null;

   function Sqrt (X : Float) return Float
   with
      Global => null;

   function Arctan (Y     : Float;
                    X     : Float;
                    Cycle : Float) return Float
   with
      Global => null;

   function Arcsin (X     : Float;
                    Cycle : Float) return Float
   with
      Global => null;

end SCSC.Math;

with SXML.Generator;
with SCSC.Math;

package body SCSC.Primitives
   with SPARK_Mode => On
is
   ----------
   -- Cart --
   ----------

   function Cartesian (From       : Types.Point;
                       To         : Types.Point;
                       X_Radius   : Natural;
                       Y_Radius   : Natural;
                       X_Rotation : Natural := 0;
                       Large      : Boolean := False;
                       Sweep      : Boolean := False) return Arc_Params_Type
   is
   begin
      return
        (From       => From,
         To         => To,
         X_Radius   => X_Radius,
         Y_Radius   => Y_Radius,
         X_Rotation => X_Rotation,
         Large      => Large,
         Sweep      => Sweep);
   end Cartesian;

   -----------
   -- Polar --
   -----------

   function Polar (Center     : Types.Point;
                   Radius     : Natural;
                   Start      : Types.Angle;
                   Stop       : Types.Angle) return Arc_Params_Type
   is
      use Math;
      use type Types.Angle;
   begin
      return
        (From       => (X => Center.X + Integer (Sin (Start) * Types.Angle (Radius)),
                        Y => Center.Y + Integer (-Cos (Start) * Types.Angle (Radius))),
         To         => (X => Center.X + Integer (Sin (Stop) * Types.Angle (Radius)),
                        Y => Center.Y + Integer (-Cos (Stop) * Types.Angle (Radius))),
         X_Radius   => Radius,
         Y_Radius   => Radius,
         X_Rotation => 0,
         Large      => False,
         Sweep      => Start > Stop);
   end Polar;

   ---------
   -- Arc --
   ---------

   function Arc (Params : Arc_Params_Type;
                 Style  : String  := "";
                 ID     : String  := "") return SCSC.SVG.Element_Type
   is
      use SCSC.SVG;
      use SXML.Generator;
   begin
      --  FIXME: Create style object to set style
      return To_Element (Commands =>
                         ((Moveto, Absolute, Params.From.X, Params.From.Y),
                          (Arc, Absolute, RX         => Params.X_Radius,
                                          RY         => Params.Y_Radius,
                                          X_Rotation => Params.X_Rotation,
                                          Large      => Params.Large,
                                          Sweep      => Params.Sweep,
                                          AX         => Params.To.X,
                                          AY         => Params.To.Y)
                         ),
                         Style => Style,
                         ID    => ID);
   end Arc;

   ----------
   -- From --
   ----------

   function From (Params : Arc_Params_Type) return Types.Point
   is
   begin
      return Params.From;
   end From;

   --------
   -- To --
   --------

   function To (Params : Arc_Params_Type) return Types.Point
   is
   begin
      return Params.To;
   end To;

end SCSC.Primitives;

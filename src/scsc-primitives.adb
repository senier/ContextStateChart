with SXML.Generator;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_IO; use Ada.Text_IO;

package body SCSC.Primitives
is
   ---------
   -- Arc --
   ---------

   function Arc (From       : Types.Point;
                 To         : Types.Point;
                 X_Radius   : Natural;
                 Y_Radius   : Natural;
                 X_Rotation : Natural := 0;
                 Large      : Boolean := False;
                 Sweep      : Boolean := False;
                 Style      : String  := "") return SCSC.SVG.Element_Type
   is
      use SCSC.SVG;
      use SXML.Generator;
   begin
      --  FIXME: Create style object to set style
      return To_Element (Commands =>
                         ((Moveto, Absolute, From.X, From.Y),
                          (Arc, Absolute, RX         => X_Radius,
                                          RY         => Y_Radius,
                                          X_Rotation => X_Rotation,
                                          Large      => Large,
                                          Sweep      => Sweep,
                                          AX         => To.X,
                                          AY         => To.Y)
                         ),
                         Style => Style);
   end Arc;

   ---------
   -- Arc --
   ---------

   function Arc (Center     : Types.Point;
                 Radius     : Natural;
                 Start      : Types.Angle;
                 Angle      : Types.Angle;
                 From       : out Types.Point;
                 To         : out Types.Point;
                 Style      : String  := "") return SCSC.SVG.Element_Type
   is
      package EF is new Ada.Numerics.Generic_Elementary_Functions (Types.Angle);
      use EF;
      use type Types.Angle;
   begin
      From.X := Center.X + Integer (Sin (Start, Cycle => 360.0) * Types.Angle (Radius));
      From.Y := Center.Y + Integer (-Cos (Start, Cycle => 360.0) * Types.Angle (Radius));
      To.X   := Center.X + Integer (Sin (Start + Angle, Cycle => 360.0) * Types.Angle (Radius));
      To.Y   := Center.Y + Integer (-Cos (Start + Angle, Cycle => 360.0) * Types.Angle (Radius));

      return Arc (From       => From,
                  To         => To,
                  X_Radius   => Radius,
                  Y_Radius   => Radius,
                  X_Rotation => 0,
                  Large      => False,
                  Sweep      => Angle >= 0.0,
                  Style      => Style);
   end Arc;
end SCSC.Primitives;

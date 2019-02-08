with SXML.Generator;
with SCSC.Math;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Numerics.Discrete_Random;

package body SCSC.Primitives
   with SPARK_Mode => On
is
   package FM is new Ada.Numerics.Generic_Elementary_Functions (Float);
   package DR is new Ada.Numerics.Discrete_Random (Natural);

   ---------------
   -- Cartesian --
   ---------------

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
      Angle : Types.Angle := (if Start < Stop then Stop - Start else 360.0 - Start + Stop);
   begin

      return
        (From       => (X => Center.X + Integer (Sin (Start) * Types.Angle (Radius)),
                        Y => Center.Y + Integer (-Cos (Start) * Types.Angle (Radius))),
         To         => (X => Center.X + Integer (Sin (Stop) * Types.Angle (Radius)),
                        Y => Center.Y + Integer (-Cos (Stop) * Types.Angle (Radius))),
         X_Radius   => Radius,
         Y_Radius   => Radius,
         X_Rotation => 0,
         Large      => Angle > 180.0,
         Sweep      => True);
   end Polar;

   --------------
   -- Distance --
   --------------

   function Distance (P1 : Types.Point;
                      P2 : Types.Point) return Natural
   is
      X_Len  : constant Integer := P1.X - P2.X;
      Y_Len  : constant Integer := P1.Y - P2.Y;
      use FM;
   begin
      Return Integer (Sqrt (Float (X_Len ** 2 + Y_Len ** 2)));
   end Distance;

   ---------------
   -- Cartesian --
   ---------------

   function Cartesian (Center : Types.Point;
                       Start  : Types.Point;
                       Length : Integer) return Line_Params_Type
   is
      use Math;
      use FM;
      use type Types.Angle;

      X_Len  : constant Integer := Start.X - Center.X;
      Y_Len  : constant Integer := Start.Y - Center.Y;

      Offset : constant Natural := Distance (Center, Start);
      Last   : constant Integer := Offset + Length;
      Stop   : constant Types.Point := (X => X_Len * Last / Offset + Center.X,
                                        Y => Y_Len * Last / Offset + Center.Y);
   begin
      return (From => Start, To => Stop);
   end Cartesian;

   -----------
   -- Polar --
   -----------

   function Polar (Center     : Types.Point;
                   Start      : Natural;
                   Stop       : Natural;
                   Angle      : Types.Angle) return Line_Params_Type
   is
      use Math;
      use type Types.Angle;
   begin

      return
        (From       => (X => Center.X + Integer (Sin (Angle) * Types.Angle (Start)),
                        Y => Center.Y + Integer (-Cos (Angle) * Types.Angle (Start))),
         To         => (X => Center.X + Integer (Sin (Angle) * Types.Angle (Stop)),
                        Y => Center.Y + Integer (-Cos (Angle) * Types.Angle (Stop))));
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
      return Path (Commands =>
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
   -- Line --
   ----------

   function Line (Params       : Line_Params_Type;
                  Marker_Start : String  := "";
                  Marker_End   : String  := "";
                  Style        : String  := "";
                  ID           : String  := "") return SCSC.SVG.Element_Type
   is
      use SCSC.SVG;
      use SXML.Generator;
   begin
      --  FIXME: Create style object to set style
      return Path (Commands =>
                   ((Moveto, Absolute, Params.From.X, Params.From.Y),
                    (Lineto, Absolute, X => Params.To.X,
                                       Y => Params.To.Y)
                   ),
                   Marker_Start => Marker_Start,
                   Marker_End   => Marker_End,
                   Style        => Style,
                   ID           => ID);
   end Line;

   ----------
   -- From --
   ----------

   function From (Params : Params_Type) return Types.Point
   is
   begin
      return Params.From;
   end From;

   --------
   -- To --
   --------

   function To (Params : Params_Type) return Types.Point
   is
   begin
      return Params.To;
   end To;

   --------------
   -- To_Angle --
   --------------

   function To_Angle (Center : Types.Point;
                      P      : Types.Point) return Types.Angle
   is
      use FM;
      use type Types.Angle;
      Result : Types.Angle;
   begin
      Result := -Types.Angle (Arctan (Y     => Float (P.Y - Center.Y),
                                      X     => Float (P.X - Center.X),
                                      Cycle => 360.0)) + 90.0;
      return (if Result < 0.0 then Result + 360.0 else Result);
   end To_Angle;

   ---------------
   -- Connector --
   ---------------

   function Connector (Center       : Types.Point;
                       Start        : Types.Point;
                       Stop         : Types.Point;
                       Radius       : Integer;
                       Text         : String         := "";
                       Textstyle    : String         := "";
                       Align        : SVG.Align_Type := SVG.Align_Centered;
                       Direction    : Dir_Type       := Dir_CW;
                       Position     : Pos_Type       := Pos_Outer;
                       Marker_Start : String         := "";
                       Marker_End   : String         := "";
                       Style        : String         := "";
                       ID           : String         := "") return SVG.Element_Type
   is
      use type SVG.Element_Type;
      use Types;

      LP_1    : constant Line_Params_Type := Cartesian (Center, Start, Radius);
      Arc_Off : constant Natural          := Distance (Center, LP_1.To);
      LP_2    : constant Line_Params_Type := Cartesian (Center, Stop, Arc_Off - Distance (Center, Stop));
      R       : constant Natural          := Distance (Center, LP_1.To);

      Start_Angle : constant Types.Angle := To_Angle (Center, LP_1.To);
      Stop_Angle  : constant Types.Angle := To_Angle (Center, LP_2.To);

      Angle       : constant Types.Angle := (if Start_Angle < Stop_Angle
                                             then Stop_Angle - Start_Angle
                                             else 360.0 - Start_Angle + Stop_Angle);

      Arc_Params  : constant Arc_Params_Type :=
         Cartesian (From     => (if Direction = Dir_CW then LP_1.To else LP_2.To),
                    To       => (if Direction = Dir_CW then LP_2.To else LP_1.To),
                    X_Radius => R,
                    Y_Radius => R,
                    Large    => Angle <= 180.0,
                    Sweep    => (if Direction = Dir_CW then True else False));

      G         : DR.Generator;
      Random_ID : Natural;
      DY        : Types.Length := (if Direction = Dir_CW
                                   then (if Position = Pos_Outer then (Em, -0.1) else (Em, 1.0))
                                   else (if Position = Pos_Outer then (Em, 1.0) else (Em, -0.1)));

      function To_ID (Value : Natural) return String is ("ID" & Value'Image (Value'Image'First + 1 .. Value'Image'Last));

   begin
      DR.Reset (G);
      Random_ID := DR.Random (G);

      return Line (Params       => LP_1,
                   Marker_Start => Marker_End,
                   Style        => Style,
                   ID           => (if ID /= "" then ID & "1" else ""))

            + Arc (Params => Arc_Params,
                   Style  => Style,
                   ID     => (if ID /= "" then ID & "2" else To_ID (Random_ID)))

            + (if Text /= ""
               then SVG.Text (Center,
                              Text  => Text,
                              Style => Textstyle,
                              Align => Align,
                              DY    => DY,
                              Path  => (if ID /= "" then ID & "2" else To_ID (Random_ID)))
               else SVG.Null_Element)

            + Line (Params       => Points (LP_2.From, LP_2.To),
                    Marker_Start => Marker_Start,
                    Style        => Style,
                    ID           => (if ID /= "" then ID & "3" else ""));
   end Connector;

   ------------
   -- Points --
   ------------

   function Points (Start : Types.Point;
                    Stop  : Types.Point) return Line_Params_Type is (Start, Stop);

   -----------
   -- Polar --
   -----------

   function Polar (Center : Types.Point;
                   Offset : Natural;
                   Radius : Natural;
                   Start  : Types.Angle;
                   Stop   : Types.Angle) return Annular_Sector_Params_Type
   is
   begin
      return (Inner => (Polar (Center => Center,
                               Radius => Offset,
                               Start  => Start,
                               Stop   => Stop)),
              Outer => (Polar (Center => Center,
                               Radius => Offset + Radius,
                               Start  => Start,
                               Stop   => Stop)));
   end Polar;

   --------------------
   -- Annular_Sector --
   --------------------

   function Annular_Sector (Params : Annular_Sector_Params_Type;
                            Style  : String  := "";
                            ID     : String  := "") return SVG.Element_Type
   is
      use SVG;
   begin
      return Path (Commands =>
                   ((Moveto, Absolute, Params.Inner.From.X, Params.Inner.From.Y),
                    (Arc, Absolute, RX         => Params.Inner.X_Radius,
                                    RY         => Params.Inner.Y_Radius,
                                    X_Rotation => Params.Inner.X_Rotation,
                                    Large      => Params.Inner.Large,
                                    Sweep      => Params.Inner.Sweep,
                                    AX         => Params.Inner.To.X,
                                    AY         => Params.Inner.To.Y),
                    (Lineto, Absolute, Params.Outer.To.X, Params.Outer.To.Y),
                    (Arc, Absolute, RX         => Params.Outer.X_Radius,
                                    RY         => Params.Outer.Y_Radius,
                                    X_Rotation => Params.Outer.X_Rotation,
                                    Large      => Params.Outer.Large,
                                    Sweep      => not Params.Outer.Sweep,
                                    AX         => Params.Outer.From.X,
                                    AY         => Params.Outer.From.Y),
                    (ZClosepath, Absolute)
                   ),
                   Style => Style,
                   ID    => ID);
   end Annular_Sector;

end SCSC.Primitives;

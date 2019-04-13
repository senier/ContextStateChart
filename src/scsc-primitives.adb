with SXML.Generator;
with SCSC.Math;
with Ada.Numerics;

package body SCSC.Primitives
   with SPARK_Mode => On
is
   use type Types.Angle;

   ---------------
   -- Cartesian --
   ---------------

   function Cartesian (From       : Types.Point;
                       To         : Types.Point;
                       Center     : Types.Point;
                       Radius     : Natural;
                       X_Rotation : Natural := 0;
                       Large      : Boolean := False;
                       Sweep      : Boolean := False) return Arc_Params_Type
   is
      Angle  : constant Types.Angle := Types.Difference (To_Angle (Center, From), To_Angle (Center, To));
      Length : constant Natural     := Natural (Float (Radius) * Float (Angle) / 180.0 * Ada.Numerics.Pi);
   begin
      return
        (From       => From,
         To         => To,
         Center     => Center,
         Length     => Length,
         Radius     => Radius,
         X_Rotation => X_Rotation,
         Large      => Large,
         Sweep      => Sweep);
   end Cartesian;

   -----------
   -- Polar --
   -----------

   function Polar (Center  : Types.Point;
                   Radius  : Natural;
                   Angle   : Types.Angle) return Types.Point
   is
   begin
      return Types.P (X => Center.X + Integer (Math.Sin (Angle) * Float (Radius)),
                      Y => Center.Y + Integer (-Math.Cos (Angle) * Float (Radius)));
   end Polar;

   -----------
   -- Polar --
   -----------

   function Polar (Center     : Types.Point;
                   Radius     : Natural;
                   Start      : Types.Angle;
                   Stop       : Types.Angle) return Arc_Params_Type
   is
      Angle  : constant Types.Angle := Types.Difference (Start, Stop);
      Length : constant Natural     := Natural (Float (Radius) * Float (Angle) / 180.0 * Ada.Numerics.Pi);
   begin
      return
        (From       => Polar (Center, Radius, Start),
         To         => Polar (Center, Radius, Stop),
         Center     => Center,
         Length     => Length,
         Radius     => Radius,
         X_Rotation => 0,
         Large      => Angle > 180.0,
         Sweep      => True);
   end Polar;

   -----------
   -- Inner --
   -----------

   function Inner (Params : Annular_Sector_Params_Type'Class) return Arc_Params_Type is (Params.Inner);

   ---------------
   -- Cartesian --
   ---------------

   function Cartesian (Center : Types.Point;
                       Start  : Types.Point;
                       Length : Integer) return Line_Params_Type
   is
      X_Len  : constant Integer := Start.X - Center.X;
      Y_Len  : constant Integer := Start.Y - Center.Y;

      Offset : constant Natural := Types.Distance (Center, Start);
      Last   : constant Integer := Offset + Length;
      Stop   : constant Types.Point := Types.P (X => X_Len * Last / Offset + Center.X,
                                                Y => Y_Len * Last / Offset + Center.Y);
   begin
      return (From   => Start,
              To     => Stop,
              Length => Types.Distance (Start, Stop));
   end Cartesian;

   -----------
   -- Polar --
   -----------

   function Polar (Center     : Types.Point;
                   Start      : Natural;
                   Stop       : Natural;
                   Angle      : Types.Angle) return Line_Params_Type
   is
      From : constant Types.Point := Polar (Center, Start, Angle);
      To   : constant Types.Point := Polar (Center, Stop, Angle);
   begin
      return
        (From   => From,
         To     => To,
         Length => Types.Distance (From, To));
   end Polar;

   ---------
   -- Arc --
   ---------

   function Arc (Params : Arc_Params_Type;
                 ID     : String := "";
                 Class  : String := "") return SXML.Document_Type
   is
      use SCSC.SVG;
   begin
      return Path (Commands =>
                   ((Moveto, Absolute, Params.From.X, Params.From.Y),
                    (Arc, Absolute, RX         => Params.Radius,
                                    RY         => Params.Radius,
                                    X_Rotation => Params.X_Rotation,
                                    Large      => Params.Large,
                                    Sweep      => Params.Sweep,
                                    AX         => Params.To.X,
                                    AY         => Params.To.Y)
                   ),
                   ID => ID,
                   Class => (if Class = "" then "scsc_arc arc" else "scsc_arc arc " & Class));
   end Arc;

   ----------
   -- Line --
   ----------

   function Line (Params : Line_Params_Type;
                  ID     : String := "";
                  Class  : String := "") return SXML.Document_Type
   is
      use SCSC.SVG;
   begin
      return Path (Commands =>
                   ((Moveto, Absolute, Params.From.X, Params.From.Y),
                    (Lineto, Absolute, X => Params.To.X,
                                       Y => Params.To.Y)
                   ),
                   ID           => ID,
                   Class        => (if Class = "" then "line" else "line " & Class));
   end Line;

   ----------
   -- From --
   ----------

   function From (Params : Params_Type) return Types.Point is (Params.From);

   --------
   -- To --
   --------

   function To (Params : Params_Type) return Types.Point is (Params.To);

   ------------
   -- Length --
   ------------

   function Length (Params : Params_Type) return Natural is (Params.Length);

   --------------
   -- To_Angle --
   --------------

   function To_Angle (Center : Types.Point;
                      P      : Types.Point) return Types.Angle
   is
      use Math;
      Result : Float;
   begin
      Result := Arctan (Y     => Float (P.Y - Center.Y),
                        X     => Float (P.X - Center.X),
                        Cycle => 360.0) + 90.0;
      return Types.Angle ((if Result < 0.0 then 360.0 + Result else Result));
   end To_Angle;

   ---------------
   -- Cartesian --
   ---------------

   function Cartesian (Center    : Types.Point;
                       Start     : Types.Point;
                       Stop      : Types.Point;
                       Radius    : Integer;
                       Direction : Dir_Type := Dir_CW;
                       Label     : String   := "") return Connector_Params_Type
   is
      LP_1    : constant Line_Params_Type := Cartesian (Center, Start, Radius);
      Arc_Off : constant Natural          := Types.Distance (Center, LP_1.To);
      LP_2    : constant Line_Params_Type := Cartesian (Center, Stop, Arc_Off - Types.Distance (Center, Stop));

      Start_Angle : constant Types.Angle := To_Angle (Center, LP_1.To);
      Stop_Angle  : constant Types.Angle := To_Angle (Center, LP_2.To);
      Diff_Angle  : constant Types.Angle := Types.Difference (Start_Angle, Stop_Angle);

      R   : constant Natural := Types.Distance (Center, LP_1.To);

      A : constant Arc_Params_Type :=
         Cartesian (From   => (if Direction = Dir_CW then LP_1.To else LP_2.To),
                    To     => (if Direction = Dir_CW then LP_2.To else LP_1.To),
                    Center => Center,
                    Radius => R,
                    Large  => (if Direction = Dir_CW then Diff_Angle >= 180.0 else Diff_Angle <= 180.0),
                    Sweep  => True);

      Label_Length : constant Natural := (if Label'Length > Label_Type'Last - Label_Type'First + 1
                                          then Label_Type'Last - Label_Type'First + 1
                                          else Label'Length);
   begin
      return
         (LP_1       => LP_1,
          LP_2       => LP_2,
          Arc        => A,
          Dir        => Direction,
          Center     => Center,
          Label_Text => Label (Label'First .. Label'First + Label_Length - 1)
                               & (Label_Type'First + Label_Length .. Label_Type'Last => ' '),
          Label_Len  => Label_Length);
   end Cartesian;

   ---------------
   -- Get_Label --
   ---------------

   function Get_Label (Params : Connector_Params_Type) return String is (Params.Label_Text (1 .. Params.Label_Len));

   -------------
   -- Get_Arc --
   -------------

   function Get_Arc (Params : Connector_Params_Type'Class) return Arc_Params_Type is (Params.Arc);

   ---------------
   -- Connector --
   ---------------

   function Connector (Params   : Connector_Params_Type;
                       ID       : String;
                       Class    : String         := "";
                       Text     : String         := "";
                       Align    : SVG.Align_Type := SVG.Align_Centered;
                       Position : Pos_Type       := Pos_Outer) return SXML.Document_Type
   is
      use Types;
      use SXML.Generator;

      DY : constant Types.Length := (if Params.Dir = Dir_CW
                                     then (if Position = Pos_Outer then (Em, -0.3) else (Em, 1.0))
                                     else (if Position = Pos_Outer then (Em, -0.4) else (Em, 0.3)));

   begin
      return SVG.Group (Line (Params       => Params.LP_1,
                              ID           => ID & "_1",
                              Class        => "connector connector_start" & (if Class = "" then "" else " " & Class))

                       + Arc (Params => Params.Arc,
                              ID     => ID & "_2",
                              Class        => "connector connector_arc" & (if Class = "" then "" else " " & Class))

                       + (if Text /= ""
                          then SVG.Text (Params.Center,
                                         Data      => Text,
                                         Align     => Align,
                                         DY        => DY,
                                         Path_Name => ID & "_2",
                                         Class     => (if Class = "" then "connector" else "connector " & Class))
                          else SXML.Null_Document)

                       + Line (Params => Points (Params.LP_2.From, Params.LP_2.To),
                               ID     => ID & "_3",
                               Class  => "connector connector_end" & (if Class = "" then "" else " " & Class)),
                        ID => ID);
   end Connector;

   ------------
   -- Points --
   ------------

   function Points (Start : Types.Point;
                    Stop  : Types.Point) return Line_Params_Type is (Start, Stop, 0);

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

   function Annular_Sector (Params    : Annular_Sector_Params_Type;
                            ID        : String := "";
                            Class     : String := "";
                            Text      : String := "") return SXML.Document_Type
   is
      use SVG;
      use SXML.Generator;
      use Types;

   begin
      return Group (Path (Commands =>
                          ((Moveto, Absolute, Params.Inner.From.X, Params.Inner.From.Y),
                           (Arc, Absolute, RX         => Params.Inner.Radius,
                                           RY         => Params.Inner.Radius,
                                           X_Rotation => Params.Inner.X_Rotation,
                                           Large      => Params.Inner.Large,
                                           Sweep      => Params.Inner.Sweep,
                                           AX         => Params.Inner.To.X,
                                           AY         => Params.Inner.To.Y),
                           (Lineto, Absolute, Params.Outer.To.X, Params.Outer.To.Y),
                           (Arc, Absolute, RX         => Params.Outer.Radius,
                                           RY         => Params.Outer.Radius,
                                           X_Rotation => Params.Outer.X_Rotation,
                                           Large      => Params.Outer.Large,
                                           Sweep      => not Params.Outer.Sweep,
                                           AX         => Params.Outer.From.X,
                                           AY         => Params.Outer.From.Y),
                           (ZClosepath, Absolute)
                          ),
                          ID    => ID & "_Path",
                          Class => (if Class = "" then "annular_sector" else Class & " annular_sector"))
                       + (if Text /= ""
                          then Arc (Params => Params.Inner,
                                    ID     => ID & "_Arc",
                                    Class  => "annular_sector_arc")
                             + SVG.Text (Params.Inner.From,
                                         Data      => Text,
                                         DY        => (Em, -0.5),
                                         Path_Name => ID & "_Arc",
                                         Class     => "annular_sector_text")
                          else SXML.Null_Document),
                    ID    => ID);

   end Annular_Sector;

   ----------
   -- Port --
   ----------

   function Port (Params    : Arc_Params_Type;
                  Port_No   : Positive;
                  Num_Ports : Natural) return Types.Point with
      Pre => Port_No <= Num_Ports;

   function Port (Params    : Arc_Params_Type;
                  Port_No   : Positive;
                  Num_Ports : Natural) return Types.Point
   is
      Start      : constant Types.Angle := To_Angle (Params.Center, Params.From);
      Stop       : constant Types.Angle := To_Angle (Params.Center, Params.To);
      Difference : constant Types.Angle := Types.Difference (Start, Stop);
      Step       : constant Types.Angle := Difference / Types.Angle (Num_Ports);
      Angle      : constant Types.Angle := Start + Step / 2.0 + Step * (Types.Angle (Port_No) - 1.0);
   begin
      return Polar (Params.Center, Params.Radius, Angle);
   end Port;

   ----------
   -- Port --
   ----------

   function Port (Params    : Annular_Sector_Params_Type;
                  Position  : Pos_Type;
                  Port_No   : Positive;
                  Num_Ports : Natural) return Types.Optional_Point
   is (case Position is
       when Pos_Invalid => Types.OP (Params.Inner.Center),
       when Pos_Inner   => (if Num_Ports > 0
                            then Types.OP (Params.Inner.Port (Port_No, Num_Ports))
                            else Types.Invalid_Point),
       when Pos_Outer   => (if Num_Ports > 0
                            then Types.OP (Params.Outer.Port (Port_No, Num_Ports))
                            else Types.Invalid_Point));

end SCSC.Primitives;

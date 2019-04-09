with SXML;
with SCSC.SVG;
with SCSC.Types;

package SCSC.Primitives
   with SPARK_Mode => On
is
   type Dir_Type is (Dir_Invalid, Dir_CW, Dir_CCW);
   type Pos_Type is (Pos_Invalid, Pos_Outer, Pos_Inner);

   type Params_Type is tagged private;

   function From (Params : Params_Type) return Types.Point;
   --  Return From point

   function To (Params : Params_Type) return Types.Point;
   --  Return To point

   function Length (Params : Params_Type) return Natural;
   --  Return length

   type Arc_Params_Type is new Params_Type with private;
   type Line_Params_Type is new Params_Type with private;
   type Annular_Sector_Params_Type is tagged private;
   type Connector_Params_Type is tagged private;

   function Arc (Params : Arc_Params_Type;
                 ID     : String := "";
                 Class  : String := "") return SXML.Document_Type;
   --  Return arc

   function Line (Params       : Line_Params_Type;
                  ID           : String := "";
                  Class        : String := "") return SXML.Document_Type;
   --  Return line

   function Cartesian (From       : Types.Point;
                       To         : Types.Point;
                       Radius     : Natural;
                       X_Rotation : Natural := 0;
                       Large      : Boolean := False;
                       Sweep      : Boolean := False) return Arc_Params_Type;
   --  Create arc parameters from cartesian coordinates

   function Polar (Center : Types.Point;
                   Radius : Natural;
                   Angle  : Types.Angle) return Types.Point;
   --  Create point from center point, radius and angle

   function Polar (Center : Types.Point;
                   Radius : Natural;
                   Start  : Types.Angle;
                   Stop   : Types.Angle) return Arc_Params_Type;
   --  Create arc parameters from center point, radius and two angles

   function Cartesian (Center : Types.Point;
                       Start  : Types.Point;
                       Length : Integer) return Line_Params_Type;
   --  Create line parameters from center point, start point and length

   function Polar (Center : Types.Point;
                   Start  : Natural;
                   Stop   : Natural;
                   Angle  : Types.Angle) return Line_Params_Type;
   --  Create line parameters from center point, start radius, end radius and an angle

   function Points (Start : Types.Point;
                    Stop  : Types.Point) return Line_Params_Type;
   --  Create line parameters from two points

   function Polar (Center : Types.Point;
                   Offset : Natural;
                   Radius : Natural;
                   Start  : Types.Angle;
                   Stop   : Types.Angle) return Annular_Sector_Params_Type;
   --  Create annular sector parameters from center point, offset, radius and two angles

   function Inner (Params : Annular_Sector_Params_Type'Class) return Arc_Params_Type;
   --  Return inner arc parameters

   function Cartesian (Center       : Types.Point;
                       Start        : Types.Point;
                       Stop         : Types.Point;
                       Radius       : Integer;
                       Direction    : Dir_Type := Dir_CW) return Connector_Params_Type;
   --  Calculate connector parameters from two points, radius and direction

   function Connector (Params   : Connector_Params_Type;
                       ID       : String;
                       Class    : String         := "";
                       Text     : String         := "";
                       Align    : SVG.Align_Type := SVG.Align_Centered;
                       Position : Pos_Type       := Pos_Outer) return SXML.Document_Type;
   --  Return connector

   function Annular_Sector (Params : Annular_Sector_Params_Type;
                            ID     : String := "";
                            Class  : String := "";
                            Text   : String := "") return SXML.Document_Type;
   --  Return annular sector

   function Port (Params    : Annular_Sector_Params_Type;
                  Position  : Pos_Type;
                  Port_No   : Positive;
                  Num_Ports : Natural) return Types.Optional_Point with
      Pre'Class => Port_No <= Num_Ports,
      Post      => (if Num_Ports > 0 then Port'Result.Valid);
   --  Return coordinate of port

private
   type Params_Type is tagged
   record
      From   : Types.Point;
      To     : Types.Point;
      Length : Natural;
   end record;

   type Arc_Params_Type is new Params_Type with
   record
      Center     : Types.Point;
      Radius     : Natural;
      X_Rotation : Natural;
      Large      : Boolean;
      Sweep      : Boolean;
   end record;

   type Line_Params_Type is new Params_Type with null record;

   type Annular_Sector_Params_Type is tagged
   record
      Inner : Arc_Params_Type;
      Outer : Arc_Params_Type;
   end record;

   type Connector_Params_Type is tagged
   record
      LP_1   : Line_Params_Type;
      LP_2   : Line_Params_Type;
      Arc    : Arc_Params_Type;
      Dir    : Dir_Type;
      Center : Types.Point;
   end record;

end SCSC.Primitives;

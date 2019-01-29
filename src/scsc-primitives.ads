with SCSC.SVG;
with SCSC.Types;

package SCSC.Primitives
   with SPARK_Mode => On
is
   type Params_Type is tagged private;

   function From (Params : Params_Type) return Types.Point;
   --  Return From point

   function To (Params : Params_Type) return Types.Point;
   --  Return To point

   type Arc_Params_Type is new Params_Type with private;
   type Line_Params_Type is new Params_Type with private;

   function Arc (Params : Arc_Params_Type;
                 Style  : String  := "";
                 ID     : String  := "") return SVG.Element_Type;
   --  Return arc

   function Line (Params : Line_Params_Type;
                  Style  : String  := "";
                  ID     : String  := "") return SVG.Element_Type;
   --  Return line

   function Cartesian (From       : Types.Point;
                       To         : Types.Point;
                       X_Radius   : Natural;
                       Y_Radius   : Natural;
                       X_Rotation : Natural := 0;
                       Large      : Boolean := False;
                       Sweep      : Boolean := False) return Arc_Params_Type;
   --  Create arc parameters from cartesian coordinates

   function Polar (Center     : Types.Point;
                   Radius     : Natural;
                   Start      : Types.Angle;
                   Stop       : Types.Angle) return Arc_Params_Type;
   --  Create arc parameters from center point, radius and two angles

   function Polar (Center : Types.Point;
                   Start  : Natural;
                   Stop   : Natural;
                   Angle  : Types.Angle) return Line_Params_Type;
   --  Create line parameters from center point, start radius, end radius and an angle

private
   type Params_Type is tagged
   record
      From       : Types.Point;
      To         : Types.Point;
   end record;

   type Arc_Params_Type is new Params_Type with
   record
      X_Radius   : Natural;
      Y_Radius   : Natural;
      X_Rotation : Natural;
      Large      : Boolean;
      Sweep      : Boolean;
   end record;

   type Line_Params_Type is new Params_Type with null record;

end SCSC.Primitives;

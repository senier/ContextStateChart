with SCSC.SVG;
with SCSC.Types;

package SCSC.Primitives
   with SPARK_Mode => On
is
   type Arc_Params_Type is tagged private;

   function Arc (Params : Arc_Params_Type;
                 Style  : String  := "";
                 ID     : String  := "") return SVG.Element_Type;
   --  Return arc from two points and radius

   function From (Params : Arc_Params_Type) return Types.Point;
   --  Return From point

   function To (Params : Arc_Params_Type) return Types.Point;
   --  Return To point

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
                   Angle      : Types.Angle) return Arc_Params_Type;
   --  Create arc parameters from center point, radius and two angles

private

   type Arc_Params_Type is tagged
   record
      From       : Types.Point;
      To         : Types.Point;
      X_Radius   : Natural;
      Y_Radius   : Natural;
      X_Rotation : Natural;
      Large      : Boolean;
      Sweep      : Boolean;
   end record;

end SCSC.Primitives;

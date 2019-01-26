with SCSC.SVG;
with SCSC.Types;

package SCSC.Primitives
is
   function Arc (From       : Types.Point;
                 To         : Types.Point;
                 X_Radius   : Natural;
                 Y_Radius   : Natural;
                 X_Rotation : Natural := 0;
                 Large      : Boolean := False;
                 Sweep      : Boolean := False;
                 Style      : String  := "") return SCSC.SVG.Element_Type;
   --  Return arc

   function Arc (Center     : Types.Point;
                 Radius     : Natural;
                 Start      : Types.Angle;
                 Angle      : Types.Angle;
                 From       : out Types.Point;
                 To         : out Types.Point;
                 Style      : String  := "") return SCSC.SVG.Element_Type;
   --  Return arc using three points

end SCSC.Primitives;

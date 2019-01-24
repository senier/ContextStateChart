with SCSC.SVG;

package SCSC.Primitives
is
   function Arc (From       : SVG.Point;
                 To         : SVG.Point;
                 X_Radius   : Natural;
                 Y_Radius   : Natural;
                 X_Rotation : Natural := 0;
                 Large      : Boolean := False;
                 Sweep      : Boolean := False;
                 Style      : String  := "") return SCSC.SVG.Element_Type;
   --  Return arc

   -- function Arc (Center     : SVG.Point;
   --               From       : SVG.Point;
   --               To         : SVG.Point;
   --               Style      : String  := "") return SCSC.SVG.Element_Type;
   --  Return arc using three points

end SCSC.Primitives;

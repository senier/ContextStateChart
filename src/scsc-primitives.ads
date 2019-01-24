with SCSC.SVG;

package SCSC.Primitives
is
   type Point is
   record
      X : Natural;
      Y : Natural;
   end record;

   function Arc (From       : Point;
                 To         : Point;
                 X_Radius   : Natural;
                 Y_Radius   : Natural;
                 X_Rotation : Natural := 0;
                 Large      : Boolean := False;
                 Sweep      : Boolean := False;
                 Style      : String  := "") return SCSC.SVG.Element_Type;
   --  Return arc

end SCSC.Primitives;

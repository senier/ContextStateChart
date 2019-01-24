with SXML.Generator;

package body SCSC.Primitives
is
   ---------
   -- Arc --
   ---------

   function Arc (From       : Point;
                 To         : Point;
                 X_Radius   : Natural;
                 Y_Radius   : Natural;
                 X_Rotation : Natural := 0;
                 Large      : Boolean := False;
                 Sweep      : Boolean := False) return SCSC.SVG.Element_Type
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
                         Style => "stroke:#660000; fill:none;");
   end Arc;

end SCSC.Primitives;

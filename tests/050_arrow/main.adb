with Ada.Text_IO; use Ada.Text_IO;
with SCSC.SVG;
with SCSC.Primitives;
with SCSC.Types;

procedure Main
is
   use SCSC.Primitives;
   use SCSC.SVG;
   use SCSC.Types;

   Center : Point := (200, 200);

   Arrow_End : Element_Type := Path (Commands => ((Moveto, Absolute, 0, 0),
                                                 (Vertical, Absolute, 4),
                                                 (Lineto, Absolute, 4, 2),
                                                 (ZClosepath, Absolute)),
                                    Style => "fill: blue");

   Arrow_Start : Element_Type := Path (Commands => ((Moveto, Absolute, 0, 2),
                                                  (Lineto, Absolute, 4, 4),
                                                  (Lineto, Absolute, 4, 0),
                                                  (ZClosepath, Absolute)),
                                     Style => "fill: blue");

   SVG : SCSC.SVG.Document_Type := SCSC.SVG.SVG
      (
       Width  => 400,
       Height => 400,

       Child  => Circle (Center, 2,   Style => "fill: black; stroke")

               + Line (Polar (Center, 20, 180, 45.0),
                       Style        => "fill: none; stroke: blue",
                       Marker_Start => "Arrow_Start")

               + Line (Polar (Center, 20, 60, 120.0),
                       Style      => "fill: none; stroke: blue",
                       Marker_End => "Arrow_End")

               + Line (Polar (Center, 60, 120, 70.0),
                       Style        => "fill: none; stroke: blue",
                       Marker_Start => "Arrow_Start",
                       Marker_End   => "Arrow_End"),

       Defs   => Marker (Element => Arrow_End, Width => 4, Height => 4, RefX => 0.1, RefY => 2.0, ID => "Arrow_End")
               + Marker (Element => Arrow_Start, Width => 4, Height => 4, RefX => 0.1, RefY => 2.0, ID => "Arrow_Start")
      );
begin
   Put_Line (SCSC.SVG.To_String (SVG));
end Main;
with Ada.Text_IO; use Ada.Text_IO;
with SCSC.SVG;
with SCSC.Primitives;
with SCSC.Types;

procedure Main
is
   use SCSC.Primitives;
   use SCSC.SVG;
   use SCSC.Types;

   Center  : Point := P (200, 200);
   Start_1 : Point := P (117, 185);
   Stop_1  : Point := P (238, 100);
   Start_2 : Point := P (100, 160);
   Stop_2  : Point := P (180, 20);
   Start_3 : Point := P (200, 140);
   Stop_3  : Point := P (300, 20);
   Start_4 : Point := P (150, 200);
   Stop_4  : Point := P (180, 250);

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

   SVG : SCSC.SVG.Document_Type := SCSC.SVG.Create_SVG
      (
       Width  => 400,
       Height => 400,

       Child  => Circle (Center,   2, Style => "fill: black; stroke: none")
               + Circle (Center, 200, Style => "fill: none; stroke: black")

               + Circle (Start_1, 2, Style => "fill: red; stroke: none")
               + Circle (Stop_1, 2, Style => "fill: green; stroke: none")
               + Connector (Center, Start_1, Stop_1, Radius => 50, Style => "fill: none; stroke: red")

               + Circle (Start_2, 2, Style => "fill: red; stroke: none")
               + Circle (Stop_2, 2, Style  => "fill: green; stroke: none")
               + Connector (Center, Start_2, Stop_2,
                            Radius       => 30,
                            Style        => "fill: none; stroke: blue",
                            Marker_Start => "Arrow_Start",
                            Marker_End   => "Arrow_End")

               + Circle (Start_3, 2, Style => "fill: red; stroke: none")
               + Circle (Stop_3, 2, Style => "fill: green; stroke: none")
               + Connector (Center, Stop_3, Start_3,
                            Radius       => -30,
                            Style        => "fill: none; stroke: green",
                            Marker_Start => "Arrow_Start",
                            Marker_End   => "Arrow_Start")

               + Circle (Start_4, 2, Style => "fill: red; stroke: none")
               + Circle (Stop_4, 2, Style => "fill: green; stroke: none")
               + Connector (Center, Start_4, Stop_4,
                            Radius       => -80,
                            Style        => "fill: none; stroke: orange",
                            Marker_Start => "Arrow_Start",
                            Marker_End   => "Arrow_Start"),

       Defs   => Marker (Element => Arrow_End,   Width => 4, Height => 4, RefX => 0.1, RefY => 2.0, ID => "Arrow_End")
               + Marker (Element => Arrow_Start, Width => 4, Height => 4, RefX => 0.1, RefY => 2.0, ID => "Arrow_Start")
      );
begin
   Put_Line (SCSC.SVG.To_String (SVG));
end Main;

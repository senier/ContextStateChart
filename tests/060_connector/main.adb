with Ada.Text_IO; use Ada.Text_IO;
with SCSC.SVG;
with SCSC.Primitives;
with SCSC.Types;
with SXML.Generator;

procedure Main
is
   use SCSC.Primitives;
   use SCSC.SVG;
   use SCSC.Types;
   use SXML.Generator;

   Center  : Point := P (200, 200);
   Start_1 : Point := P (117, 185);
   Stop_1  : Point := P (238, 100);
   Start_2 : Point := P (100, 160);
   Stop_2  : Point := P (180, 20);
   Start_3 : Point := P (200, 140);
   Stop_3  : Point := P (300, 20);
   Start_4 : Point := P (150, 200);
   Stop_4  : Point := P (180, 250);

   Arrow_End : Document_Type := Path (Commands => ((Moveto, Absolute, 0, 0),
                                                   (Vertical, Absolute, 4),
                                                   (Lineto, Absolute, 4, 2),
                                                   (ZClosepath, Absolute)),
                                      Class => "arrow");

   Arrow_Start : Document_Type := Path (Commands => ((Moveto, Absolute, 0, 2),
                                                     (Lineto, Absolute, 4, 4),
                                                     (Lineto, Absolute, 4, 0),
                                                     (ZClosepath, Absolute)),
                                        Class => "arrow");

   SVG : SCSC.SVG.Document_Type := SCSC.SVG.Create_SVG
      (
       Width  => 400,
       Height => 400,

       Child  => Circle (Center,   2)
               + Circle (Center, 200, ID => "border")

               + Circle (Start_1, 2, Class => "red_filled")
               + Circle (Stop_1, 2, Class => "green_filled")
               + Connector (Center, Start_1, Stop_1, Radius => 50, Class => "red", ID => "C1")

               + Circle (Start_2, 2, Class => "red_filled")
               + Circle (Stop_2, 2, Class => "green_filled")
               + Connector (Center, Start_2, Stop_2, Radius => 30, Class => "blue", ID => "C2")

               + Circle (Start_3, 2, Class => "red_filled")
               + Circle (Stop_3, 2, Class => "green_filled")
               + Connector (Center, Stop_3, Start_3, Radius => -30, Class => "green", ID => "C3")

               + Circle (Start_4, 2, Class => "red_filled")
               + Circle (Stop_4, 2, Class => "green_filled")
               + Connector (Center, Start_4, Stop_4, Radius => -80, Class => "orange", ID => "C4"),

       Defs   => Marker (Element => Arrow_End,   Width => 4, Height => 4, RefX => 0.1, RefY => 2.0, ID => "Arrow_End")
               + Marker (Element => Arrow_Start, Width => 4, Height => 4, RefX => 0.1, RefY => 2.0, ID => "Arrow_Start"),

       Style => "circle { fill: none; stroke: black; } .arrow { fill: blue; } "
                & ".connector_end { marker-start: url(#Arrow_Start); } "
                & ".connector_start { marker-start: url(#Arrow_Start); } "
                & "#border { fill: none; stroke: black; } .red_filled { fill: red; stroke: none; } "
                & ".blue { stroke: blue; fill: none; } .orange { stroke: orange; fill: none; } "
                & ".green_filled { stroke: none; fill: green; } .green { stroke: green; fill: none; } "
                & ".red { stroke: red; fill: none; }"
      );
begin
   Put_Line (SCSC.SVG.To_String (SVG));
end Main;

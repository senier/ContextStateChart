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

   Center : Point := P (200, 200);

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

       Child  => Circle (Center, 2)

               + Line (Polar (Center, 20, 180, 45.0), ID => "l1")

               + Line (Polar (Center, 20, 60, 120.0), ID => "l2")

               + Line (Polar (Center, 60, 120, 70.0), ID => "l3"),

       Defs   => Marker (Element => Arrow_End, Width => 4, Height => 4, RefX => 0.1, RefY => 2.0, ID => "Arrow_End")
               + Marker (Element => Arrow_Start, Width => 4, Height => 4, RefX => 0.1, RefY => 2.0, ID => "Arrow_Start"),

       Style => ".line { fill: none; stroke: blue; } circle { fill: black; } .arrow { fill: blue } "
                & "#l1 { marker-start: url(#Arrow_Start); } #l3 { marker-end: url(#Arrow_End); }"
                & "#l2 { marker-start: url(#Arrow_Start); marker-end: url(#Arrow_End); }"
      );
begin
   Put_Line (SCSC.SVG.To_String (SVG));
end Main;

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

       Child  => Circle (Center,   2, Class => "black_circle")
               + Circle (Center, 200, ID => "border")

               + Connector (Cartesian (Center, P (56, 300), P (68, 116), 10),
                            Text      => "Outer/Default",
                            ID        => "C1")

               + Connector (Cartesian (Center, P (65, 100), P (180, 50), 20),
                            Text      => "Outer/Start/CW",
                            Align     => SCSC.SVG.Align_Start,
                            ID        => "C2")

               + Connector (Cartesian (Center, P (200, 30), P (320, 100), 15),
                            Text      => "Outer/Center/CW",
                            Align     => SCSC.SVG.Align_Centered,
                            ID        => "C3")

               + Circle (P (340, 130), 5, Class => "red_circle")
               + Circle (P (380, 240), 5, Class => "green_circle")

               + Connector (Cartesian (Center, P (340, 130), P (380, 240), 15),
                            Text      => "Outer/End/CW",
                            Align     => SCSC.SVG.Align_End,
                            ID        => "C4")

               + Circle (P (360, 250), 5, Class => "red_circle")
               + Circle (P (270, 330), 5, Class => "green_circle")

               + Connector (Cartesian (Center, P (270, 330), P (360, 250), 15, Dir_CCW),
                            Text      => "Outer/Start/CCW",
                            Align     => SCSC.SVG.Align_Start,
                            ID        => "C5")

               + Connector (Cartesian (Center, P (140, 330), P (250, 340), 25, Dir_CCW),
                            Text      => "Outer/Center/CCW",
                            Align     => SCSC.SVG.Align_Centered,
                            ID        => "C6")

               + Connector (Cartesian (Center, P (80, 130), P (140, 280), 15, Dir_CCW),
                            Text      => "Outer/End/CCW",
                            Align     => SCSC.SVG.Align_End,
                            Position  => Pos_Outer,
                            ID        => "C7")
               ,

       Defs   => Marker (Element => Arrow_End,   Width => 4, Height => 4, RefX => 0.1, RefY => 2.0, ID => "Arrow_End")
               + Marker (Element => Arrow_Start, Width => 4, Height => 4, RefX => 0.1, RefY => 2.0, ID => "Arrow_Start"),

       Style => "circle { fill: none; stroke: black; }"
                & ".connector_end { marker-start: url(#Arrow_Start); } "
                & ".connector_start { marker-start: url(#Arrow_Start); } "
                & ".connector { fill: none; stroke: red; } "
                & ".text { font-size: 10px; fill: blue; stroke: none; } "
                & ".arrow { fill: blue; } "
                & "#border { stroke: black; fill: none; } "
                & ".black_circle { fill: black; stroke: none; } "
                & ".red_circle { fill: red; stroke: none; } "
                & ".green_circle { fill: green; stroke: none; } "
      );
begin
   Put_Line (SCSC.SVG.To_String (SVG));
end Main;

with Ada.Text_IO; use Ada.Text_IO;
with SCSC.SVG;
with SCSC.Types;
with SCSC.Primitives;
with SXML.Generator;

procedure Main
is
   use SCSC.Primitives;
   use SCSC.SVG;
   use SCSC.Types;
   use SXML.Generator;
   use type SCSC.Types.Angle;

   Center : SCSC.Types.Point := P (200, 200);

   Params1 : Arc_Params_Type := Polar (Center => Center,
                                       Radius => 100,
                                       Start  => 0.0,
                                       Stop   => 240.0);

   Arc1 : Document_Type := Arc (Params1);

   Params2 : Arc_Params_Type := Polar (Center => Center,
                                       Radius => 60,
                                       Start  => 30.0,
                                       Stop   => 100.0);

   Arc2 : Document_Type := Arc (Params2);

   Params3 : Arc_Params_Type := Polar (Center => Center,
                                       Radius => 30,
                                       Start  => 120.0,
                                       Stop   => 140.0);

   Arc3 : Document_Type := Arc (Params3);

   Params4 : Line_Params_Type := Cartesian (Center, P (180, 180), 100);
   Params5 : Line_Params_Type := Cartesian (Center, P (250, 250), 50);
   Params6 : Line_Params_Type := Polar (Center, 20, 120, 90.0);

   SVG : SCSC.SVG.Document_Type := SCSC.SVG.Create_SVG
      (Width  => 400,
       Height => 400,
       Child  => Group (Circle (Center, 5)
                        + Arc1
                        + Circle (Params1.From, 2, ID => "c1")
                        + Circle (Params1.To, 2, ID => "c2")
                        + Text (Params1.To, Params1.Length'Img, SCSC.SVG.Align_End)

                        + Arc2
                        + Circle (Params2.From, 2, ID => "c3")
                        + Circle (Params2.To, 2, ID => "c4")
                        + Text (Params2.To, Params2.Length'Img, SCSC.SVG.Align_End)

                        + Arc3
                        + Circle (Params3.From, 2, ID => "c5")
                        + Circle (Params3.To, 2, ID => "c6")
                        + Text (Params3.To, Params3.Length'Img, SCSC.SVG.Align_End)

                        + Line (Params4, Class => "l1")
                        + Text (Params4.To, Params4.Length'Img, SCSC.SVG.Align_End)

                        + Line (Params5, Class => "l2")
                        + Text (Params5.To, Params5.Length'Img, SCSC.SVG.Align_End)

                        + Line (Params6, Class => "l3")
                        + Text (Params6.To, Params6.Length'Img, SCSC.SVG.Align_End)),

       Style => ".arc { stroke: #ff0000; fill: none; } .line { stroke: black; fill: none; } "
                & "#c1 { fill: green; } #c2 { fill: blue; } "
                & "#c3 { fill: orange; } #c4 { fill: purple; } "
                & "#c5 { fill: magenta; } #c6 { fill: cyan; } ");
begin
   Put_Line (SCSC.SVG.To_String (SVG));
end Main;

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

   SVG : SCSC.SVG.Document_Type := SCSC.SVG.Create_SVG
      (Width  => 400,
       Height => 400,
       Child  => Group (Circle (Center, 5) +
                        Arc1 + Circle (Params1.From, 2, ID => "c1") + Circle (Params1.To, 2, ID => "c2") +
                        Arc2 + Circle (Params2.From, 2, ID => "c3") + Circle (Params2.To, 2, ID => "c4") +
                        Arc3 + Circle (Params3.From, 2, ID => "c5") + Circle (Params3.To, 2, ID => "c6")),
       Style => ".arc { stroke: #ff0000; fill: none; } "
                & "#c1 { fill: green; } #c2 { fill: blue; } "
                & "#c3 { fill: orange; } #c4 { fill: purple; } "
                & "#c5 { fill: magenta; } #c6 { fill: cyan; } ");
begin
   Put_Line (SCSC.SVG.To_String (SVG));
end Main;

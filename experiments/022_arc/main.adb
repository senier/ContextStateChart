with Ada.Text_IO; use Ada.Text_IO;
with SCSC.SVG;
with SCSC.Types;
with SCSC.Primitives;

procedure Main
is
   use SCSC.Primitives;
   use SCSC.SVG;
   use type SCSC.Types.Angle;

   Center : SCSC.Types.Point := (200, 200);
   From1, To1, From2, To2, From3, To3 : SCSC.Types.Point;

   Arc1 : Element_Type := Arc (Center => Center,
                               Radius => 100,
                               Start  => 0.0,
                               Angle  => -90.0,
                               From   => From1,
                               To     => To1,
                               Style  => "stroke: #ff0000; fill: none");

   Arc2 : Element_Type := Arc (Center => Center,
                               Radius => 60,
                               Start  => 30.0,
                               Angle  => 70.0,
                               From   => From2,
                               To     => To2,
                               Style  => "stroke: #00ffff; fill: none");

   Arc3 : Element_Type := Arc (Center => Center,
                               Radius => 30,
                               Start  => 120.0,
                               Angle  => 20.0,
                               From   => From3,
                               To     => To3,
                               Style  => "stroke: #00ff00; fill: none");

   SVG : SCSC.SVG.Document_Type := SCSC.SVG.SVG
      (Width  => 400,
       Height => 400,
       Child  => Group (Circle (Center, 5, "fill:red") +
                        Arc1 + Circle (From1, 2, "fill:green") + Circle (To1, 2, "fill:blue") +
                        Arc2 + Circle (From2, 2, "fill:orange") + Circle (To2, 2, "fill:purple") +
                        Arc3 + Circle (From3, 2, "fill:magenta") + Circle (To3, 2, "fill:cyan")));
begin
   Put_Line (SCSC.SVG.To_String (SVG));
end Main;

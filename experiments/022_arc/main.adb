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

   Params1 : Arc_Params_Type := Polar (Center => Center,
                                       Radius => 100,
                                       Start  => 0.0,
                                       Angle  => -90.0);

   Arc1 : Element_Type := Arc (Params1, Style  => "stroke: #ff0000; fill: none");

   Params2 : Arc_Params_Type := Polar (Center => Center,
                                       Radius => 60,
                                       Start  => 30.0,
                                       Angle  => 70.0);

   Arc2 : Element_Type := Arc (Params2, Style  => "stroke: #00ffff; fill: none");

   Params3 : Arc_Params_Type := Polar (Center => Center,
                                       Radius => 30,
                                       Start  => 120.0,
                                       Angle  => 20.0);

   Arc3 : Element_Type := Arc (Params3, Style  => "stroke: #00ff00; fill: none");

   SVG : SCSC.SVG.Document_Type := SCSC.SVG.SVG
      (Width  => 400,
       Height => 400,
       Child  => Group (Circle (Center, 5, "fill:red") +
                        Arc1 + Circle (Params1.From, 2, "fill:green") + Circle (Params1.To, 2, "fill:blue") +
                        Arc2 + Circle (Params2.From, 2, "fill:orange") + Circle (Params2.To, 2, "fill:purple") +
                        Arc3 + Circle (Params3.From, 2, "fill:magenta") + Circle (Params3.To, 2, "fill:cyan")));
begin
   Put_Line (SCSC.SVG.To_String (SVG));
end Main;

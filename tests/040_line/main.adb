with Ada.Text_IO; use Ada.Text_IO;
with SCSC.SVG;
with SCSC.Primitives;
with SCSC.Types;

procedure Main
is
   use SCSC.Primitives;
   use SCSC.SVG;
   use SCSC.Types;

   Center : Point := P (200, 200);

   SVG : SCSC.SVG.Document_Type := SCSC.SVG.Create_SVG
      (
       Width  => 400,
       Height => 400,
       Child  => Circle (Center, 2,   Style => "fill: black; stroke: black")
               + Circle (Center, 200, Style => "fill: none; stroke: black")

               + Line (Polar (Center, 20, 180, 45.0), Style => "fill: none; stroke: blue")
               + Line (Polar (Center, 50, 120, 60.0), Style => "fill: none; stroke: red")
               + Line (Polar (Center,  0,  80, 140.0), Style => "fill: none; stroke: green")

               + Line (Polar (Center,  180, 200, 160.0), Style => "fill: none; stroke: green")
               + Line (Polar (Center,  180, 200, 170.0), Style => "fill: none; stroke: green")
               + Line (Polar (Center,  180, 200, 180.0), Style => "fill: none; stroke: green")
               + Line (Polar (Center,  180, 200, 190.0), Style => "fill: none; stroke: green")
               + Line (Polar (Center,  180, 200, 200.0), Style => "fill: none; stroke: green")
               + Line (Polar (Center,  180, 200, 210.0), Style => "fill: none; stroke: green")
               + Line (Polar (Center,  180, 200, 220.0), Style => "fill: none; stroke: green")
               + Line (Polar (Center,  180, 200, 230.0), Style => "fill: none; stroke: green")
      );
begin
   Put_Line (SCSC.SVG.To_String (SVG));
end Main;

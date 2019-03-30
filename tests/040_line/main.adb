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

   SVG : SCSC.SVG.Document_Type := SCSC.SVG.Create_SVG
      (
       Width  => 400,
       Height => 400,
       Child  => Circle (Center, 2, ID => "filled_circle")
               + Circle (Center, 200)

               + Line (Polar (Center, 20, 180, 45.0), ID => "blue_line")
               + Line (Polar (Center, 50, 120, 60.0), ID => "red_line")
               + Line (Polar (Center,  0,  80, 140.0))

               + Line (Polar (Center,  180, 200, 160.0))
               + Line (Polar (Center,  180, 200, 170.0))
               + Line (Polar (Center,  180, 200, 180.0))
               + Line (Polar (Center,  180, 200, 190.0))
               + Line (Polar (Center,  180, 200, 200.0))
               + Line (Polar (Center,  180, 200, 210.0))
               + Line (Polar (Center,  180, 200, 220.0))
               + Line (Polar (Center,  180, 200, 230.0)),
       Style => ".line { fill: none; stroke: green; } circle { fill: none; stroke: black; } "
                & "#blue_line { stroke: blue; } #red_line { stroke: red; } #filled_circle { fill: black; }"
      );
begin
   Put_Line (SCSC.SVG.To_String (SVG));
end Main;

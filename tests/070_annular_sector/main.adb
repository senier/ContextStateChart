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

   Center : Point := P (200, 200);

   SVG : SCSC.SVG.Document_Type := SCSC.SVG.Create_SVG
      (Width  => 400,
       Height => 400,
       Child  => Circle (Center, 2)
               + Annular_Sector (Params => Polar (Center, 100, 50, 315.0, 45.0), ID => "S1")
               + Annular_Sector (Params => Polar (Center, 100, 200, 315.0, 45.0), ID => "S2")
               + Annular_Sector (Params => Polar (Center, 120, 20, 179.0, 241.0), ID => "S3")
               + Annular_Sector (Params => Polar (Center, 180, 10, 180.0, 170.0), ID => "S4")
               + Annular_Sector (Params => Polar (Center, 10, 150, 60.0, 80.0), ID => "S5")
               + Annular_Sector (Params => Polar (Center, 30, 140, 260.0, 310.0), ID => "S6"),
         Style => "#S1 { fill: yellow; stroke: green; } #S2 { fill: blue; stroke: red; } #S3 { fill: gray; stroke: black; } "
                  & "#S4 { fill: green; stroke: orange; } #S5 { fill: green; stroke: orange; } #S6 { fill: orange; stroke: blue; }"
                  & ".circle { fill: red; stroke: none; } "
      );
begin
   Put_Line (SCSC.SVG.To_String (SVG));
end Main;

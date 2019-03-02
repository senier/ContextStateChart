with Ada.Text_IO; use Ada.Text_IO;
with SCSC.SVG;
with SCSC.Types;
with SCSC.Primitives;

procedure Main
is
   use SCSC.Primitives;
   use SCSC.SVG;
   use SCSC.Types;

   Center : Point := P (200, 200);

   SVG : SCSC.SVG.Document_Type := SCSC.SVG.SVG
      (Width  => 400,
       Height => 400,
       Child  => Circle (Center, 2, Style => "fill: black; stroke: none")
               + Annular_Sector (Params => Polar (Center, 100, 50, 315.0, 45.0),
                                 Style  => "fill: yellow; stroke: green")
               + Annular_Sector (Params => Polar (Center, 100, 200, 315.0, 45.0),
                                 Style  => "fill: blue; stroke: red")
               + Annular_Sector (Params => Polar (Center, 120, 20, 179.0, 241.0),
                                 Style  => "fill: gray; stroke: black")
               + Annular_Sector (Params => Polar (Center, 180, 10, 180.0, 170.0),
                                 Style  => "fill: green; stroke: orange")
               + Annular_Sector (Params => Polar (Center, 10, 150, 60.0, 80.0),
                                 Style  => "fill: green; stroke: orange")
               + Annular_Sector (Params => Polar (Center, 30, 140, 260.0, 310.0),
                                 Style  => "fill: orange; stroke: blue")
      );
begin
   Put_Line (SCSC.SVG.To_String (SVG));
end Main;

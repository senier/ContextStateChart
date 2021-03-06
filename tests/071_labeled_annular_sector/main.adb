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
               + Annular_Sector (Params => Polar (Center, 160, 20, 331.0, 359.0), Text => "Element 1", ID => "S1")
               + Annular_Sector (Params => Polar (Center, 160, 20, 1.0, 29.0), Text => "Element 2", ID => "S5")
               + Annular_Sector (Params => Polar (Center, 160, 20, 31.0, 59.0), Text => "Element 3", ID => "S9")
               + Annular_Sector (Params => Polar (Center, 160, 20, 61.0, 89.0), Text => "Element 4", ID => "S13")
               + Annular_Sector (Params => Polar (Center, 160, 20, 91.0, 119.0), Text => "Element 5", ID => "S17")
               + Annular_Sector (Params => Polar (Center, 160, 20, 121.0, 149.0), Text => "Element 6", ID => "S21")
               + Annular_Sector (Params => Polar (Center, 160, 20, 151.0, 179.0), Text => "Element 7", ID => "S25")
               + Annular_Sector (Params => Polar (Center, 160, 20, 181.0, 209.0), Text => "Element 8", ID => "S29")
               + Annular_Sector (Params => Polar (Center, 160, 20, 211.0, 239.0), Text => "Element 9", ID => "S33")
               + Annular_Sector (Params => Polar (Center, 160, 20, 241.0, 269.0), Text => "Element 10", ID => "S37")
               + Annular_Sector (Params => Polar (Center, 160, 20, 271.0, 299.0), Text => "Element 11", ID => "S41")
               + Annular_Sector (Params => Polar (Center, 160, 20, 301.0, 329.0), Text => "Element 12", ID => "S45"),
       Style => ".annular_sector { fill: yellow; stroke: green; } "
                & ".annular_sector_arc { fill: none; } "
                & ".text { font-size: 10px; fill: green; stroke: none; } "
                & ".circle { fill: black; stroke: none; } "
      );
begin
   Put_Line (SCSC.SVG.To_String (SVG));
end Main;

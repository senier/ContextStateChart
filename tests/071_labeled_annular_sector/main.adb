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
       Child  => Circle (Center, 2, Style => "fill: black; stroke: none")
               + Annular_Sector (Params => Polar (Center, 160, 20, 331.0, 359.0),
                                 Text   => "Element 1", Textstyle => "font-size: 10px",
                                 Style  => "fill: yellow; stroke: green",
                                 ASID   => "S1")
               + Annular_Sector (Params => Polar (Center, 160, 20, 1.0, 29.0),
                                 Text   => "Element 2", Textstyle => "font-size: 10px",
                                 Style  => "fill: yellow; stroke: green",
                                 ASID   => "S5")
               + Annular_Sector (Params => Polar (Center, 160, 20, 31.0, 59.0),
                                 Text   => "Element 3", Textstyle => "font-size: 10px",
                                 Style  => "fill: yellow; stroke: green",
                                 ASID   => "S9")
               + Annular_Sector (Params => Polar (Center, 160, 20, 61.0, 89.0),
                                 Text   => "Element 4", Textstyle => "font-size: 10px",
                                 Style  => "fill: yellow; stroke: green",
                                 ASID   => "S13")
               + Annular_Sector (Params => Polar (Center, 160, 20, 91.0, 119.0),
                                 Text   => "Element 5", Textstyle => "font-size: 10px",
                                 Style  => "fill: yellow; stroke: green",
                                 ASID   => "S17")
               + Annular_Sector (Params => Polar (Center, 160, 20, 121.0, 149.0),
                                 Text   => "Element 6", Textstyle => "font-size: 10px",
                                 Style  => "fill: yellow; stroke: green",
                                 ASID   => "S21")
               + Annular_Sector (Params => Polar (Center, 160, 20, 151.0, 179.0),
                                 Text   => "Element 7", Textstyle => "font-size: 10px",
                                 Style  => "fill: yellow; stroke: green",
                                 ASID   => "S25")
               + Annular_Sector (Params => Polar (Center, 160, 20, 181.0, 209.0),
                                 Text   => "Element 8", Textstyle => "font-size: 10px",
                                 Style  => "fill: yellow; stroke: green",
                                 ASID   => "S29")
               + Annular_Sector (Params => Polar (Center, 160, 20, 211.0, 239.0),
                                 Text   => "Element 9", Textstyle => "font-size: 10px",
                                 Style  => "fill: yellow; stroke: green",
                                 ASID   => "S33")
               + Annular_Sector (Params => Polar (Center, 160, 20, 241.0, 269.0),
                                 Text   => "Element 10", Textstyle => "font-size: 10px",
                                 Style  => "fill: yellow; stroke: green",
                                 ASID   => "S37")
               + Annular_Sector (Params => Polar (Center, 160, 20, 271.0, 299.0),
                                 Text   => "Element 11", Textstyle => "font-size: 10px",
                                 Style  => "fill: yellow; stroke: green",
                                 ASID   => "S41")
               + Annular_Sector (Params => Polar (Center, 160, 20, 301.0, 329.0),
                                 Text   => "Element 12", Textstyle => "font-size: 10px",
                                 Style  => "fill: yellow; stroke: green",
                                 ASID   => "S45")
      );
begin
   Put_Line (SCSC.SVG.To_String (SVG));
end Main;

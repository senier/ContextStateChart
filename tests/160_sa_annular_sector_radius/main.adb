with Ada.Text_IO; use Ada.Text_IO;
with SCSC.SVG;
with SCSC.Types;
with SCSC.Primitives;
with SCSC.SA;
with SXML.Generator;

procedure Main
is
   use SCSC.Primitives;
   use SCSC.SVG;
   use SCSC.Types;
   use SXML.Generator;

   Center : Point := P (200, 300);

   AS1_Params : Annular_Sector_Params_Type := Polar (Center, 240, 20, 331.0, 359.0);
   AS2_Params : Annular_Sector_Params_Type := Polar (Center, 200, 20, 331.0, 359.0);
   AS3_Params : Annular_Sector_Params_Type := Polar (Center, 160, 20, 331.0, 359.0);
   AS4_Params : Annular_Sector_Params_Type := Polar (Center, 120, 20, 331.0, 359.0);
   AS5_Params : Annular_Sector_Params_Type := Polar (Center, 80, 20, 331.0, 359.0);

   AS_Text   : constant String := "Annular Sector";

   Style      : constant String := ".annular_sector { fill: yellow; stroke: green; } "
                                   & ".annular_sector_arc { fill: none; } "
                                   & ".text { font-size: 10px; fill: green; stroke: none; } "
                                   & ".circle { fill: black; stroke: none; } ";
begin
   declare
      Energy1 : constant Natural := SCSC.SA.Energy (AS1_Params, AS_Text);
      Energy2 : constant Natural := SCSC.SA.Energy (AS2_Params, AS_Text);
      Energy3 : constant Natural := SCSC.SA.Energy (AS3_Params, AS_Text);
      Energy4 : constant Natural := SCSC.SA.Energy (AS4_Params, AS_Text);
      Energy5 : constant Natural := SCSC.SA.Energy (AS5_Params, AS_Text);

      SVG : SCSC.SVG.Document_Type := SCSC.SVG.Create_SVG
         (Width  => 400,
          Height => 400,
          Child  => Circle (Center, 2)
                  + Annular_Sector (Params => AS1_Params, Text => AS_Text, ID => "AS1")
                  + Text (P (60, 70), Energy1'Img)
                  + Annular_Sector (Params => AS2_Params, Text => AS_Text, ID => "AS2")
                  + Text (P (80, 110), Energy2'Img)
                  + Annular_Sector (Params => AS3_Params, Text => AS_Text, ID => "AS3")
                  + Text (P (100, 150), Energy3'Img)
                  + Annular_Sector (Params => AS4_Params, Text => AS_Text, ID => "AS4")
                  + Text (P (120, 190), Energy4'Img)
                  + Annular_Sector (Params => AS5_Params, Text => AS_Text, ID => "AS5")
                  + Text (P (140, 230), Energy5'Img),
          Style => Style);
   begin
      Put_Line (SCSC.SVG.To_String (SVG));
   end;
end Main;

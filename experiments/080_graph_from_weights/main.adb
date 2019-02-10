with Ada.Text_IO; use Ada.Text_IO;
with SCSC.SVG;
with SCSC.Types;
with SCSC.Primitives;
with SCSC.Graph;

procedure Main
is
   use SCSC.Primitives;
   use SCSC.SVG;
   use SCSC.Types;
   use SCSC.Graph;

   Center : Point := (200, 200);

   SVG : SCSC.SVG.Document_Type := SCSC.SVG.SVG
      (Width  => 400,
       Height => 400,
       Child  => Circle (Center, 2, Style => "fill: black; stroke: none")
               + Graph (Params => Polar (Center, 120, 50),
                        Data   => (Node (5),
                                   Node (3),
                                   Node (7),
                                   Node (2)),
                        Style  => "fill: yellow; stroke: green")
      );
begin
   Put_Line (SCSC.SVG.To_String (SVG));
end Main;

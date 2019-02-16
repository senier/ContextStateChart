with Ada.Text_IO; use Ada.Text_IO;
with SCSC.SVG;
with SCSC.Types;
with SCSC.Primitives;
with SCSC.Graph;
with SCSC.Graph_Layout;

procedure Main
is
   use SCSC;
   use type SVG.Element_Type;

   Center : Types.Point := (200, 200);

   Doc : SVG.Document_Type := SVG.SVG
      (Width  => 400,
       Height => 400,
       Child  => SVG.Circle (Center, 2, Style => "fill: black; stroke: none")
               + Graph.Graph (Params => Graph.Polar (Center, 120, 20, 20),
                              Data   => (Graph.Node (5),
                                         Graph.Node (3),
                                         Graph.Node (7),
                                         Graph.Node (2)),
                              Layout => Graph_Layout.Initialize,
                              Style  => "fill: yellow; stroke: green")
      );
begin
   Put_Line (SVG.To_String (Doc));
end Main;

with Ada.Text_IO; use Ada.Text_IO;
with SCSC.SVG;
with SCSC.Types;
with SCSC.Primitives;
with SCSC.Graph;
with SXML.Generator;

with GNAT.Traceback;
with GNAT.Traceback.Symbolic;

procedure Main
is
   use SCSC;
   use SCSC.Types;
   use SXML.Generator;

   Center : Types.Point := P (200, 200);

   Doc : SVG.Document_Type := SVG.Create_SVG
      (Width  => 400,
       Height => 400,
       Child  => SVG.Circle (Center, 2)
               + Graph.Create_Graph (Params => Graph.Create_Polar (Center  => Center,
                                                                   Radius  => 20,
                                                                   Spacing => (0 => 140),
                                                                   Padding => 10),
                                     Data   => (Graph.Create_Node (Label => "Node 1", Weight => 5),
                                                Graph.Create_Node (Label => "Node 2", Weight => 3),
                                                Graph.Create_Node (Label => "Node 3", Weight => 7),
                                                Graph.Create_Node (Label => "Node 4", Weight => 2)),
                                     ID     => "G1")
               + Graph.Create_Graph (Params     => Graph.Create_Polar (Center  => Center,
                                                                       Radius  => 15,
                                                                       Spacing => (0 => 60),
                                                                       Padding => 3),
                                     Data       => (Graph.Create_Node (Label => "In. I", Weight => 1),
                                                    Graph.Create_Node (Label => "In. II", Weight => 1),
                                                    Graph.Create_Node (Label => "Inner III", Weight => 5),
                                                    Graph.Create_Node (Label => "Inner IV", Weight => 5),
                                                    Graph.Create_Node (Label => "Inner V", Weight => 2),
                                                    Graph.Create_Node (Label => "Inner VI", Weight => 4)),
                                     ID     => "G2"),
       Style => ".circle { fill: red; stroke: none; } "
                & "#G1 { fill: yellow; stroke: green; }"
                & "#G1 .text { fill: black; stroke: none; font-size: 10px; }"
                & "#G2 { fill: green; stroke: black; }"
                & "#G2 .text { fill: white; stroke: none; font-size: 8px; font-weight: bold; }"
      );
begin
   Put_Line (SVG.To_String (Doc));
end Main;

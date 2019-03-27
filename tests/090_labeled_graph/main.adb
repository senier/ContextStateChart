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
       Child  => SVG.Circle (Center, 2, Style => "fill: black; stroke: none")
               + Graph.Create_Graph (Params => Graph.Create_Polar (Center  => Center,
                                                                   Radius  => 20,
                                                                   Spacing => (0 => 140),
                                                                   Padding => 10),
                                     Data   => (Graph.Create_Node (Label => "Node 1", Weight => 5),
                                                Graph.Create_Node (Label => "Node 2", Weight => 3),
                                                Graph.Create_Node (Label => "Node 3", Weight => 7),
                                                Graph.Create_Node (Label => "Node 4", Weight => 2)),
                                     GID    => "G1",
                                     Style  => "fill: yellow; stroke: green")
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
                                     GID    => "G2",
                                     Text_style => "fill: white; stroke: none; font-size: 8px; font-weight: bold",
                                     Style      => "fill: green; stroke: black")
      );
begin
   Put_Line (SVG.To_String (Doc));
end Main;

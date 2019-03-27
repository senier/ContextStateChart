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
               + Graph.Create_Graph (Params => Graph.Create_Polar (Center   => Center,
                                                                   Spacing  => (40, 30, 60),
                                                                   Radius   => 20),
                                     Data      => (Graph.Create_Node (Label => "Node 1", Weight => 5, Level => 1),
                                                   Graph.Create_Node (Label => "Node 2", Weight => 3, Level => 1),
                                                   Graph.Create_Node (Label => "Node 3", Weight => 7, Level => 1),
                                                   Graph.Create_Node (Label => "Node 4", Weight => 2, Level => 2),
                                                   Graph.Create_Node (Label => "Node 5", Weight => 2, Level => 2),
                                                   Graph.Create_Node (Label => "Node 6", Weight => 5, Level => 2),
                                                   Graph.Create_Node (Label => "Node 7", Weight => 9, Level => 3),
                                                   Graph.Create_Node (Label => "Node 8", Weight => 1, Level => 3),
                                                   Graph.Create_Node (Label => "Node 9", Weight => 2, Level => 3),
                                                   Graph.Create_Node (Label => "Node 10", Weight => 4, Level => 3)),
                                     Positions => (4, 5, 3, 7, 8, 10, 9, 6, 1, 2),
                                     GID       => "G1",
                                     Style     => "fill: yellow; stroke: green")
      );
begin
   Put_Line (SVG.To_String (Doc));
end Main;

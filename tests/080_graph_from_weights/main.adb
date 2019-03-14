with Ada.Text_IO; use Ada.Text_IO;
with SCSC.SVG;
with SCSC.Types;
with SCSC.Primitives;
with SCSC.Graph;

procedure Main
is
   use SCSC;
   use SCSC.Types;
   use type SVG.Element_Type;

   Center : Types.Point := P (200, 200);

   Doc : SVG.Document_Type := SVG.Create_SVG
      (Width  => 400,
       Height => 400,
       Child  => SVG.Circle (Center, 2, Style => "fill: black; stroke: none")
               + Graph.Create_Graph (Params => Graph.Create_Polar (Center, 120, 20, 20),
                                     Data   => (Graph.Create_Node (5),
                                                Graph.Create_Node (3),
                                                Graph.Create_Node (7),
                                                Graph.Create_Node (2)),
                                     Style  => "fill: yellow; stroke: green")
      );
begin
   Put_Line (SVG.To_String (Doc));
end Main;

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

   Data1 : constant Graph.Data_Type := (Graph.Create_Node (Label => "Node 1", Weight => 5),
                                        Graph.Create_Node (Label => "Node 2", Weight => 3),
                                        Graph.Create_Node (Label => "Node 3", Weight => 7),
                                        Graph.Create_Node (Label => "Node 4", Weight => 2));

   Params1 : constant Graph.Graph_Params_Type := Graph.Create_Polar (Center  => Center,
                                                                     Radius  => 20,
                                                                     Spacing => (0 => 140),
                                                                     Padding => 10);

   Data2 : constant Graph.Data_Type := (Graph.Create_Node (Label => "In. I", Weight => 1),
                                        Graph.Create_Node (Label => "In. II", Weight => 1),
                                        Graph.Create_Node (Label => "Inner III", Weight => 5),
                                        Graph.Create_Node (Label => "Inner IV", Weight => 5),
                                        Graph.Create_Node (Label => "Inner V", Weight => 2),
                                        Graph.Create_Node (Label => "Inner VI", Weight => 4));

   Params2 : constant Graph.Graph_Params_Type := Graph.Create_Polar (Center  => Center,
                                                                     Radius  => 15,
                                                                     Spacing => (0 => 60),
                                                                     Padding => 3);

   Sectors1   : Graph.Annular_Sectors_Type (Data1'Range);
   Positions1 : Graph.Positions_Type (Data1'Range);

   Sectors2   : Graph.Annular_Sectors_Type (Data2'Range);
   Positions2 : Graph.Positions_Type (Data2'Range);

   Length1, Length2  : Natural;
   Unused : Long_Integer;

begin

   Graph.Identity (Positions1);
   Graph.Layout (Params    => Params1,
                 Data      => Data1,
                 ID        => "G1",
                 Sectors   => Sectors1,
                 Length    => Length1,
                 Positions => Positions1,
                 Energy    => Unused);

   Graph.Identity (Positions2);
   Graph.Layout (Params    => Params2,
                 Data      => Data2,
                 ID        => "G2",
                 Sectors   => Sectors2,
                 Length    => Length2,
                 Positions => Positions2,
                 Energy    => Unused);

   declare
      Doc : SVG.Document_Type := SVG.Create_SVG
         (Width  => 400,
          Height => 400,
          Child  => SVG.Circle (Center, 2)
                  + Graph.Create_Graph (Params    => Params1,
                                        Data      => Data1,
                                        Sectors   => Sectors1,
                                        Length    => Length1,
                                        ID        => "G1",
                                        Positions => Positions1)
                  + Graph.Create_Graph (Params    => Params2,
                                        Data      => Data2,
                                        Sectors   => Sectors2,
                                        Length    => Length2,
                                        ID        => "G2",
                                        Positions => Positions2),
          Style => ".circle { fill: red; stroke: none; } "
                   & "#G1 { fill: yellow; stroke: green; }"
                   & "#G1 .text { fill: black; stroke: none; font-size: 10px; }"
                   & "#G2 { fill: green; stroke: black; }"
                   & "#G2 .text { fill: white; stroke: none; font-size: 8px; font-weight: bold; }"
         );
   begin
      Put_Line (SVG.To_String (Doc));
   end;
end Main;

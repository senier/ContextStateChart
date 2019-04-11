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

   Data : constant SCSC.Graph.Data_Type := (Graph.Create_Node (Label => "Node 1", Weight => 5, Level => 1),
                                            Graph.Create_Node (Label => "Node 2", Weight => 3, Level => 1),
                                            Graph.Create_Node (Label => "Node 3", Weight => 7, Level => 1),
                                            Graph.Create_Node (Label => "Node 4", Weight => 2, Level => 2),
                                            Graph.Create_Node (Label => "Node 5", Weight => 2, Level => 2),
                                            Graph.Create_Node (Label => "Node 6", Weight => 5, Level => 2),
                                            Graph.Create_Node (Label => "Node 7", Weight => 9, Level => 3),
                                            Graph.Create_Node (Label => "Node 8", Weight => 1, Level => 3),
                                            Graph.Create_Node (Label => "Node 9", Weight => 2, Level => 3),
                                            Graph.Create_Node (Label => "Node 10", Weight => 4, Level => 3));

   Params : constant SCSC.Graph.Graph_Params_Type := Graph.Create_Polar (Center   => Center,
                                                                         Spacing  => (40, 30, 60),
                                                                         Radius   => 20);

   EP     : constant SCSC.Graph.Energy_Params_Type := SCSC.Graph.Create_Energy_Params;

   Sectors   : Graph.Annular_Sectors_Type (Data'Range);
   Positions : Graph.Positions_Type (Data'Range);
   Length    : Natural;
   Unused    : Long_Integer;

begin
   Graph.Identity (Positions);
   Graph.Layout (Params    => Params,
                 EP        => EP,
                 Data      => Data,
                 ID        => "G1",
                 Sectors   => Sectors,
                 Length    => Length,
                 Positions => Positions,
                 Energy    => Unused);
   declare
      Doc : SVG.Document_Type := SVG.Create_SVG
         (Width  => 400,
          Height => 400,
          Child  => SVG.Circle (Center, 2, Class => "fill_black")
                  + Graph.Create_Graph (Params    => Params,
                                        Data      => Data,
                                        Sectors   => Sectors,
                                        Length    => Length,
                                        ID        => "G1",
                                        Positions => Positions),
          Style  => ".arrow { fill: blue; } .fill_black { fill: black; stroke: none; } "
                    & ".connector { fill: none; stroke: blue; } "
                    & ".text { fill: green; stroke: none; font-size: 10px; } "
                    & ".graph { fill: yellow; stroke: green; } "
                    & ".annular_sector { fill: yellow; stroke: red; } "
                    & ".connector_end { marker-start: url(#End_Arrow); } "
         );
   begin
      Put_Line (SVG.To_String (Doc));
   end;
end Main;

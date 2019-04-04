with Ada.Text_IO; use Ada.Text_IO;
with SCSC.SVG;
with SCSC.Types;
with SCSC.Primitives;
with SCSC.Graph;
with SCSC.Simulated_Annealing;
with SXML.Generator;

with GNAT.Traceback;
with GNAT.Traceback.Symbolic;

procedure Main
is
   use SCSC;
   use SCSC.Types;
   use SXML.Generator;

   Center : Types.Point := P (200, 200);

   Data : constant Graph.Data_Type := (Graph.Create_Node (Label => "Node 1", Weight => 5, Level => 2),
                                       Graph.Create_Node (Label => "Node 2", Weight => 3, Level => 2),
                                       Graph.Create_Node (Label => "Node 3", Weight => 7, Level => 2),
                                       Graph.Create_Node (Label => "Node 4", Weight => 2, Level => 2),
                                       Graph.Create_Node (Label => "Inner I", Weight => 1, Level => 1),
                                       Graph.Create_Node (Label => "Inner II", Weight => 1, Level => 1),
                                       Graph.Create_Node (Label => "Inner III", Weight => 5, Level => 1),
                                       Graph.Create_Node (Label => "Inner IV", Weight => 5, Level => 1),
                                       Graph.Create_Node (Label => "Inner V", Weight => 2, Level => 1),
                                       Graph.Create_Node (Label => "Inner VI", Weight => 4, Level => 1));

   Params : constant Graph.Graph_Params_Type := Graph.Create_Polar (Center  => Center,
                                                                    Radius  => 20,
                                                                    Spacing => (20, 20),
                                                                    Padding => 3);

   package SA is new SCSC.Simulated_Annealing;

   Sectors : Graph.Annular_Sectors_Type (Data'Range);
   Length  : Natural;

begin

   Graph.Calculate_Params (Params  => Params,
                           Data    => Data,
                           ID      => "G1",
                           Sectors => Sectors,
                           Length  => Length);
   declare
      Graph_Energy : constant Natural := SA.Energy (Params, Data, Sectors);
      Doc          : constant SVG.Document_Type := SVG.Create_SVG
         (Width  => 400,
          Height => 400,
          Child  => SVG.Circle (Center, 2)
                  + Graph.Create_Graph (Params  => Params,
                                        Data    => Data,
                                        ID      => "G1",
                                        Sectors => Sectors,
                                        Length  => Length)
                  + SVG.Text (P (20, 20), Graph_Energy'Img),

          Style => ".circle { fill: red; stroke: none; } "
                   & "#G1 { fill: yellow; stroke: green; }"
                   & "#G1 .text { fill: black; stroke: none; font-size: 10px; }"
         );
   begin
      Put_Line (SVG.To_String (Doc));
   end;
end Main;

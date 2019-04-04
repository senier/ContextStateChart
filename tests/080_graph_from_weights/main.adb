with Ada.Text_IO; use Ada.Text_IO;
with SCSC.SVG;
with SCSC.Types;
with SCSC.Primitives;
with SCSC.Graph;
with SXML.Generator;

procedure Main
is
   use SCSC;
   use SCSC.Types;
   use SXML.Generator;
   use type SVG.Document_Type;

   Center : Types.Point := P (200, 200);

   Data : constant Graph.Data_Type := (Graph.Create_Node (5),
                                       Graph.Create_Node (3),
                                       Graph.Create_Node (7),
                                       Graph.Create_Node (2));

   Params : constant Graph.Graph_Params_Type := Graph.Create_Polar (Center  => Center,
                                                                    Radius  => 120,
                                                                    Spacing => (0 => 20),
                                                                    Padding => 20);

   Sectors : Graph.Annular_Sectors_Type (Data'Range);
   Length  : Natural;

begin
   Graph.Calculate_Params (Params  => Params,
                           Data    => Data,
                           ID      => "G1",
                           Sectors => Sectors,
                           Length  => Length);
   declare
      Doc : SVG.Document_Type := SVG.Create_SVG
         (Width  => 400,
          Height => 400,
          Child  => SVG.Circle (Center, 2)
                  + Graph.Create_Graph (Params  => Params,
                                        Sectors => Sectors,
                                        Length  => Length,
                                        Data    => Data),
          Style => ".circle { fill: black; stroke: none; } .annular_sector { fill: yellow; stroke: green; } ");
   begin
      Put_Line (SVG.To_String (Doc));
   end;
end Main;

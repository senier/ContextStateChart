with Ada.Text_IO; use Ada.Text_IO;
with SCSC.SVG;
with SCSC.Types;
with SCSC.Primitives;
with SCSC.Graph;
with SCSC.SA;
with SXML.Generator;

with GNAT.Traceback;
with GNAT.Traceback.Symbolic;

procedure Main
is
   use SCSC;
   use SCSC.Types;
   use SXML.Generator;

   Data : Graph.Data_Type := (Graph.Create_Node (Label => "Node 1", Weight => 5, Level => 2),
                              Graph.Create_Node (Label => "Node 2", Weight => 3, Level => 2),
                              Graph.Create_Node (Label => "Node 3", Weight => 7, Level => 2),
                              Graph.Create_Node (Label => "Node 4", Weight => 2, Level => 2),
                              Graph.Create_Node (Label => "Inner I", Weight => 1, Level => 1),
                              Graph.Create_Node (Label => "Inner II", Weight => 1, Level => 1),
                              Graph.Create_Node (Label => "Inner III", Weight => 5, Level => 1),
                              Graph.Create_Node (Label => "Inner IV", Weight => 5, Level => 1),
                              Graph.Create_Node (Label => "Inner V", Weight => 2, Level => 1),
                              Graph.Create_Node (Label => "Inner VI", Weight => 4, Level => 1));

   Params : Graph.Graph_Params_Type := Graph.Create_Polar (Center  => P (300, 250),
                                                           Radius  => 20,
                                                           Spacing => (0, 0),
                                                           Padding => 3);

   Sectors   : Graph.Annular_Sectors_Type (Data'Range);
   Positions : Graph.Positions_Type (Data'Range);
   Length    : Natural;

   Font_Size : constant Natural := 10;

   Style : constant String := ".circle { fill: red; stroke: none; } "
                              & "#G1 { fill: yellow; stroke: green; }"
                              & "#G1 .text { fill: black; stroke: none; font-size:" & Font_Size'Img & "px; }"
                              & "#G2 { fill: yellow; stroke: green; }"
                              & "#G2 .text { fill: black; stroke: none; font-size:" & Font_Size'Img & "px; }";

   OP : constant SA.Params_Type := SA.Create_Optimize_Params;

   EP : constant SCSC.Graph.Energy_Params_Type := SCSC.Graph.Create_Energy_Params;

begin
   Graph.Identity (Positions);
   SA.Optimize (ID              => "G1",
                Optimize_Params => OP,
                Energy_Params   => EP,
                Graph_Params    => Params,
                Graph_Data      => Data,
                Sectors         => Sectors,
                Length          => Length,
                Positions       => Positions);

   declare
      EP : Graph.Energy_Params_Type := Graph.Create_Energy_Params;

      Doc : constant SVG.Document_Type := SVG.Create_SVG
         (Width  => 600,
          Height => 500,
          Child  => Graph.Create_Graph (Params    => Params,
                                        Sectors   => Sectors,
                                        Length    => Length,
                                        Data      => Data,
                                        Positions => Positions,
                                        ID        => "G1")
                  + SVG.Text (P (20, 20), Graph.Calculate_Energy (Params, EP, Data, Sectors, Positions)'Img),
          Style => Style);
   begin
      Put_Line (SVG.To_String (Doc));
   end;
end Main;

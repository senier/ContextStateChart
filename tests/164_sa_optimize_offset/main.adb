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

   Positions : Graph.Positions_Type  := (1 .. 0 => 0);

   package SA is new SCSC.Simulated_Annealing;

   Sectors : Graph.Annular_Sectors_Type (Data'Range);
   Length  : Natural;

   Font_Size : constant Natural := 10;

   Style : constant String := ".circle { fill: red; stroke: none; } "
                              & "#G1 { fill: yellow; stroke: green; }"
                              & "#G1 .text { fill: black; stroke: none; font-size:" & Font_Size'Img & "px; }"
                              & "#G2 { fill: yellow; stroke: green; }"
                              & "#G2 .text { fill: black; stroke: none; font-size:" & Font_Size'Img & "px; }";
begin

   SA.Optimize (ID        => "G1",
                Font_Size => Font_Size,
                Params    => Params,
                Data      => Data,
                Sectors   => Sectors,
                Length    => Length,
                Positions => Positions);

   declare
      Doc : constant SVG.Document_Type := SVG.Create_SVG
         (Width  => 600,
          Height => 500,
          Child  => Graph.Create_Graph (Params    => Params,
                                        Data      => Data,
                                        ID        => "G1",
                                        Sectors   => Sectors,
                                        Length    => Length,
                                        Positions => Positions)
                  + SVG.Text (P (20, 20), SA.Energy (Params, Data, Sectors, Font_Size)'Img),
          Style => Style);
   begin
      Put_Line (SVG.To_String (Doc));
   end;
end Main;

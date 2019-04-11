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

   Params1 : constant Graph.Graph_Params_Type := Graph.Create_Polar (Center  => P (100, 250),
                                                                     Radius  => 20,
                                                                     Spacing => (0, 0),
                                                                     Padding => 3);

   Params2 : constant Graph.Graph_Params_Type := Graph.Create_Polar (Center  => P (380, 250),
                                                                     Radius  => 20,
                                                                     Spacing => (130, 20),
                                                                     Padding => 3);

   Sectors1   : Graph.Annular_Sectors_Type (Data'Range);
   Positions1 : Graph.Positions_Type (Data'Range);
   Length1    : Natural;

   Sectors2   : Graph.Annular_Sectors_Type (Data'Range);
   Positions2 : Graph.Positions_Type (Data'Range);
   Length2    : Natural;

   Fontsize : constant Natural := 10;

   Style : constant String := ".circle { fill: red; stroke: none; } "
                              & "#G1 { fill: yellow; stroke: green; }"
                              & "#G1 .text { fill: black; stroke: none; font-size:" & Fontsize'Img & "px; }"
                              & "#G2 { fill: yellow; stroke: green; }"
                              & "#G2 .text { fill: black; stroke: none; font-size:" & Fontsize'Img & "px; }";

   EP    : constant SCSC.Graph.Energy_Params_Type := SCSC.Graph.Create_Energy_Params;

   Graph_Energy1 : Long_Integer;
   Graph_Energy2 : Long_Integer;

begin

   Graph.Identity (Positions1);
   Graph.Layout (Params    => Params1,
                 EP        => EP,
                 Data      => Data,
                 ID        => "G1",
                 Sectors   => Sectors1,
                 Length    => Length1,
                 Positions => Positions1,
                 Energy    => Graph_Energy1);

   Graph.Identity (Positions2);
   Graph.Layout (Params    => Params2,
                 EP        => EP,
                 Data      => Data,
                 ID        => "G2",
                 Sectors   => Sectors2,
                 Length    => Length2,
                 Positions => Positions2,
                 Energy    => Graph_Energy2);
   declare
      Doc : constant SVG.Document_Type := SVG.Create_SVG
         (Width  => 600,
          Height => 500,
          Child  => Graph.Create_Graph (Params    => Params1,
                                        Data      => Data,
                                        ID        => "G1",
                                        Sectors   => Sectors1,
                                        Length    => Length1,
                                        Positions => Positions1)
                  + SVG.Text (P (20, 20), Graph_Energy1'Img)
                  + Graph.Create_Graph (Params    => Params2,
                                        Data      => Data,
                                        ID        => "G2",
                                        Sectors   => Sectors2,
                                        Length    => Length2,
                                        Positions => Positions2)
                  + SVG.Text (P (250, 20), Graph_Energy2'Img),
          Style => Style);
   begin
      Put_Line (SVG.To_String (Doc));
   end;
end Main;

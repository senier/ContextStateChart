with Ada.Text_IO; use Ada.Text_IO;
with SCSC.SVG;
with SCSC.Types;
with SCSC.Primitives;
with SCSC.Graph;
with SXML.Generator;

procedure Main
is
   use SCSC.Types;
   use SCSC.Primitives;
   use SCSC.Graph;
   use SCSC.SVG;
   use SXML.Generator;
   use type Document_Type;

   Center : Point := P (200, 200);

   Arrow_End : Document_Type := Path (Commands => ((Moveto, Absolute, 0, 2),
                                                   (Lineto, Absolute, 4, 4),
                                                   (Lineto, Absolute, 4, 0),
                                                   (ZClosepath, Absolute)),
                                      Class => "arrow");

   Data : constant SCSC.Graph.Data_Type :=
      ( 1 => Create_Node (Label => "N1 (W2)", Weight => 2, Outer_Ports => 3, Inner_Ports => 1,
                          Edges => (1 => Create_Edge (2, Dir_CW, 20, (2, Pos_Outer), (1, Pos_Outer), "1 ⇒ 2"),
                                    2 => Create_Edge (3, Dir_CCW, 30, (3, Pos_Outer), (3, Pos_Outer), "1 ⇒ 3"))),
        2 => Create_Node (Label => "N2 (W4)", Weight => 4, Outer_Ports => 2, Inner_Ports => 1,
                          Edges => (1 => Create_Edge (3, Dir_CW, 40, (2, Pos_Outer), (1, Pos_Outer), "2 ⇒ 3"))),
        3 => Create_Node (Label => "N3 (W8)", Weight => 8, Outer_Ports => 3, Inner_Ports => 1,
                          Edges => (1 => Create_Edge (4, Dir_CW, 60, (2, Pos_Outer), (1, Pos_Outer), "3 ⇒ 4"))),
        4 => Create_Node (Label => "N4 (W16)", Weight => 16, Outer_Ports => 2, Inner_Ports => 1,
                          Edges => (1 => Create_Edge (1, Dir_CW, 80, (2, Pos_Outer), (1, Pos_Outer), "4 ⇒ 1"))));

   Params : constant SCSC.Graph.Graph_Params_Type := Create_Polar (Center  => Center,
                                                                   Radius  => 20,
                                                                   Spacing => (0 => 120),
                                                                   Padding => 5);

   Positions : constant SCSC.Graph.Positions_Type := (1, 3, 2, 4);

   Sectors : SCSC.Graph.Annular_Sectors_Type (Data'Range);
   Length  : Natural;

begin

   SCSC.Graph.Calculate_Params (Params    => Params,
                                Data      => Data,
                                ID        => "Graph1",
                                Positions => Positions,
                                Sectors   => Sectors,
                                Length    => Length);
   declare
      Doc : Document_Type := Create_SVG
         (Width  => 400,
          Height => 400,
          Child  => Circle (Center, 2, Class => "fill_black")
                  + Create_Graph (Params    => Params,
                                  Data      => Data,
                                  ID        => "Graph1",
                                  Positions => Positions,
                                  Sectors   => Sectors,
                                  Length    => Length),
          Defs   => Marker (Element => Arrow_End, Width => 4, Height => 4, RefX => 0.1, RefY => 2.0, ID => "End_Arrow"),
          Style  => ".arrow { fill: blue; } .fill_black { fill: black; stroke: none; } "
                    & ".connector { fill: none; stroke: blue; } "
                    & ".text { fill: green; stroke: none; font-size: 10px; } "
                    & ".graph { fill: yellow; stroke: black; } "
                    & ".annular_sector { fill: yellow; stroke: red; } "
                    & ".connector_end { marker-start: url(#End_Arrow); } "
         );
   begin
      Put_Line (To_String (Doc));
   end;
end Main;

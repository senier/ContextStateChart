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
      ( 1 => Create_Node (Label => "Node 1", Weight => 5, Outer_Ports => 4, Inner_Ports => 1,
                          Edges => (1 => Create_Edge (2, Dir_CW, 20, (4, Pos_Outer), (1, Pos_Outer), "1 ⇒ 2"),
                                    2 => Create_Edge (3, Dir_CCW, 40, (2, Pos_Outer), (2, Pos_Outer), "1 ⇒ 3"),
                                    3 => Create_Edge (2, Dir_CW, -30, (1, Pos_Inner), (1, Pos_Inner), "1 ⇒ 2"))),
        2 => Create_Node (Label => "Node 2", Weight => 3, Outer_Ports => 3, Inner_Ports => 4,
                          Edges => (1 => Create_Edge (1, Dir_CCW, 40, (2, Pos_Outer), (3, Pos_Outer), "2 ⇒ 1"),
                                    2 => Create_Edge (3, Dir_CW, 20, (3, Pos_Outer), (1, Pos_Outer), "2 ⇒ 3"),
                                    3 => Create_Edge (3, Dir_CW, -30, (4, Pos_Inner), (1, Pos_Inner), "2 ⇒ 3"))),
        3 => Create_Node (Label => "Node 3", Weight => 7, Outer_Ports => 2, Inner_Ports => 3,
                          Edges => (1 => Create_Edge (2, Dir_CCW, -50, (2, Pos_Inner), (3, Pos_Inner), "3 ⇒ 2"))),
        4 => Create_Node (Label => "Node 4", Weight => 2, Outer_Ports => 1, Inner_Ports => 2,
                          Edges => (1 => Create_Edge (1, Dir_CW, 20, (1, Pos_Outer), (1, Pos_Outer), "4 ⇒ 1"),
                                    2 => Create_Edge (3, Dir_CCW, -30, (1, Pos_Inner), (3, Pos_Inner), "4 ⇒ 3"),
                                    3 => Create_Edge (2, Dir_CW, -50, (2, Pos_Inner), (2, Pos_Inner), "4 ⇒ 2"))));

   Params : constant SCSC.Graph.Graph_Params_Type := Create_Polar (Center  => Center,
                                                                   Radius  => 20,
                                                                   Spacing => (0 => 120),
                                                                   Padding => 5);

   EP     : constant SCSC.Graph.Energy_Params_Type := SCSC.Graph.Create_Energy_Params;

   Sectors   : SCSC.Graph.Annular_Sectors_Type (Data'Range);
   Positions : SCSC.Graph.Positions_Type (Data'Range);
   Length : Natural;
   Unused : Long_Integer;

begin
   SCSC.Graph.Identity (Positions);
   SCSC.Graph.Layout (Params        => Params,
                      Energy_Params => EP,
                      Data          => Data,
                      ID            => "Graph",
                      Sectors       => Sectors,
                      Length        => Length,
                      Positions     => Positions,
                      Energy        => Unused);
   declare
      Doc : Document_Type := Create_SVG
         (Width  => 400,
          Height => 400,
          Child  => Circle (Center, 2, Class => "fill_black")
                  + Create_Graph (Params    => Params,
                                  Data      => Data,
                                  Sectors   => Sectors,
                                  Length    => Length,
                                  ID        => "Graph1",
                                  Positions => Positions),
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

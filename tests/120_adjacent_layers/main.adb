with Ada.Text_IO; use Ada.Text_IO;
with SCSC.SVG;
with SCSC.Types;
with SCSC.Primitives;
with SCSC.Graph;

procedure Main
is
   use SCSC.Types;
   use SCSC.Primitives;
   use SCSC.Graph;
   use SCSC.SVG;
   use type Element_Type;

   Center : Point := P (200, 200);

   Arrow_End : Element_Type := Path (Commands => ((Moveto, Absolute, 0, 2),
                                                 (Lineto, Absolute, 4, 4),
                                                 (Lineto, Absolute, 4, 0),
                                                 (ZClosepath, Absolute)),
                                    Style => "fill: blue");


   Doc : Document_Type := SVG
      (Width  => 400,
       Height => 400,
       Child  => Circle (Center, 2, Style => "fill: black; stroke: none")
               + Graph (Params => Polar (Center => Center, Offset => 100, Radius => 20, Layer_Spacing => 80, Padding => 5),
                        Data   => ( 1 => Node (Label => "Inner 1", Level => 1, Weight => 5, Outer_Ports => 3, Inner_Ports => 1,
                                               Edges => (1 => Edge (2, Dir_CW, -30, (1, Pos_Inner), (1, Pos_Inner), "I1⇒I2"))),
                                    2 => Node (Label => "Inner 2", Level => 1, Weight => 3, Outer_Ports => 1, Inner_Ports => 4,
                                               Edges => (1 => Edge (3, Dir_CW, -30, (4, Pos_Inner), (1, Pos_Inner), "I2⇒I3"),
                                                         2 => Edge (1, Dir_CCW, 15, (1, Pos_Outer), (3, Pos_Outer), "I2⇒I1"))),
                                    3 => Node (Label => "Inner 3", Level => 1, Weight => 7, Outer_Ports => 3, Inner_Ports => 3,
                                               Edges => (1 => Edge (2, Dir_CCW, -50, (2, Pos_Inner), (3, Pos_Inner), "I3⇒I2"),
                                                         2 => Edge (7, Dir_CCW, 15, (1, Pos_Outer), (1, Pos_Inner), "I3⇒O3"),
                                                         3 => Edge (8, Dir_CW, 15, (2, Pos_Outer), (1, Pos_Inner), "I3⇒O4"),
                                                         4 => Edge (9, Dir_CW, 15, (3, Pos_Outer), (1, Pos_Inner), "I3⇒O5"))),
                                    4 => Node (Label => "Inner 4", Level => 1, Weight => 2, Outer_Ports => 1, Inner_Ports => 2,
                                               Edges => (1 => Edge (3, Dir_CCW, -30, (1, Pos_Inner), (3, Pos_Inner), "I4⇒I3"),
                                                         2 => Edge (2, Dir_CW, -50, (2, Pos_Inner), (2, Pos_Inner), "I4⇒I2"))),
                                    5 => Node (Label => "Outer 1", Level => 2, Weight => 2, Outer_Ports => 1, Inner_Ports => 1),
                                    6 => Node (Label => "Outer 2", Level => 2, Weight => 2, Outer_Ports => 1, Inner_Ports => 2,
                                               Edges => (1 => Edge (1, Dir_CCW, -15, (1, Pos_Inner), (1, Pos_Outer), "O2⇒I1"),
                                                         2 => Edge (1, Dir_CCW, -30, (2, Pos_Inner), (2, Pos_Outer), "O2⇒I1"))),
                                    7 => Node (Label => "Outer 3", Level => 2, Weight => 2, Outer_Ports => 1, Inner_Ports => 1),
                                    8 => Node (Label => "Outer 4", Level => 2, Weight => 3, Outer_Ports => 1, Inner_Ports => 1),
                                    9 => Node (Label => "Outer 5", Level => 2, Weight => 1, Outer_Ports => 1, Inner_Ports => 1)),
                        Style  => "fill: yellow; stroke: black",
                        Text_Style  => "fill: green; stroke: none; font-size: 10px",
                        Connector_Style => "fill: none; stroke: blue"),
       Defs   => Marker (Element => Arrow_End, Width => 4, Height => 4, RefX => 0.1, RefY => 2.0, ID => "End_Arrow")
      );
begin
   Put_Line (To_String (Doc));
end Main;
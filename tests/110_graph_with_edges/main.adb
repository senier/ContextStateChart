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

   Doc : Document_Type := SVG
      (Width  => 400,
       Height => 400,
       Child  => Circle (Center, 2, Style => "fill: black; stroke: none")
               + Graph (Params => Polar (Center, 120, 20, 20),
                        Data   => ( 1 => Node (Label => "Node 1", Weight => 5, Outer_Ports => 3,
                                               Edges => (1 => Edge (2, Dir_CW, (2, Pos_Outer), (3, Pos_Outer), "1 => 2"),
                                                         2 => Edge (3, Dir_CCW, (1, Pos_Outer), (1, Pos_Outer), "1 => 3"))),
                                    2 => Node (Label => "Node 2", Weight => 3, Outer_Ports => 3,
                                               Edges => (1 => Edge (1, Dir_CW, (1, Pos_Outer), (1, Pos_Outer), "2 => 1"),
                                                         2 => Edge (3, Dir_CW, (2, Pos_Outer), (1, Pos_Outer), "2 => 2"))),
                                    3 => Node (Label => "Node 3", Weight => 7, Outer_Ports => 1),
                                    4 => Node (Label => "Node 4", Weight => 2, Outer_Ports => 1,
                                               Edges => (1 => Edge (1, Dir_CCW, (1, Pos_Outer), (3, Pos_Outer), "4 => 1")))),
                        Style  => "fill: yellow; stroke: green")
      );
begin
   Put_Line (To_String (Doc));
end Main;

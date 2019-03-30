with Ada.Text_IO; use Ada.Text_IO;
with SCSC.SVG;
with SCSC.Types;
with SCSC.Primitives;
with SXML.Generator;

procedure Main
is
   use SCSC.Primitives;
   use SCSC.SVG;
   use SCSC.Types;
   use SXML.Generator;

   Center : Point := P (200, 200);

   P01 : Annular_Sector_Params_Type := Polar (Center, 160, 20, 331.0, 359.0);
   P02 : Annular_Sector_Params_Type := Polar (Center, 160, 20, 1.0, 29.0);
   P03 : Annular_Sector_Params_Type := Polar (Center, 160, 20, 31.0, 59.0);
   P04 : Annular_Sector_Params_Type := Polar (Center, 160, 20, 61.0, 89.0);
   P05 : Annular_Sector_Params_Type := Polar (Center, 160, 20, 91.0, 119.0);
   P06 : Annular_Sector_Params_Type := Polar (Center, 160, 20, 121.0, 149.0);
   P07 : Annular_Sector_Params_Type := Polar (Center, 160, 20, 151.0, 179.0);
   P08 : Annular_Sector_Params_Type := Polar (Center, 160, 20, 181.0, 209.0);
   P09 : Annular_Sector_Params_Type := Polar (Center, 160, 20, 211.0, 239.0);
   P10 : Annular_Sector_Params_Type := Polar (Center, 160, 20, 241.0, 269.0);
   P11 : Annular_Sector_Params_Type := Polar (Center, 160, 20, 271.0, 299.0);
   P12 : Annular_Sector_Params_Type := Polar (Center, 160, 20, 301.0, 329.0);

   SVG : SCSC.SVG.Document_Type := SCSC.SVG.Create_SVG
      (Width  => 400,
       Height => 400,
       Child  => Circle (Center, 2, Class => "fill_black")
               + Annular_Sector (Params => P01,
                                 Text   => "PO/1 PI/1",
                                 ID     => "AS1",
                                 Class  => "yellow_green")

               + Circle (P (Port (P01, Pos_Outer, 1, 1)), 2, Class => "fill_red")
               + Circle (P (Port (P01, Pos_Inner, 1, 1)), 2, Class => "fill_green")

               + Annular_Sector (Params => P02,
                                 Text   => "PO/2 PI/2",
                                 ID     => "AS2",
                                 Class  => "yellow_green")

               + Circle (P (Port (P02, Pos_Outer, 1, 2)), 2, Class => "fill_red")
               + Circle (P (Port (P02, Pos_Outer, 2, 2)), 2, Class => "fill_red")
               + Circle (P (Port (P02, Pos_Inner, 1, 2)), 2, Class => "fill_green")
               + Circle (P (Port (P02, Pos_Inner, 2, 2)), 2, Class => "fill_green")

               + Annular_Sector (Params => P03,
                                 Text   => "PO/3 PI/3",
                                 ID     => "AS3",
                                 Class  => "yellow_green")

               + Circle (P (Port (P03, Pos_Outer, 1, 3)), 2, Class => "fill_red")
               + Circle (P (Port (P03, Pos_Outer, 2, 3)), 2, Class => "fill_red")
               + Circle (P (Port (P03, Pos_Outer, 3, 3)), 2, Class => "fill_red")

               + Circle (P (Port (P03, Pos_Inner, 1, 3)), 2, Class => "fill_green")
               + Circle (P (Port (P03, Pos_Inner, 2, 3)), 2, Class => "fill_green")
               + Circle (P (Port (P03, Pos_Inner, 3, 3)), 2, Class => "fill_green")

               + Annular_Sector (Params => P04,
                                 Text   => "PO/4 PI/4",
                                 ID     => "AS4",
                                 Class  => "yellow_green")

               + Circle (P (Port (P04, Pos_Outer, 1, 4)), 2, Class => "fill_red")
               + Circle (P (Port (P04, Pos_Outer, 2, 4)), 2, Class => "fill_red")
               + Circle (P (Port (P04, Pos_Outer, 3, 4)), 2, Class => "fill_red")
               + Circle (P (Port (P04, Pos_Outer, 4, 4)), 2, Class => "fill_red")

               + Circle (P (Port (P04, Pos_Inner, 1, 4)), 2, Class => "fill_green")
               + Circle (P (Port (P04, Pos_Inner, 2, 4)), 2, Class => "fill_green")
               + Circle (P (Port (P04, Pos_Inner, 3, 4)), 2, Class => "fill_green")
               + Circle (P (Port (P04, Pos_Inner, 4, 4)), 2, Class => "fill_green")

               + Annular_Sector (Params => P05,
                                 Text   => "PO/5 PI/5",
                                 ID     => "AS5",
                                 Class  => "yellow_green")

               + Circle (P (Port (P05, Pos_Outer, 1, 5)), 2, Class => "fill_red")
               + Circle (P (Port (P05, Pos_Outer, 2, 5)), 2, Class => "fill_red")
               + Circle (P (Port (P05, Pos_Outer, 3, 5)), 2, Class => "fill_red")
               + Circle (P (Port (P05, Pos_Outer, 4, 5)), 2, Class => "fill_red")
               + Circle (P (Port (P05, Pos_Outer, 5, 5)), 2, Class => "fill_red")

               + Circle (P (Port (P05, Pos_Inner, 1, 5)), 2, Class => "fill_green")
               + Circle (P (Port (P05, Pos_Inner, 2, 5)), 2, Class => "fill_green")
               + Circle (P (Port (P05, Pos_Inner, 3, 5)), 2, Class => "fill_green")
               + Circle (P (Port (P05, Pos_Inner, 4, 5)), 2, Class => "fill_green")
               + Circle (P (Port (P05, Pos_Inner, 5, 5)), 2, Class => "fill_green")

               + Annular_Sector (Params => P06,
                                 Text   => "PO/6 PI/6",
                                 ID     => "AS6",
                                 Class  => "yellow_green")

               + Circle (P (Port (P06, Pos_Outer, 1, 6)), 2, Class => "fill_red")
               + Circle (P (Port (P06, Pos_Outer, 2, 6)), 2, Class => "fill_red")
               + Circle (P (Port (P06, Pos_Outer, 3, 6)), 2, Class => "fill_red")
               + Circle (P (Port (P06, Pos_Outer, 4, 6)), 2, Class => "fill_red")
               + Circle (P (Port (P06, Pos_Outer, 5, 6)), 2, Class => "fill_red")
               + Circle (P (Port (P06, Pos_Outer, 6, 6)), 2, Class => "fill_red")

               + Circle (P (Port (P06, Pos_Inner, 1, 6)), 2, Class => "fill_green")
               + Circle (P (Port (P06, Pos_Inner, 2, 6)), 2, Class => "fill_green")
               + Circle (P (Port (P06, Pos_Inner, 3, 6)), 2, Class => "fill_green")
               + Circle (P (Port (P06, Pos_Inner, 4, 6)), 2, Class => "fill_green")
               + Circle (P (Port (P06, Pos_Inner, 5, 6)), 2, Class => "fill_green")
               + Circle (P (Port (P06, Pos_Inner, 6, 6)), 2, Class => "fill_green")

               + Annular_Sector (Params => P07,
                                 Text   => "PO/6 PI/1",
                                 ID     => "AS7",
                                 Class  => "yellow_green")

               + Circle (P (Port (P07, Pos_Outer, 1, 6)), 2, Class => "fill_red")
               + Circle (P (Port (P07, Pos_Outer, 2, 6)), 2, Class => "fill_red")
               + Circle (P (Port (P07, Pos_Outer, 3, 6)), 2, Class => "fill_red")
               + Circle (P (Port (P07, Pos_Outer, 4, 6)), 2, Class => "fill_red")
               + Circle (P (Port (P07, Pos_Outer, 5, 6)), 2, Class => "fill_red")
               + Circle (P (Port (P07, Pos_Outer, 6, 6)), 2, Class => "fill_red")

               + Circle (P (Port (P07, Pos_Inner, 1, 1)), 2, Class => "fill_green")

               + Annular_Sector (Params => P08,
                                 Text   => "PO/5 PI/2",
                                 ID     => "AS8",
                                 Class  => "yellow_green")

               + Circle (P (Port (P08, Pos_Outer, 1, 5)), 2, Class => "fill_red")
               + Circle (P (Port (P08, Pos_Outer, 2, 5)), 2, Class => "fill_red")
               + Circle (P (Port (P08, Pos_Outer, 3, 5)), 2, Class => "fill_red")
               + Circle (P (Port (P08, Pos_Outer, 4, 5)), 2, Class => "fill_red")
               + Circle (P (Port (P08, Pos_Outer, 5, 5)), 2, Class => "fill_red")

               + Circle (P (Port (P08, Pos_Inner, 1, 2)), 2, Class => "fill_green")
               + Circle (P (Port (P08, Pos_Inner, 2, 2)), 2, Class => "fill_green")

               + Annular_Sector (Params => P09,
                                 Text   => "PO/4 PI/3",
                                 ID     => "AS9",
                                 Class  => "yellow_green")

               + Circle (P (Port (P09, Pos_Outer, 1, 4)), 2, Class => "fill_red")
               + Circle (P (Port (P09, Pos_Outer, 2, 4)), 2, Class => "fill_red")
               + Circle (P (Port (P09, Pos_Outer, 3, 4)), 2, Class => "fill_red")
               + Circle (P (Port (P09, Pos_Outer, 4, 4)), 2, Class => "fill_red")

               + Circle (P (Port (P09, Pos_Inner, 1, 3)), 2, Class => "fill_green")
               + Circle (P (Port (P09, Pos_Inner, 2, 3)), 2, Class => "fill_green")
               + Circle (P (Port (P09, Pos_Inner, 3, 3)), 2, Class => "fill_green")

               + Annular_Sector (Params => P10,
                                 Text   => "PO/3 PI/4",
                                 ID     => "AS10",
                                 Class  => "yellow_green")

               + Circle (P (Port (P10, Pos_Outer, 1, 3)), 2, Class => "fill_red")
               + Circle (P (Port (P10, Pos_Outer, 2, 3)), 2, Class => "fill_red")
               + Circle (P (Port (P10, Pos_Outer, 3, 3)), 2, Class => "fill_red")

               + Circle (P (Port (P10, Pos_Inner, 1, 4)), 2, Class => "fill_green")
               + Circle (P (Port (P10, Pos_Inner, 2, 4)), 2, Class => "fill_green")
               + Circle (P (Port (P10, Pos_Inner, 3, 4)), 2, Class => "fill_green")
               + Circle (P (Port (P10, Pos_Inner, 4, 4)), 2, Class => "fill_green")

               + Annular_Sector (Params => P11,
                                 Text   => "PO/2 PI/5",
                                 ID     => "AS11",
                                 Class  => "yellow_green")

               + Circle (P (Port (P11, Pos_Outer, 1, 2)), 2, Class => "fill_red")
               + Circle (P (Port (P11, Pos_Outer, 2, 2)), 2, Class => "fill_red")

               + Circle (P (Port (P11, Pos_Inner, 1, 5)), 2, Class => "fill_green")
               + Circle (P (Port (P11, Pos_Inner, 2, 5)), 2, Class => "fill_green")
               + Circle (P (Port (P11, Pos_Inner, 3, 5)), 2, Class => "fill_green")
               + Circle (P (Port (P11, Pos_Inner, 4, 5)), 2, Class => "fill_green")
               + Circle (P (Port (P11, Pos_Inner, 5, 5)), 2, Class => "fill_green")

               + Annular_Sector (Params => P12,
                                 Text   => "PO/1 PI/6",
                                 ID     => "AS12",
                                 Class  => "yellow_green")

               + Circle (P (Port (P12, Pos_Outer, 1, 1)), 2, Class => "fill_red")

               + Circle (P (Port (P12, Pos_Inner, 1, 6)), 2, Class => "fill_green")
               + Circle (P (Port (P12, Pos_Inner, 2, 6)), 2, Class => "fill_green")
               + Circle (P (Port (P12, Pos_Inner, 3, 6)), 2, Class => "fill_green")
               + Circle (P (Port (P12, Pos_Inner, 4, 6)), 2, Class => "fill_green")
               + Circle (P (Port (P12, Pos_Inner, 5, 6)), 2, Class => "fill_green")
               + Circle (P (Port (P12, Pos_Inner, 6, 6)), 2, Class => "fill_green"),
       Style => ".text { font-size: 10px; } .yellow_green { fill: yellow; stroke: green; } "
                & ".fill_green { fill: green; } .fill_red { fill: red; } .fill_black { fill: black; } "
      );
begin
   Put_Line (SCSC.SVG.To_String (SVG));
end Main;

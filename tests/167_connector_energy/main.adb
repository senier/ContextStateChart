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
   use SCSC.Primitives;
   use SCSC.SVG;
   use SXML.Generator;

   Center_1 : Point := P (200, 200);
   Center_2 : Point := P (800, 200);
   Center_3 : Point := P (500, 500);
   Font_Size : constant Natural := 10;

   Style : constant String := ".circle { fill: none; stroke: red; } "
                              & "#C1 { fill: none; stroke: black; } "
                              & "#E1 { fill: black; stroke: none; } "
                              & "#C2 { fill: none; stroke: green; } "
                              & "#E2 { fill: green; stroke: none; } "
                              & "#C3 { fill: none; stroke: blue; } "
                              & "#E3 { fill: blue; stroke: none; } "
                              & "#C4 { fill: none; stroke: red; } "
                              & "#E4 { fill: red; stroke: none; } "
                              & "#C5 { fill: none; stroke: orange; } "
                              & "#E5 { fill: orange; stroke: none; } "
                              & "#C6 { fill: none; stroke: cyan; } "
                              & "#E6 { fill: cyan; stroke: none; } ";

   EP     : constant SCSC.Graph.Energy_Params_Type := SCSC.Graph.Create_Energy_Params;
   Params_1 : Connector_Params_Type := Cartesian (Center_1, P (56, 300), P (68, 116), 10, Dir_CW, "Connector 1");
   Params_2 : Connector_Params_Type := Cartesian (Center_1, P (66, 300), P (78, 116), 10, Dir_CCW, "Connector 2");
   Params_3 : Connector_Params_Type := Cartesian (Center_2, P (656, 300), P (668, 116), 10, Dir_CCW, "Connector 3");
   Params_4 : Connector_Params_Type := Cartesian (Center_2, P (646, 300), P (658, 116), 10, Dir_CW, "Connector 4");
   Params_5 : Connector_Params_Type := Cartesian (Center_3, P (620, 380), P (358, 416), 10, Dir_CW, "Connector 5");
   Params_6 : Connector_Params_Type := Cartesian (Center_3, P (620, 370), P (358, 406), 10, Dir_CCW, "Connector 6");

   Energy_1 : Long_Integer := Graph.Calculate_Energy (Params_1, EP);
   Energy_2 : Long_Integer := Graph.Calculate_Energy (Params_2, EP);
   Energy_3 : Long_Integer := Graph.Calculate_Energy (Params_3, EP);
   Energy_4 : Long_Integer := Graph.Calculate_Energy (Params_4, EP);
   Energy_5 : Long_Integer := Graph.Calculate_Energy (Params_5, EP);
   Energy_6 : Long_Integer := Graph.Calculate_Energy (Params_6, EP);
begin

   declare
      Doc : constant SVG.Document_Type := SVG.Create_SVG
         (Width  => 1000,
          Height =>  700,
       Child  => Connector (Params => Params_1, ID => "C1")
               + SVG.Text (P (70, 150), Energy_1'Img, ID => "E1")
               + SVG.Text (P (70, 170), Params_1.Get_Arc.Length'Img, ID => "L1")
               + Connector (Params => Params_2, ID => "C2")
               + SVG.Text (P (250, 150), Energy_2'Img, ID => "E2")
               + SVG.Text (P (250, 170), Params_2.Get_Arc.Length'Img, ID => "L2")
               + Connector (Params => Params_3, ID => "C3")
               + SVG.Text (P (670, 150), Energy_3'Img, ID => "E3")
               + SVG.Text (P (670, 170), Params_3.Get_Arc.Length'Img, ID => "L3")
               + Connector (Params => Params_4, ID => "C4")
               + SVG.Text (P (850, 150), Energy_4'Img, ID => "E4")
               + SVG.Text (P (850, 170), Params_4.Get_Arc.Length'Img, ID => "L4")
               + Connector (Params => Params_5, ID => "C5")
               + SVG.Text (P (450, 450), Energy_5'Img, ID => "E5")
               + SVG.Text (P (450, 470), Params_5.Get_Arc.Length'Img, ID => "L5")
               + Connector (Params => Params_6, ID => "C6")
               + SVG.Text (P (450, 550), Energy_6'Img, ID => "E6")
               + SVG.Text (P (450, 570), Params_6.Get_Arc.Length'Img, ID => "L6"),
          Style => Style);
   begin
      Put_Line (SVG.To_String (Doc));
   end;
end Main;

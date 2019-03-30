with Ada.Text_IO; use Ada.Text_IO;
with SCSC.SVG;
with SCSC.Primitives;
with SCSC.Types;
with SXML.Generator;

procedure Main
is
   use SCSC.Primitives;
   use SCSC.SVG;
   use SCSC.Types;
   use SXML.Generator;

   Center : Point := P (200, 200);

   Style_1 : constant String := "fill: black; stroke: black";
   Style_2 : constant String := "fill: none; stroke: black";
   Style_3 : constant String := "fill: none; stroke: red";
   Style_4 : constant String := "fill: none; stroke: green";
   Style_5 : constant String := "fill: none; stroke: cyan";

   SVG : SCSC.SVG.Document_Type := SCSC.SVG.Create_SVG
      (
       Width  => 400,
       Height => 400,
       Child  => Circle (Center,   2, Class => "s1")
               + Circle (Center,  50, Class => "s2")
               + Circle (Center, 100, Class => "s2")
               + Circle (Center, 150, Class => "s2")
               + Circle (Center, 200, Class => "s2")

               + Line (Cartesian (Center, P (117, 185), -50), Class => "s5")
               + Line (Cartesian (Center, P (0, 200), -150), Class => "s5")

               + Line (Cartesian (Center, P (250, 250), 50), Class => "s3")
               + Line (Cartesian (Center, P (150, 150), -50), Class => "s4")
               + Line (Cartesian (Center, P (400, 200), -150), Class => "s3")
               + Line (Cartesian (Center, P (190, 210), 75), Class => "s3"),
       Style => "* { fill: none; } .s1 { fill:black; stroke: black; } .s2 { stroke: black; } "
                & ".s3 { stroke: red; } .s4 { stroke: green; } .s5 { stroke: cyan; }"
      );
begin
   Put_Line (SCSC.SVG.To_String (SVG));
end Main;

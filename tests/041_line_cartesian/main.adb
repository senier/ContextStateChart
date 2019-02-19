with Ada.Text_IO; use Ada.Text_IO;
with SCSC.SVG;
with SCSC.Primitives;
with SCSC.Types;

procedure Main
is
   use SCSC.Primitives;
   use SCSC.SVG;
   use SCSC.Types;

   Center : Point := P (200, 200);

   Style_1 : constant String := "fill: black; stroke: black";
   Style_2 : constant String := "fill: none; stroke: black";
   Style_3 : constant String := "fill: none; stroke: red";
   Style_4 : constant String := "fill: none; stroke: green";
   Style_5 : constant String := "fill: none; stroke: cyan";

   SVG : SCSC.SVG.Document_Type := SCSC.SVG.SVG
      (
       Width  => 400,
       Height => 400,
       Child  => Circle (Center,   2, Style => Style_1)
               + Circle (Center,  50, Style => Style_2)
               + Circle (Center, 100, Style => Style_2)
               + Circle (Center, 150, Style => Style_2)
               + Circle (Center, 200, Style => Style_2)

               + Line (Cartesian (Center, P (117, 185), -50), Style => Style_5)
               + Line (Cartesian (Center, P (0, 200), -150), Style => Style_5)

               + Line (Cartesian (Center, P (250, 250), 50), Style => Style_3)
               + Line (Cartesian (Center, P (150, 150), -50), Style => Style_4)
               + Line (Cartesian (Center, P (400, 200), -150), Style => Style_3)
               + Line (Cartesian (Center, P (190, 210), 75), Style => Style_3)
      );
begin
   Put_Line (SCSC.SVG.To_String (SVG));
end Main;

with Ada.Text_IO; use Ada.Text_IO;
with SCSC.SVG;
with SCSC.Primitives;
with SCSC.Types;

procedure Main
is
   use SCSC.Primitives;
   use SCSC.SVG;
   use SCSC.Types;

   SVG : SCSC.SVG.Document_Type := SCSC.SVG.Create_SVG
      (Width  => 400,
       Height => 400,
       Child  => Group (Arc (Cartesian (From   => P (200, 400),
                                        To     => P (400, 200),
                                        Radius => 200,
                                        Sweep  => False),
                             Style => "fill: none; stroke: black") +
                        Circle (Center => P (50, 50),
                                Radius => 2,
                                Style  => "fill: black")));
begin
   Put_Line (SCSC.SVG.To_String (SVG));
end Main;

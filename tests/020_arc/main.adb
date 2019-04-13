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

   C : constant SCSC.Types.Point := P (50, 50);

   SVG : SCSC.SVG.Document_Type := SCSC.SVG.Create_SVG
      (Width  => 400,
       Height => 400,
       Child  => Group (Arc (Cartesian (From   => P (200, 400),
                                        To     => P (400, 200),
                                        Center => C,
                                        Radius => 200,
                                        Sweep  => False),
                             ID => "myarc") +
                        Circle (Center => C,
                                Radius => 2)),
       Style => ".circle { fill: black; } #myarc { fill:none; stroke: green; }");
begin
   Put_Line (SCSC.SVG.To_String (SVG));
end Main;

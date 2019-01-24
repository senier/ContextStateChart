with Ada.Text_IO; use Ada.Text_IO;
with SCSC.SVG;
with SCSC.Primitives;

procedure Main
is
   use SCSC.Primitives;
   use SCSC.SVG;

   SVG : SCSC.SVG.Document_Type := SCSC.SVG.SVG
      (Width  => 400,
       Height => 400,
       Child  => Group (Arc (From     => (200, 400),
                             To       => (400, 200),
                             X_Radius => 200,
                             Y_Radius => 200,
                             Sweep    => True) +
                        Arc (From     => (50, 50),
                             To       => (51, 50),
                             X_Radius => 3,
                             Y_Radius => 3,
                             Large    => True,
                             Style    => "fill")));
begin
   Put_Line (SCSC.SVG.To_String (SVG));
end Main;

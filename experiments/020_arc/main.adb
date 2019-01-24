with Ada.Text_IO; use Ada.Text_IO;
with SCSC.SVG;
with SCSC.Primitives;

procedure Main
is
   SVG : SCSC.SVG.Document_Type := SCSC.SVG.SVG
      (Width  => 400,
       Height => 400,
       Child  => SCSC.Primitives.Arc (From     => (200, 400),
                                      To       => (400, 200),
                                      X_Radius => 200,
                                      Y_Radius => 200,
                                      Sweep    => True));
begin
   Put_Line (SCSC.SVG.To_String (SVG));
end Main;

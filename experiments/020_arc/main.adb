with Ada.Text_IO; use Ada.Text_IO;
with SCSC.SVG;
with SCSC.Primitives;

procedure Main
is
   SVG : SCSC.SVG.Document_Type := SCSC.SVG.SVG (SCSC.Primitives.Arc);
begin
   Put_Line (SCSC.SVG.To_String (SVG));
end Main;

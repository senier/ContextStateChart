with Ada.Text_IO; use Ada.Text_IO;
with SCSC.SVG;

procedure Main
is
   SVG : SCSC.SVG.Document_Type := SCSC.SVG.SVG (200, 200);
begin
   Put_Line (SCSC.SVG.To_String (SVG));
end Main;

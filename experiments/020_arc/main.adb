with Ada.Text_IO; use Ada.Text_IO;
with SSVG;

procedure Main
is
   SVG : SSVG.Document_Type := SSVG.SVG (SSVG.Arc);
begin
   Put_Line (SSVG.To_String (SVG));
end Main;

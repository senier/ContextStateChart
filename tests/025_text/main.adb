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
       Child  => Text ((200, 200), Text => "This is a text", Style => "fill: green; color: red"));
begin
   Put_Line (SCSC.SVG.To_String (SVG));
end Main;

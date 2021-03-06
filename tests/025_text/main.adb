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
       Child  => Text (P (200, 200), Data => "This is a text"), Style => ".text { fill: green; color: red; }");
begin
   Put_Line (SCSC.SVG.To_String (SVG));
end Main;

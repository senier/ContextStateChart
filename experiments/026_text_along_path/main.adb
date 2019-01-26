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
       Child  => Circle ((200, 200), 100, ID => "path1", Style => "fill: none; stroke: black") +
                 Text ((200, 200), Text => "This is a text", Style => "fill: red", Path => "path1", DX => 50, DY => 20) +
                 Circle ((200, 200), 50, ID => "path2", Style => "fill: none; stroke: blue") +
                 Text ((200, 200), Text => "This is another text", Style => "fill: green", Path => "path2"));
begin
   Put_Line (SCSC.SVG.To_String (SVG));
end Main;

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
                 Arc (Polar ((200, 200), 70, 60.0, 180.0), ID => "path2", Style => "fill: none; stroke: blue") +
                 Text ((200, 200), Text => "Left aligned", Style => "fill: green", Path => "path2", Align => Align_Start) +
                 Arc (Polar ((200, 200), 30, 120.0, 180.0), ID => "path3", Style => "fill: none; stroke: blue") +
                 Text ((200, 200), Text => "Centered", Style => "fill: green", Path => "path3", Align => Align_Centered) +
                 Arc (Polar ((200, 200), 50, 180.0, 180.0), ID => "path4", Style => "fill: none; stroke: blue") +
                 Text ((200, 200), Text => "Right aligned", Style => "fill: green", Path => "path4", Align => Align_End));
begin
   Put_Line (SCSC.SVG.To_String (SVG));
end Main;

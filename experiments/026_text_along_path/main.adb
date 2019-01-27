with Ada.Text_IO; use Ada.Text_IO;
with SCSC.SVG;
with SCSC.Primitives;
with SCSC.Types;

procedure Main
is
   use SCSC.Primitives;
   use SCSC.SVG;
   use SCSC.Types;

   SVG : SCSC.SVG.Document_Type := SCSC.SVG.SVG
      (Width  => 400,
       Height => 400,
       Child  => Circle ((200, 200), 100, ID => "path1", Style => "fill: none; stroke: black") +
                 Text ((200, 200), Text => "This is a text", Style => "fill: red", Path => "path1", DX => (Px, 50), DY => (Px, 20)) +
                 Arc (Polar ((200, 200), 70, 60.0, 240.0), ID => "path2", Style => "fill: none; stroke: blue") +
                 Text ((200, 200), Text => "Left aligned (60->240)", Style => "fill: green", Path => "path2", Align => Align_Start) +
                 Arc (Polar ((200, 200), 30, 120.0, 300.0), ID => "path3", Style => "fill: none; stroke: blue") +
                 Text ((200, 200), Text => "Centered (120->300)", Style => "fill: green", Path => "path3", Align => Align_Centered) +
                 Arc (Polar ((200, 200), 50, 180.0, 360.0), ID => "path4", Style => "fill: none; stroke: blue") +
                 Text ((200, 200), Text => "Right aligned (180->360)", Style => "fill: green", Path => "path4", Align => Align_End) +
                 Arc (Polar ((200, 200), 130, 240.0, 20.0), ID => "path5", Style => "fill: none; stroke: blue") +
                 Text ((200, 200), Text => "Base middle (240->20)", Style => "fill: green", Path => "path5", DY => (Em, 0.4)) +
                 Arc (Polar ((200, 200), 150, 280.0, 20.0), ID => "path6", Style => "fill: none; stroke: blue") +
                 Text ((200, 200), Text => "Base top (280->20)", Style => "fill: green", Path => "path6", DY => (Px, -2)) +
                 Arc (Polar ((200, 200), 170, 200.0, 10.0), ID => "path7", Style => "fill: none; stroke: blue") +
                 Text ((200, 200), Text => "Base bottom (200->10)", Style => "fill: green", Path => "path7", DY => (Em, 1.0))
);
begin
   Put_Line (SCSC.SVG.To_String (SVG));
end Main;

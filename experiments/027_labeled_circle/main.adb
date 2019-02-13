with Ada.Text_IO; use Ada.Text_IO;
with SCSC.SVG;
with SCSC.Primitives;
with SCSC.Types;

procedure Main
is
   use SCSC.Primitives;
   use SCSC.SVG;
   use SCSC.Types;

   Center : Point := (200, 200);

   SVG : SCSC.SVG.Document_Type := SCSC.SVG.SVG
      (
       Width  => 400,
       Height => 400,
       Child  => Circle (Center, 2,   Style => "fill: black; stroke: black")
               + Circle (Center, 200, Style => "fill: none; stroke: black")

               + Arc  (Polar (Center, 190, 0.0, 90.0), ID => "path1", Style => "fill: none; stroke: blue")
               + Text (Center, Text => "Start 0⇒90", Style => "font-size: 10px; fill: blue", Path => "path1", Align => Align_Start)

               + Arc  (Polar (Center, 180, 90.0, 0.0), ID => "path2", Style => "fill: none; stroke: blue")
               + Text (Center, Text => "Start 90⇒0", Style => "font-size: 10px; fill: blue", Path => "path2", Align => Align_Start)

               + Arc  (Polar (Center, 170, 0.0, 90.0), ID => "path3", Style => "fill: none; stroke: blue")
               + Text (Center, Text => "Center 0⇒90", Style => "font-size: 10px; fill: blue", Path => "path3", Align => Align_Centered)

               + Arc  (Polar (Center, 160, 90.0, 0.0), ID => "path4", Style => "fill: none; stroke: blue")
               + Text (Center, Text => "Center 90⇒0", Style => "font-size: 10px; fill: blue", Path => "path4", Align => Align_Centered)

               + Arc  (Polar (Center, 150, 0.0, 90.0), ID => "path5", Style => "fill: none; stroke: blue")
               + Text (Center, Text => "End 0⇒90", Style => "font-size: 10px; fill: blue", Path => "path5", Align => Align_End)

               + Arc  (Polar (Center, 140, 90.0, 0.0), ID => "path6", Style => "fill: none; stroke: blue")
               + Text (Center, Text => "End 90⇒0", Style => "font-size: 10px; fill: blue", Path => "path6", Align => Align_End)

               + Arc  (Polar (Center, 130, 315.0, 45.0), ID => "path7", Style => "fill: none; stroke: blue")
               + Text (Center, Text => "Start 315⇒45", Style => "font-size: 10px; fill: blue", Path => "path7", Align => Align_Start)

               + Arc  (Polar (Center, 120, 45.0, 315.0), ID => "path8", Style => "fill: none; stroke: blue")
               + Text (Center, Text => "Start 45⇒315", Style => "font-size: 10px; fill: blue", Path => "path8", Align => Align_Start)

               + Arc  (Polar (Center, 110, 315.0, 45.0), ID => "path9", Style => "fill: none; stroke: blue")
               + Text (Center, Text => "Center 315⇒45", Style => "font-size: 10px; fill: blue", Path => "path9", Align => Align_Centered)

               + Arc  (Polar (Center, 100, 45.0, 315.0), ID => "path10", Style => "fill: none; stroke: blue")
               + Text (Center, Text => "Center 45⇒315", Style => "font-size: 10px; fill: blue", Path => "path10", Align => Align_Centered)

               + Arc  (Polar (Center, 90, 315.0, 45.0), ID => "path11", Style => "fill: none; stroke: blue")
               + Text (Center, Text => "End 315⇒45", Style => "font-size: 10px; fill: blue", Path => "path11", Align => Align_End)

               + Arc  (Polar (Center, 80, 45.0, 315.0), ID => "path12", Style => "fill: none; stroke: blue")
               + Text (Center, Text => "End 45⇒315", Style => "font-size: 10px; fill: blue", Path => "path12", Align => Align_End)
      );
begin
   Put_Line (SCSC.SVG.To_String (SVG));
end Main;

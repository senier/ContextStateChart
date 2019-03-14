with Ada.Text_IO; use Ada.Text_IO;
with SCSC.SVG;
with SCSC.Primitives;
with SCSC.Types;

procedure Main
is
   use SCSC.Primitives;
   use SCSC.SVG;
   use SCSC.Types;

   Center : Point := P (200, 200);

   SVG : SCSC.SVG.Document_Type := SCSC.SVG.Create_SVG
      (Width  => 400,
       Height => 400,
       Child  => Circle (Center, 100, ID => "path1", Style => "fill: none; stroke: black") +
                 Text (Center, Data => "This is a text", Style => "font-size: 10px; fill: red", Path_Name => "path1", DX => (Px, 50), DY => (Px, 20)) +
                 Arc (Polar (Center, 70, 60.0, 240.0), ID => "path2", Style => "fill: none; stroke: blue") +
                 Text (Center, Data => "Left aligned (60->240)", Style => "font-size: 10px; fill: green", Path_Name => "path2", Align => Align_Start) +
                 Arc (Polar (Center, 30, 120.0, 300.0), ID => "path3", Style => "fill: none; stroke: blue") +
                 Text (Center, Data => "Centered (120->300)", Style => "font-size: 10px; fill: green", Path_Name => "path3", Align => Align_Centered) +
                 Arc (Polar (Center, 50, 180.0, 0.0), ID => "path4", Style => "fill: none; stroke: blue") +
                 Text (Center, Data => "Right aligned (180->360)", Style => "font-size: 10px; fill: green", Path_Name => "path4", Align => Align_End) +
                 Arc (Polar (Center, 130, 240.0, 20.0), ID => "path5", Style => "fill: none; stroke: blue") +
                 Text (Center, Data => "Base middle (240->20)", Style => "font-size: 10px; fill: green", Path_Name => "path5", DY => (Em, 0.4)) +
                 Arc (Polar (Center, 150, 280.0, 20.0), ID => "path6", Style => "fill: none; stroke: blue") +
                 Text (Center, Data => "Base top (280->20)", Style => "font-size: 10px; fill: green", Path_Name => "path6", DY => (Px, -2)) +
                 Arc (Polar (Center, 170, 200.0, 10.0), ID => "path7", Style => "fill: none; stroke: blue") +
                 Text (Center, Data => "Base bottom (200->10)", Style => "font-size: 10px; fill: green", Path_Name => "path7", DY => (Em, 1.0))
);
begin
   Put_Line (SCSC.SVG.To_String (SVG));
end Main;

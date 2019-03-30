with Ada.Text_IO; use Ada.Text_IO;
with SCSC.SVG;
with SCSC.Primitives;
with SCSC.Types;
with SXML.Generator;

procedure Main
is
   use SCSC.Primitives;
   use SCSC.SVG;
   use SCSC.Types;
   use SXML.Generator;

   Center : Point := P (200, 200);

   SVG : SCSC.SVG.Document_Type := SCSC.SVG.Create_SVG
      (
       Width  => 400,
       Height => 400,
       Child  => Circle (Center, 2)
               + Circle (Center, 200, ID => "border")

               + Arc  (Polar (Center, 190, 0.0, 90.0), ID => "path1")
               + Text (Center, Data => "Start 0⇒90", Path_Name => "path1", Align => Align_Start)

               + Arc  (Polar (Center, 180, 90.0, 0.0), ID => "path2")
               + Text (Center, Data => "Start 90⇒0", Path_Name => "path2", Align => Align_Start)

               + Arc  (Polar (Center, 170, 0.0, 90.0), ID => "path3")
               + Text (Center, Data => "Center 0⇒90", Path_Name => "path3", Align => Align_Centered)

               + Arc  (Polar (Center, 160, 90.0, 0.0), ID => "path4")
               + Text (Center, Data => "Center 90⇒0", Path_Name => "path4", Align => Align_Centered)

               + Arc  (Polar (Center, 150, 0.0, 90.0), ID => "path5")
               + Text (Center, Data => "End 0⇒90", Path_Name => "path5", Align => Align_End)

               + Arc  (Polar (Center, 140, 90.0, 0.0), ID => "path6")
               + Text (Center, Data => "End 90⇒0", Path_Name => "path6", Align => Align_End)

               + Arc  (Polar (Center, 130, 315.0, 45.0), ID => "path7")
               + Text (Center, Data => "Start 315⇒45", Path_Name => "path7", Align => Align_Start)

               + Arc  (Polar (Center, 120, 45.0, 315.0), ID => "path8")
               + Text (Center, Data => "Start 45⇒315", Path_Name => "path8", Align => Align_Start)

               + Arc  (Polar (Center, 110, 315.0, 45.0), ID => "path9")
               + Text (Center, Data => "Center 315⇒45", Path_Name => "path9", Align => Align_Centered)

               + Arc  (Polar (Center, 100, 45.0, 315.0), ID => "path10")
               + Text (Center, Data => "Center 45⇒315", Path_Name => "path10", Align => Align_Centered)

               + Arc  (Polar (Center, 90, 315.0, 45.0), ID => "path11")
               + Text (Center, Data => "End 315⇒45", Path_Name => "path11", Align => Align_End)

               + Arc  (Polar (Center, 80, 45.0, 315.0), ID => "path12")
               + Text (Center, Data => "End 45⇒315", Path_Name => "path12", Align => Align_End),

         Style => ".arc { fill: none; stroke: blue; } .text { font-size: 10px; fill: blue; } "
                  & "circle { fill: black; stroke: black; } #border { fill: none; stroke: black; }"
      );
begin
   Put_Line (SCSC.SVG.To_String (SVG));
end Main;

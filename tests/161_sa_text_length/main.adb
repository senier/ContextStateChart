with Ada.Text_IO; use Ada.Text_IO;
with SCSC.SVG;
with SCSC.Primitives;
with SCSC.Types;
with SCSC.Text;
with SXML.Generator;

procedure Main
is
   use SCSC.Primitives;
   use SCSC.SVG;
   use SCSC.Types;
   use SXML.Generator;

   Center : Point := P (550, 450);
   Data_01 : constant String := "This is a text";
   Data_02 : constant String := "WAIT_EE.PARSE_APPLICATION_LAYER_PROTOCOL_NEGOTIATION";
   Data_03 : constant String := "Certificate";
   Data_04 : constant String := "CONNECTED.PHA_PARSE_CR.PARSE_SIGNATURE_ALGORITHMS_CERT";
   Data_05 : constant String := "datastore_in";
   Data_06 : constant String := "WAIT_CV";

   SVG : SCSC.SVG.Document_Type := SCSC.SVG.Create_SVG
      (Width  => 1000,
       Height => 1000,
       Child  => Line (Polar (Center, 20, SCSC.Text.Estimate_Width (Data_01, 10) + 20, 90.0), ID => "path1")
               + Text (Center, Data => Data_01, ID => "text1", Path_Name => "path1")
               + Line (Polar (Center, 20, SCSC.Text.Estimate_Width (Data_02, 10) + 20, 60.0), ID => "path2")
               + Text (Center, Data => Data_02, ID => "text2", Path_Name => "path2")
               + Line (Polar (Center, 20, SCSC.Text.Estimate_Width (Data_03, 10) + 20, 30.0), ID => "path3")
               + Text (Center, Data => Data_03, ID => "text3", Path_Name => "path3")
               + Line (Polar (Center, 20, SCSC.Text.Estimate_Width (Data_04, 10) + 20, 00.0), ID => "path4")
               + Text (Center, Data => Data_04, ID => "text4", Path_Name => "path4")
               + Line (Polar (Center, 20, SCSC.Text.Estimate_Width (Data_05, 10) + 20, 330.0), ID => "path5")
               + Text (Center, Data => Data_05, ID => "text5", Path_Name => "path5")
               + Line (Polar (Center, 20, SCSC.Text.Estimate_Width (Data_06, 10) + 20, 300.0), ID => "path6")
               + Text (Center, Data => Data_06, ID => "text6", Path_Name => "path6")

               + Line (Polar (Center, 20, SCSC.Text.Estimate_Width (Data_02, 14) + 20, 270.0), ID => "path7")
               + Text (Center, Data => Data_02, Class => "text14", Path_Name => "path7")
               + Line (Polar (Center, 20, SCSC.Text.Estimate_Width (Data_03, 14) + 20, 240.0), ID => "path8")
               + Text (Center, Data => Data_03, Class => "text14", Path_Name => "path8")
               + Line (Polar (Center, 20, SCSC.Text.Estimate_Width (Data_04, 14) + 20, 210.0), ID => "path9")
               + Text (Center, Data => Data_04, Class => "text14", Path_Name => "path9")
               + Line (Polar (Center, 20, SCSC.Text.Estimate_Width (Data_05, 14) + 20, 180.0), ID => "path10")
               + Text (Center, Data => Data_05, Class => "text14", Path_Name => "path10")
               + Line (Polar (Center, 20, SCSC.Text.Estimate_Width (Data_06, 14) + 20, 150.0), ID => "path11")
               + Text (Center, Data => Data_06, Class => "text14", Path_Name => "path11")
               ,
       Style => ".text { font-size: 10px; fill: green; } "
                & ".text14 { font-size: 14px; fill: blue; } "
                & ".line { fill: none; stroke: blue; } #text1 { fill: red; } ");
begin
   Put_Line (SCSC.SVG.To_String (SVG));
end Main;

with Ada.Text_IO; use Ada.Text_IO;
with Util;
with SCSC.SVG;
with SCSC.Types;
with SCSC.Primitives;
with SCSC.Graph;
with SCSC.GEXF;

procedure Main
is
   use SCSC.Types;
   use SCSC.Primitives;
   use SCSC.Graph;
   use SCSC.GEXF;
   use SCSC.SVG;
   use type Element_Type;

   Arrow_End : Element_Type := Path (Commands => ((Moveto, Absolute, 0, 2),
                                                 (Lineto, Absolute, 4, 4),
                                                 (Lineto, Absolute, 4, 0),
                                                 (ZClosepath, Absolute)),
                                    Style => "fill: blue");

   GEXF_File : String := Util.Read_File ("tests/data/client_handshake_states.gexf").all;

   Data : access Data_Type := new Data_Type (1 .. GEXF_File'Length);
   Last : Natural;
begin
   Import (GEXF     => GEXF_File,
           Data     => Data.all,
           Last     => Last,
           Level_ID => "kind",
           Levels   => "channel<<data");
   if Last = 0
   then
      Put_Line ("!!! Invalid GEXF input data !!!");
      return;
   end if;

   declare
      Doc : Document_Type := SVG
         (Width  => 2000,
          Height => 2000,
          Child  => Graph (Params => Polar (Center => P (1000, 1000), Offset => 1000, Radius => 20, Layer_Spacing => 80, Padding => 5),
                           Data   => Data.all (Data.all'First .. Last),
                           Style  => "fill: yellow; stroke: black",
                           Text_Style  => "fill: black; stroke: none; font-size: 10px",
                           Connector_Style => "fill: none; stroke: blue"),
          Defs   => Marker (Element => Arrow_End, Width => 4, Height => 4, RefX => 0.1, RefY => 2.0, ID => "End_Arrow"));
   begin
      Put_Line (To_String (Doc));
   end;
end Main;

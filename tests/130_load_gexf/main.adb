with Ada.Text_IO; use Ada.Text_IO;
with Util;
with SXML.Serialize;
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
   use type Document_Type;

   Arrow_End : Document_Type := Path (Commands => ((Moveto, Absolute, 0, 2),
                                                   (Lineto, Absolute, 4, 4),
                                                   (Lineto, Absolute, 4, 0),
                                                   (ZClosepath, Absolute)),
                                    Style => "fill: blue");

   GEXF_File : String := Util.Read_File ("tests/data/client_handshake_states.gexf").all;

   Data   : access Data_Type := new Data_Type (1 .. GEXF_File'Length);
   Output : access String := new String (1 .. 1000000);
   Stack  : access SXML.Serialize.Stack_Type := new SXML.Serialize.Stack_Type (1 .. 10000);
   Last : Natural;
   Output_Last : Integer;
begin
   Import (GEXF_Data => GEXF_File,
           Data      => Data.all,
           Last      => Last,
           Label     => "xlabel",
           Level     => "kind",
           Levels    => "channel<<data");
   if Last = 0
   then
      Put_Line ("!!! Invalid GEXF input data !!!");
      return;
   end if;

   declare
      Doc : Document_Type := Create_SVG
         (Width  => 2000,
          Height => 2000,
          Child  => Create_Graph (Params => Create_Polar (Center        => P (1000, 1000),
                                                          Offset        => 1000,
                                                          Radius        => 20,
                                                          Layer_Spacing => 80,
                                                          Padding       => 5),
                                  Data   => Data.all (Data.all'First .. Last),
                                  Style  => "fill: yellow; stroke: black",
                                  Text_Style  => "fill: black; stroke: none; font-size: 10px",
                                  Connector_Style => "fill: none; stroke: blue"),
          Defs   => Marker (Element => Arrow_End, Width => 4, Height => 4, RefX => 0.1, RefY => 2.0, MID => "End_Arrow"));
   begin
      SXML.Serialize.Initialize (Stack.all);
      To_String (Doc, Output.all, Output_Last, Stack.all);
      if Output_Last >= 0 then
         Put_Line (Output.all (Output'First .. Output_Last));
      else
         Put_Line ("#Invalid#");
      end if;
   end;

end Main;

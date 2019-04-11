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
                                      Class => "arrow");

   GEXF_File : String := Util.Read_File ("tests/data/client_handshake_states.gexf").all;

   Data   : access Data_Type := new Data_Type (1 .. GEXF_File'Length);
   Output : access String := new String (1 .. 1000000);
   Stack  : access SXML.Serialize.Stack_Type := new SXML.Serialize.Stack_Type (1 .. 10000);
   Last : Natural;
   Output_Last : Integer;

   Params : constant SCSC.Graph.Graph_Params_Type := Create_Polar (Center  => P (1000, 1000),
                                                                   Radius  => 20,
                                                                   Spacing => (150, 300, 450),
                                                                   Padding => 5);

   EP     : constant SCSC.Graph.Energy_Params_Type := SCSC.Graph.Create_Energy_Params;

   Unused : Long_Integer;

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
      Sectors   : SCSC.Graph.Annular_Sectors_Type (Data.all'First .. Last);
      Positions : SCSC.Graph.Positions_Type (Data.all'First .. Last);
      Length    : Natural;
   begin
      SCSC.Graph.Identity (Positions);
      SCSC.Graph.Layout (Params    => Params,
                         EP        => EP,
                         Data      => Data.all (Data.all'First .. Last),
                         ID        => "G1",
                         Sectors   => Sectors,
                         Length    => Length,
                         Positions => Positions,
                         Energy    => Unused);
      declare
         Doc : Document_Type := Create_SVG
            (Width  => 2000,
             Height => 2000,
             Child  => Create_Graph (Params    => Params,
                                     Sectors   => Sectors,
                                     Length    => Length,
                                     Data      => Data.all (Data.all'First .. Last),
                                     Positions => Positions),
             Defs   => Marker (Element => Arrow_End, Width => 4, Height => 4, RefX => 0.1, RefY => 2.0, ID => "End_Arrow"),
             Style  => ".arrow { fill: blue; } .fill_black { fill: black; stroke: none; } "
                       & ".connector { fill: none; stroke: blue; } "
                       & ".text { fill: green; stroke: none; font-size: 10px; } "
                       & ".graph { fill: yellow; stroke: black; } "
                       & ".annular_sector { fill: yellow; stroke: red; } "
                       & ".connector_end { marker-start: url(#End_Arrow); } ");
      begin
         SXML.Serialize.Initialize (Stack.all);
         To_String (Doc, Output.all, Output_Last, Stack.all);
         if Output_Last >= 0 then
            Put_Line (Output.all (Output'First .. Output_Last));
         else
            Put_Line ("#Invalid#");
         end if;
      end;
   end;

end Main;

with SCSC.Graph;

package SCSC.GEXF is

   procedure Import (GEXF     :     String;
                     Data     : out Graph.Data_Type;
                     Last     : out Natural;
                     Level_ID :     String := "";
                     Levels   :     String := "")
   with
      SPARK_Mode,
      Pre => GEXF'First <= GEXF'Last and then
             GEXF'Last <= Natural'Last - 8 and then
             GEXF'Length >= 12;
   --  Import graph data in GEXF format

end SCSC.GEXF;

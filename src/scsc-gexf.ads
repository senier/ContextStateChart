with SCSC.Graph;

package SCSC.GEXF is

   procedure Import (GEXF_Data :     String;
                     Data      : out Graph.Data_Type;
                     Last      : out Natural;
                     Level_ID  :     String := "";
                     Levels    :     String := "")
   with
      SPARK_Mode,
      Pre => GEXF_Data'First <= GEXF_Data'Last and then
             GEXF_Data'Last <= Natural'Last - 8 and then
             GEXF_Data'Length >= 12;
   --  Import graph data in GEXF format

end SCSC.GEXF;

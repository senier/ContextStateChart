with SCSC.Primitives;
with SCSC.Graph;

generic
   Max_Unsuccessful_Iterations     : Long_Integer :=     10;
   Initial_Acceptance_Threshold    : Float        := 2000.0;
   Threshold_Decay                 : Float        :=   0.95;

   Factor_Sector_Too_Wide          : Long_Integer :=   10;
   Factor_Sector_Too_Narrow        : Long_Integer := 1000;
   Factor_Level_Spacing_Too_Wide   : Long_Integer :=   10;
   Factor_Level_Spacing_Too_Narrow : Long_Integer :=  500;

   Factor_Radius_Spacing           : Long_Integer :=   5;
   Text_Border                     : Long_Integer :=  20;

   Level_Spacing_Increase_Step     : Natural := 20;
   Level_Spacing_Decrease_Step     : Natural := 20;

   Debug : Boolean := False;

package SCSC.Simulated_Annealing  is

   function Energy (Params : Primitives.Annular_Sector_Params_Type;
                    Text   : String;
                    Size   : Natural) return Long_Integer;

   function Energy (Params  : Graph.Graph_Params_Type;
                    Data    : Graph.Data_Type;
                    Sectors : Graph.Annular_Sectors_Type;
                    Size    : Natural) return Long_Integer;

   procedure Print_Debug (Iteration : Long_Integer;
                          Energy_1  : Long_Integer;
                          Energy_2  : Long_Integer;
                          Threshold : Long_Integer);

   procedure Optimize (ID        :        String;
                       Font_Size :        Natural;
                       Length    :    out Natural;
                       Params    : in out Graph.Graph_Params_Type;
                       Data      : in out Graph.Data_Type;
                       Sectors   : in out Graph.Annular_Sectors_Type;
                       Positions :        Graph.Positions_Type);

end SCSC.Simulated_Annealing;

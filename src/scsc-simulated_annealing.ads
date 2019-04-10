with SCSC.Primitives;
with SCSC.Graph;

generic
   Max_Unsuccessful_Iterations     : Long_Integer :=     10;
   Initial_Acceptance_Threshold    : Float        := 2000.0;
   Threshold_Decay                 : Float        :=   0.95;
   Level_Spacing_Increase_Step     : Natural := 20;
   Level_Spacing_Decrease_Step     : Natural := 20;
   Debug : Boolean := False;
package SCSC.Simulated_Annealing  is

   procedure Print_Debug (Iteration : Long_Integer;
                          Energy_1  : Long_Integer;
                          Energy_2  : Long_Integer;
                          Threshold : Long_Integer);

   procedure Optimize (ID        :        String;
                       Length    :    out Natural;
                       Params    : in out Graph.Graph_Params_Type;
                       Data      : in out Graph.Data_Type;
                       Sectors   : in out Graph.Annular_Sectors_Type;
                       Positions :        Graph.Positions_Type);

   procedure Set_Direction (Data : in out Graph.Data_Type;
                            NI   :        Positive;
                            EI   :        Natural;
                            Dir  :        Primitives.Dir_Type);

end SCSC.Simulated_Annealing;

with SCSC.Primitives;
with SCSC.Graph;

package SCSC.SA
   with SPARK_Mode
is

   type Params_Type is private;

   function Create_Optimize_Params (Max_Unsuccessful_Iterations  : Long_Integer :=     10;
                                    Initial_Acceptance_Threshold : Float        := 2000.0;
                                    Threshold_Decay              : Float        :=   0.95;
                                    Level_Spacing_Increase_Step  : Natural      := 20;
                                    Level_Spacing_Decrease_Step  : Natural      := 20;
                                    Debug                        : Boolean      := False) return Params_Type;

   procedure Print_Debug (Iteration : Long_Integer;
                          Energy_1  : Long_Integer;
                          Energy_2  : Long_Integer;
                          Threshold : Long_Integer);

   procedure Optimize (ID              :        String;
                       Optimize_Params :        Params_Type;
                       Energy_Params   :        Graph.Energy_Params_Type;
                       Positions       :        Graph.Positions_Type;
                       Length          :    out Natural;
                       Graph_Params    : in out Graph.Graph_Params_Type;
                       Graph_Data      : in out Graph.Data_Type;
                       Sectors         : in out Graph.Annular_Sectors_Type);

   procedure Set_Direction (Graph_Data : in out Graph.Data_Type;
                            Node_Index :        Positive;
                            Edge_Index :        Natural;
                            Direction  :        Primitives.Dir_Type);

private

   type Params_Type is
   record
      Max_Unsuccessful_Iterations  : Long_Integer :=     10;
      Initial_Acceptance_Threshold : Float        := 2000.0;
      Threshold_Decay              : Float        :=   0.95;
      Level_Spacing_Increase_Step  : Natural      := 20;
      Level_Spacing_Decrease_Step  : Natural      := 20;
      Debug                        : Boolean      := False;
   end record;

   function Create_Optimize_Params (Max_Unsuccessful_Iterations  : Long_Integer :=     10;
                                    Initial_Acceptance_Threshold : Float        := 2000.0;
                                    Threshold_Decay              : Float        :=   0.95;
                                    Level_Spacing_Increase_Step  : Natural      := 20;
                                    Level_Spacing_Decrease_Step  : Natural      := 20;
                                    Debug                        : Boolean      := False) return Params_Type is
   ((Max_Unsuccessful_Iterations  =>     10,
     Initial_Acceptance_Threshold => 2000.0,
     Threshold_Decay              =>   0.95,
     Level_Spacing_Increase_Step  =>     20,
     Level_Spacing_Decrease_Step  =>     20,
     Debug                        => False));

end SCSC.SA;

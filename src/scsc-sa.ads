with SCSC.Primitives;
with SCSC.Graph;

package SCSC.SA
   with SPARK_Mode
is

   type Params_Type is private;

   type Moves is
      (Move_Noop,
       Move_Decrease_Random_Level_Spacing,
       Move_Increase_Random_Level_Spacing,
       Move_Switch_Random_Direction);

   type Move_Type (Kind : Moves := Move_Noop) is private;

   function Create_Optimize_Params (Max_Unsuccessful_Iterations  : Long_Integer :=     10;
                                    Initial_Acceptance_Threshold : Float        := 2000.0;
                                    Threshold_Decay              : Float        :=   0.95;
                                    Level_Spacing_Increase_Step  : Natural      := 20;
                                    Level_Spacing_Decrease_Step  : Natural      := 20;
                                    Debug                        : Boolean      := False) return Params_Type;

   procedure Print_Debug (Iteration : Long_Integer;
                          M         : Move_Type;
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

   type Move_Type (Kind : Moves := Move_Noop) is
   record
      case Kind is
         when Move_Decrease_Random_Level_Spacing
            | Move_Increase_Random_Level_Spacing =>
            Spacing_Index : Graph.Spacing_Index;
            Spacing_Value : Natural;
         when Move_Switch_Random_Direction =>
            Direction  : Primitives.Dir_Type;
            Node_Index : Positive;
            Edge_Index : Natural;
         when Move_Noop =>
            null;
      end case;
   end record;

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
   ((Max_Unsuccessful_Iterations  => Max_Unsuccessful_Iterations,
     Initial_Acceptance_Threshold => Initial_Acceptance_Threshold,
     Threshold_Decay              => Threshold_Decay,
     Level_Spacing_Increase_Step  => Level_Spacing_Increase_Step,
     Level_Spacing_Decrease_Step  => Level_Spacing_Decrease_Step,
     Debug                        => Debug));

end SCSC.SA;

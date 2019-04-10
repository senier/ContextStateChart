with SCSC.Primitives;
with SCSC.Graph;

generic
package SCSC.Simulated_Annealing  is

   type Params_Type is private;

   function Create_Opt_Params (Max_Unsuccessful_Iterations  : Long_Integer :=     10;
                               Initial_Acceptance_Threshold : Float        := 2000.0;
                               Threshold_Decay              : Float        :=   0.95;
                               Level_Spacing_Increase_Step  : Natural      := 20;
                               Level_Spacing_Decrease_Step  : Natural      := 20;
                               Debug                        : Boolean      := False) return Params_Type;

   procedure Print_Debug (Iteration : Long_Integer;
                          Energy_1  : Long_Integer;
                          Energy_2  : Long_Integer;
                          Threshold : Long_Integer);

   procedure Optimize (ID         :        String;
                       Opt_Params :        Params_Type;
                       Length     :    out Natural;
                       Params     : in out Graph.Graph_Params_Type;
                       Data       : in out Graph.Data_Type;
                       Sectors    : in out Graph.Annular_Sectors_Type;
                       Positions  :        Graph.Positions_Type);

   procedure Set_Direction (Data : in out Graph.Data_Type;
                            NI   :        Positive;
                            EI   :        Natural;
                            Dir  :        Primitives.Dir_Type);

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

   function Create_Opt_Params (Max_Unsuccessful_Iterations  : Long_Integer :=     10;
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

end SCSC.Simulated_Annealing;

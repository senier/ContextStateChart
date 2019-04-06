with SCSC.Primitives;
with SCSC.Graph;

generic
   Initial_Temperature             : Long_Integer := 1_000_000_000;
   Temperature_Delta               : Long_Integer := 1;

   Factor_Sector_Too_Wide          : Natural :=    3;
   Factor_Sector_Too_Narrow        : Natural := 1000;
   Factor_Level_Spacing_Too_Wide   : Natural :=   10;
   Factor_Level_Spacing_Too_Narrow : Natural :=  100;
   Factor_Radius_Spacing           : Natural :=    3;
package SCSC.Simulated_Annealing  is

   function Energy (Params : Primitives.Annular_Sector_Params_Type;
                    Text   : String;
                    Size   : Natural) return Natural;

   function Energy (Params  : Graph.Graph_Params_Type;
                    Data    : Graph.Data_Type;
                    Sectors : Graph.Annular_Sectors_Type;
                    Size    : Natural) return Natural;

   procedure Optimize (Params  : in out Graph.Graph_Params_Type;
                       Data    : in out Graph.Data_Type;
                       Sectors : in out Graph.Annular_Sectors_Type;
                       Size    :        Natural);

private

   procedure Move (Params  : in out Graph.Graph_Params_Type;
                   Data    : in out Graph.Data_Type;
                   Sectors : in out Graph.Annular_Sectors_Type;
                   Size    :        Natural);

   function Accept_Worse (Energy_1    : Natural;
                          Energy_2    : Natural;
                          Temperature : Long_Integer) return Boolean;

end SCSC.Simulated_Annealing;

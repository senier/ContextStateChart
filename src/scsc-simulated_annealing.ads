with SCSC.Primitives;
with SCSC.Graph;

generic
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

end SCSC.Simulated_Annealing;

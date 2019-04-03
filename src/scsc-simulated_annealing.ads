with SCSC.Primitives;

generic
   Factor_Sector_Too_Wide   : Natural :=  10;
   Factor_Sector_Too_Narrow : Natural := 100;
package SCSC.Simulated_Annealing  is

   function Energy (Params : Primitives.Annular_Sector_Params_Type;
                    Text   : String;
                    Size   : Natural) return Natural;

end SCSC.Simulated_Annealing;

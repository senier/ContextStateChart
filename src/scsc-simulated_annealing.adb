with SCSC.Text;

package body SCSC.Simulated_Annealing is

   function Energy (Params : Primitives.Annular_Sector_Params_Type;
                    Text   : String;
                    Size   : Natural) return Natural
   is
      Diff : constant Integer := SCSC.Text.Estimate_Width (Text, Size) + 10 - Params.Inner.Length;
   begin
      return (if Diff < 0
              then (-Diff) * Factor_Sector_Too_Wide
              else Diff * Factor_Sector_Too_Narrow);
   end Energy;

end SCSC.Simulated_Annealing;

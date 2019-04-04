with SCSC.Text;

package body SCSC.Simulated_Annealing is

   ------------
   -- Energy --
   ------------

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

   ------------
   -- Energy --
   ------------

   function Energy (Params    : Graph.Graph_Params_Type;
                    Data      : Graph.Data_Type;
                    Sectors   : Graph.Annular_Sectors_Type;
                    Size      : Natural) return Natural
   is
      Result : Natural := 0;
   begin
      for I in Sectors'Range
      loop
         --  FIXME: Only one font size supported
         Result := Result + Energy (Sectors (I), Data (I).Get_Label, Size);
      end loop;

      for S of Graph.Get_Spacing (Params)
      loop
         declare
            Diff : constant Integer := 2 * Graph.Get_Radius (Params) - S;
         begin
            Result := Result + (if Diff < 0
                                then (-Diff) * Factor_Level_Spacing_Too_Wide
                                else Diff * Factor_Level_Spacing_Too_Narrow);
         end;
      end loop;

      return Result;
   end Energy;

end SCSC.Simulated_Annealing;

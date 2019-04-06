with SCSC.Text;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Numerics.Float_Random;

package body SCSC.Simulated_Annealing is

   G : Ada.Numerics.Float_Random.Generator;

   package GEF is new Ada.Numerics.Generic_Elementary_Functions (Float);

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
            Diff : constant Integer := Factor_Radius_Spacing * Graph.Get_Radius (Params) - S;
         begin
            Result := Result + (if Diff < 0
                                then (-Diff) * Factor_Level_Spacing_Too_Wide
                                else Diff * Factor_Level_Spacing_Too_Narrow);
         end;
      end loop;

      return Result;
   end Energy;

   ----------
   -- Move --
   ----------

   procedure Move (Params  : in out Graph.Graph_Params_Type;
                   Data    : in out Graph.Data_Type;
                   Sectors : in out Graph.Annular_Sectors_Type;
                   Size    :        Natural)
   is
   begin
      null;
   end Move;

   ----------------
   -- Acceptable --
   ----------------

   function Accept_Worse (Energy_1    : Natural;
                          Energy_2    : Natural;
                          Temperature : Long_Integer) return Boolean
   is
      Acceptance_Probability : constant Float := GEF.Exp (Float (Energy_1 - Energy_2) / Float (Temperature));
   begin
      return Acceptance_Probability > Ada.Numerics.Float_Random.Random (G);
   end Accept_Worse;

   --------------
   -- Optimize --
   --------------

   procedure Optimize (Params  : in out Graph.Graph_Params_Type;
                       Data    : in out Graph.Data_Type;
                       Sectors : in out Graph.Annular_Sectors_Type;
                       Size    :        Natural)
   is
      Temperature : Long_Integer := Initial_Temperature;
      Params_2    : Graph.Graph_Params_Type := Params;
      Data_2      : Graph.Data_Type := Data;
      Sectors_2   : Graph.Annular_Sectors_Type := Sectors;

      Energy_1 : Natural := Energy (Params, Data, Sectors, Size);
      Energy_2 : Natural;
   begin
      while Temperature > 0
      loop
         Move (Params_2, Data_2, Sectors_2, Size);
         Energy_2 := Energy (Params_2, Data_2, Sectors_2, Size);

         if Energy_2 < Energy_1
            or else Accept_Worse (Energy_1, Energy_2, Temperature)
         then
            Params   := Params_2;
            Data     := Data_2;
            Sectors  := Sectors_2;
            Energy_1 := Energy_2;
         end if;

         Temperature := Temperature - Temperature_Delta;
      end loop;
   end Optimize;

begin
   Ada.Numerics.Float_Random.Reset (G);
end SCSC.Simulated_Annealing;

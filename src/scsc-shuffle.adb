with Ada.Numerics.Discrete_Random;

package body SCSC.Shuffle
   with SPARK_Mode => Off
is

   package DR is new Ada.Numerics.Discrete_Random (Result_Subtype => Integer);

   procedure Generic_Shuffle (Data : in out Data_Type) is
      Tmp : Element_Type;
      K   : Integer;
      G   : DR.Generator;
   begin
      DR.Reset (G);
      for I in reverse Data'Range loop
         K := (DR.Random (G) mod I) + 1;
         Tmp := Data (I);
         Data (I) := Data (K);
         Data (K) := Tmp;
      end loop;
   end Generic_Shuffle;

end SCSC.Shuffle;

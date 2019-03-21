with Ada.Numerics.Discrete_Random;

package body SCSC.Random
   with SPARK_Mode => Off
is
   package DR is new Ada.Numerics.Discrete_Random (Result_Type);
   G : DR.Generator;

   function Get_Random return Result_Type
   is
   begin
      return DR.Random (G);
   end Get_Random;

begin
   DR.Reset (G);
end SCSC.Random;

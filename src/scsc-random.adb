with Ada.Numerics.Discrete_Random;

package body SCSC.Random
   with SPARK_Mode => Off
is

   package DR is new Ada.Numerics.Discrete_Random (Natural);

   G : DR.Generator;

   function Random return Natural
   is
   begin
      return DR.Random (G);
   end Random;

begin
   DR.Reset (G);
end SCSC.Random;


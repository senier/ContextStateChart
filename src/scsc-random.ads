generic
   type Result_Type is (<>);
package SCSC.Random
   with SPARK_Mode
is

   function Get_Random return Result_Type with
      Volatile_Function;
   --  Return random value

end SCSC.Random;

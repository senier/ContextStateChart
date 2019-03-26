package SCSC.Shuffle
   with SPARK_Mode
is

   generic
      type Element_Type is private;
      type Data_Type is array (Positive range <>) of Element_Type;
   procedure Generic_Shuffle (Data : in out Data_Type);

end SCSC.Shuffle;

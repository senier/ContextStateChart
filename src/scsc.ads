package SCSC
   with SPARK_Mode => On
is
   function To_ID (I : Natural) return String is (I'Img (I'Img'First + 1 .. I'Img'Last));

end SCSC;

package SCSC.Types
   with SPARK_Mode => On
is

   type Point is
   record
      X : Integer;
      Y : Integer;
   end record;

   type Angle is digits 4 range -360.0 .. 360.0;

end SCSC.Types;

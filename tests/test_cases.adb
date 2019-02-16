with SCSC.Types;
with AUnit.Assertions;

package body Test_Cases
is
   use AUnit.Assertions;
   use SCSC;

   procedure Test_Angle_Difference (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      use type SCSC.Types.Angle;
      D1 : Types.Angle := Types.Difference (100.0, 150.0);
      D2 : Types.Angle := Types.Difference (0.0, 123.5);
      D3 : Types.Angle := Types.Difference (350.0, 10.3);
   begin
      Assert (D1 = 50.0, "Invalid result:" & D1'Image);
      Assert (D2 = 123.5, "Invalid result:" & D2'Image);
      Assert (D3 = 20.3, "Invalid result:" & D3'Image);
   end Test_Angle_Difference;

   procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Angle_Difference'Access, "Angle difference");
   end Register_Tests;

   function Name (T : Test_Case) return Message_String is
      (Format ("SCSC tests"));

end Test_Cases;

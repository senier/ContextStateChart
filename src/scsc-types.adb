with Ada.Text_IO;
with Ada.Strings.Fixed;

package body SCSC.Types
   with SPARK_Mode => Off
is

   function Image (L : Length) return String
   is
      package FIO is new Ada.Text_IO.Float_IO (Float);
      Buffer : String (1..50);
      use Ada.Strings;
      use Ada.Strings.Fixed;
   begin
      case L.Unit is
         when Px =>
            return L.Px'Image;
         when Em =>
            FIO.Put (To => Buffer, Item => L.Em, Aft => 3, Exp => 0); 
            return Trim (Buffer, Both) & "em";
         when others =>
            return "INVALID";
      end case;
   end Image;

end SCSC.Types;

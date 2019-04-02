package body SCSC.Text is

   function Estimate_Width (Data : String;
                            Size : Natural) return Natural is
      Result : Natural := 0;
   begin
      for C of Data
      loop
         Result := Result + (if    C >= 'a' and C <= 'z' then 65
                             elsif C >= 'A' and C <= 'Z' then 75
                             else                             55) * Size / 100;
      end loop;
      return Result;
   end Estimate_Width;

end SCSC.Text;

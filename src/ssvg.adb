with SXML.Generator;
with SXML.Serialize;

package body SSVG is

   XML_String : String (1 .. 100000);

   ---------
   -- SVG --
   ---------

   function SVG return Document_Type
   is
      use SXML.Generator;
   begin
      return E ("svg", A ("xmlns", "http://www.w3.org/2000/svg")
                     + A ("xmlns:xlink", "http://www.w3.org/1999/xlink"));
   end SVG;

   ---------------
   -- To_String --
   ---------------

   function To_String (Document : Document_Type) return String
   is
      use SXML;
      use SXML.Serialize;

      Result : Result_Type;
      Last   : Natural;
   begin
      To_String (Document => Document,
                 Data     => XML_String,
                 Last     => Last,
                 Result   => Result);

      if Result = Result_OK
      then
         return XML_String (1 .. Last);
      else
         return "#INVALID#";
      end if;
   end To_String;

end SSVG;

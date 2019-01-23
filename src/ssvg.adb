with SXML.Generator;
with SXML.Serialize;

package body SSVG is

   XML_String : String (1 .. 100000);

   ---------
   -- SVG --
   ---------

   function SVG (Child : Element_Type := Null_Element) return Document_Type
   is
      use SXML.Generator;
   begin
      return Document_Type
         (E ("svg",
          A ("xmlns", "http://www.w3.org/2000/svg") + A ("xmlns:xlink", "http://www.w3.org/1999/xlink"),
          SXML.Document_Type (Child)));
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
      SXML.Serialize.To_String
         (Document => SXML.Document_Type (Document),
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

   ---------
   -- Arc --
   ---------

   function Arc return Element_Type
   is
      use SXML.Generator;
   begin
      return Element_Type (E ("path", A ("d", "M50,50 A30,50 0 0,1 100,100")
                                    + A ("style", "stroke:#660000; fill:none;")));
   end Arc;

end SSVG;

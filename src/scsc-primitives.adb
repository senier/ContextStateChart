with SXML.Generator;

package body SCSC.Primitives
is
   ---------
   -- Arc --
   ---------

   function Arc return SCSC.SVG.Element_Type
   is
      use SXML.Generator;
   begin
      return SCSC.SVG.Element_Type (E ("path", A ("d", "M50,50 A30,50 0 0,1 100,100")
                                             + A ("style", "stroke:#660000; fill:none;")));
   end Arc;

end SCSC.Primitives;

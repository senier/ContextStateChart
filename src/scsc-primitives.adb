with SXML.Generator;

package body SCSC.Primitives
is
   ---------
   -- Arc --
   ---------

   function Arc return SCSC.SVG.Element_Type
   is
      use SCSC.SVG;
      use SXML.Generator;
   begin
      return To_Element (((M_oveto, False, 50, 50),
                          (A_rc, False, 30, 50, 0, False, True, 100, 100)), Style => "stroke:#660000; fill:none;");
   end Arc;

end SCSC.Primitives;

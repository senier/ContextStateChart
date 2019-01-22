with SXML;

package SSVG
is

   subtype Document_Type is SXML.Document_Type;

   function SVG return Document_Type;
   --  SVG document

   function To_String (Document : Document_Type) return String;
   --  Serialize SVG document

end SSVG;

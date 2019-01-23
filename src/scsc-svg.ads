with SXML;

package SCSC.SVG
is

   type Document_Type is new SXML.Document_Type;
   type Element_Type is new SXML.Document_Type;

   Null_Element : constant Element_Type;

   function SVG (Child : Element_Type := Null_Element) return Document_Type;
   --  SVG document

   function To_String (Document : Document_Type) return String;
   --  Serialize SVG document

private
   Null_Element : constant Element_Type := Element_Type (SXML.Null_Document);
end SCSC.SVG;

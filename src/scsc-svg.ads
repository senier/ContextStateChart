with SXML;
with SCSC.Types;

package SCSC.SVG
is

   type Document_Type is new SXML.Document_Type;
   type Element_Type is new SXML.Document_Type;
   Null_Element : constant Element_Type;

   type Path_Command_Kind is
      (Moveto,
       Lineto,
       Horizontal,
       Vertical,
       Curveto,
       Smooth,
       Quadratic,
       TShorthand,
       Arc,
       ZClosepath,
       Invalid);

   type Mode_Type is (Absolute, Relative);

   type Path_Command_Type (Command  : Path_Command_Kind := Invalid;
                           Mode     : Mode_Type         := Absolute)
   is
   record
      case Command is
         when Moveto | Lineto | TShorthand =>
            X          : Natural;
            Y          : Natural;
         when Horizontal =>
            H_X        : Natural;
         when Vertical =>
            V_Y        : Natural;
         when Quadratic =>
            Q_X1       : Natural;
            Q_Y1       : Natural;
            Q_X        : Natural;
            Q_Y        : Natural;
         when Curveto =>
            C_X1       : Natural;
            C_Y1       : Natural;
            C_X2       : Natural;
            C_Y2       : Natural;
            C_X        : Natural;
            C_Y        : Natural;
         when Smooth =>
            S_X2       : Natural;
            S_Y2       : Natural;
            S_X        : Natural;
            S_Y        : Natural;
         when Arc =>
            RX         : Natural;
            RY         : Natural;
            X_Rotation : Natural;
            Large      : Boolean;
            Sweep      : Boolean;
            AX         : Natural;
            AY         : Natural;
         when ZClosepath | Invalid =>
            null;
      end case;
   end record;

   type Path_Commands_Type is array (Natural range <>) of Path_Command_Type;


   function SVG (Width  : Natural;
                 Height : Natural;
                 Child  : Element_Type := Null_Element) return Document_Type;
   --  SVG document

   function To_Element (Commands : Path_Commands_Type;
                        Style    : String) return Element_Type;
   --  Create element from path commands

   function To_String (Document : Document_Type) return String;
   --  Serialize SVG document

   function Group (Element : Element_Type) return Element_Type;
   --  Group elements

   function "+" (Left, Right : Element_Type) return Element_Type;
   --  Join elements

   function Circle (Center : Types.Point;
                    Radius : Natural;
                    Style  : String := ";") return Element_Type;
   --  Circle

private
   Null_Element : constant Element_Type := Element_Type (SXML.Null_Document);
end SCSC.SVG;

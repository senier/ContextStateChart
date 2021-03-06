with SXML.Serialize;
with SCSC.Types;

package SCSC.SVG
   with SPARK_Mode => On
is

   subtype Document_Type is SXML.Document_Type;

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
            X          : Integer;
            Y          : Integer;
         when Horizontal =>
            H_X        : Integer;
         when Vertical =>
            V_Y        : Integer;
         when Quadratic =>
            Q_X1       : Integer;
            Q_Y1       : Integer;
            Q_X        : Integer;
            Q_Y        : Integer;
         when Curveto =>
            C_X1       : Integer;
            C_Y1       : Integer;
            C_X2       : Integer;
            C_Y2       : Integer;
            C_X        : Integer;
            C_Y        : Integer;
         when Smooth =>
            S_X2       : Integer;
            S_Y2       : Integer;
            S_X        : Integer;
            S_Y        : Integer;
         when Arc =>
            RX         : Integer;
            RY         : Integer;
            X_Rotation : Integer;
            Large      : Boolean;
            Sweep      : Boolean;
            AX         : Integer;
            AY         : Integer;
         when ZClosepath | Invalid =>
            null;
      end case;
   end record;

   type Path_Commands_Type is array (Natural range <>) of Path_Command_Type;

   function Create_SVG (Width  : Natural;
                        Height : Natural;
                        Child  : SXML.Document_Base_Type := SXML.Null_Document;
                        Defs   : SXML.Document_Base_Type := SXML.Null_Document;
                        Style  : String := "") return SXML.Document_Type;
   --  SVG document

   function To_String (Document : SXML.Document_Type) return String;
   --  Serialize SVG document

   use type SXML.Index_Type;

   procedure To_String (Document :        SXML.Document_Type;
                        Output   :    out String;
                        Last     :    out Integer;
                        Stack    : in out SXML.Serialize.Stack_Type) with
      Pre => Document'First > 0 and Document'Length > 0;
   --  Serialize SVG document

   function Path (Commands : Path_Commands_Type;
                  ID       : String := "";
                  Class    : String := "") return SXML.Document_Type;
   --  Create element from path commands

   function Group (Element : SXML.Document_Type;
                   ID      : String := "") return SXML.Document_Type;
   --  Group elements

   function Circle (Center : Types.Point;
                    Radius : Natural;
                    ID     : String := "";
                    Class  : String := "") return SXML.Document_Type;
   --  Circle

   type Align_Type is (Align_Start, Align_Centered, Align_End);

   function Text (Position  : Types.Point;
                  Data      : String;
                  Align     : Align_Type := Align_Centered;
                  DX        : Types.Length := Types.Invalid_Length;
                  DY        : Types.Length := Types.Invalid_Length;
                  Path_Name : String := "";
                  ID        : String := "";
                  Class     : String := "") return SXML.Document_Type;
   --  Text

   function Marker (Element : SXML.Document_Type;
                    Width   : Natural;
                    Height  : Natural;
                    RefX    : Float;
                    RefY    : Float;
                    ID      : String) return SXML.Document_Type;
   --  Return marker

end SCSC.SVG;

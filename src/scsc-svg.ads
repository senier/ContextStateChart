with SXML;

package SCSC.SVG
is

   type Document_Type is new SXML.Document_Type;
   type Element_Type is new SXML.Document_Type;

   Null_Element : constant Element_Type;

   type Path_Command_Kind is
      (M_oveto,
       L_ineto,
       H_orizontal,
       V_ertical,
       C_urveto,
       S_mooth,
       Q_uadratic,
       T_Shorthand,
       A_rc,
       Z_Closepath,
       Invalid);

   type Path_Command_Type (Command  : Path_Command_Kind := Invalid;
                           Relative : Boolean           := False)
   is
   record
      case Command is
         when M_oveto | L_ineto | T_Shorthand =>
            X          : Natural;
            Y          : Natural;
         when H_orizontal =>
            H_X        : Natural;
         when V_ertical =>
            V_Y        : Natural;
         when Q_uadratic =>
            Q_X1       : Natural;
            Q_Y1       : Natural;
            Q_X        : Natural;
            Q_Y        : Natural;
         when C_urveto =>
            C_X1       : Natural;
            C_Y1       : Natural;
            C_X2       : Natural;
            C_Y2       : Natural;
            C_X        : Natural;
            C_Y        : Natural;
         when S_mooth =>
            S_X2       : Natural;
            S_Y2       : Natural;
            S_X        : Natural;
            S_Y        : Natural;
         when A_rc =>
            RX         : Natural;
            RY         : Natural;
            X_Rotation : Natural;
            Large      : Boolean;
            Sweep      : Boolean;
            AX         : Natural;
            AY         : Natural;
         when Z_Closepath | Invalid =>
            null;
      end case;
   end record;

   type Path_Commands_Type is array (Natural range <>) of Path_Command_Type;


   function SVG (Child : Element_Type := Null_Element) return Document_Type;
   --  SVG document

   function To_Element (Commands : Path_Commands_Type;
                        Style    : String) return Element_Type;
   --  Create element from path commands

   function To_String (Document : Document_Type) return String;
   --  Serialize SVG document

private
   Null_Element : constant Element_Type := Element_Type (SXML.Null_Document);
end SCSC.SVG;

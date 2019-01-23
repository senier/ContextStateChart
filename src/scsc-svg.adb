with SXML.Generator;
with SXML.Serialize;

package body SCSC.SVG is

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

   ---------------
   -- To_String --
   ---------------

   function To_String (C : Path_Command_Type) return String
   is
      function Img (N : Natural) return String
      is
      begin
         return N'Image (N'Image'First + 1 .. N'Image'Last);
      end Img;
   begin
      case C.Command is
         when M_oveto =>
            return (if C.Relative then "m" else "M")
               & Img (C.X) & "," & Img (C.Y);
         when L_ineto =>
            return (if C.Relative then "l" else "L")
               & Img (C.X) & "," & Img (C.Y);
         when T_Shorthand =>
            return (if C.Relative then "t" else "T")
               & Img (C.X) & "," & Img (C.Y);
         when H_orizontal =>
            return (if C.Relative then "h" else "H")
               & Img (C.H_X);
         when V_ertical =>
            return (if C.Relative then "v" else "V")
               & Img (C.V_Y);
         when C_urveto =>
            return (if C.Relative then "c" else "C")
               & Img (C.C_X1) & "," & Img (C.C_Y1) & " "
               & Img (C.C_X2) & "," & Img (C.C_Y2) & " "
               & Img (C.C_X) & "," & Img (C.C_Y);
         when S_mooth =>
            return (if C.Relative then "s" else "S")
               & Img (C.S_X2) & "," & Img (C.S_Y2) & " "
               & Img (C.S_X) & "," & Img (C.S_Y);
         when Q_uadratic =>
            return (if C.Relative then "q" else "Q")
               & Img (C.Q_X1) & "," & Img (C.Q_Y1) & " "
               & Img (C.Q_X) & "," & Img (C.Q_Y);
         when A_rc =>
            return (if C.Relative then "a" else "A")
               & Img (C.RX) & "," & Img (C.RY) & " "
               & Img (C.X_Rotation) & " "
               & (if C.Large then "1" else "0") & " "
               & (if C.Sweep then "1" else "0") & " "
               & Img (C.AX) & "," & Img (C.AY);
         when Z_Closepath =>
            return (if C.Relative then "z" else "Z");
         when Invalid =>
            return "INVALID";
      end case;
   end To_String;

   ----------------
   -- To_Element --
   ----------------

   function To_Element (Commands : Path_Commands_Type;
                        Style    : String) return Element_Type
   is
      use SXML.Generator;
      Length : Natural := 0;
   begin
      for Command of Commands
      loop
         Length := Length + To_String (Command)'Length + 1;
      end loop;

      declare
         D : String (1 .. Length);
         L : Natural := 1;
      begin
         for Command of Commands
         loop
            declare
               C : String := To_String (Command);
            begin
               D (L .. L + C'Length - 1) := C;
               D (L + C'Length) := ' ';
               L := L + C'Length + 1;
            end;
         end loop;
         return SCSC.SVG.Element_Type (E ("path", A ("d", D)
                                                + A ("style", Style)));
      end;
   end To_Element;

end SCSC.SVG;

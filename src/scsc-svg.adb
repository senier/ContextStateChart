with SXML.Generator;
with SXML.Serialize;

package body SCSC.SVG is

   XML_String : String (1 .. 100000);

   ---------
   -- SVG --
   ---------

   function SVG (Width  : Natural;
                 Height : Natural;
                 Child  : Element_Type := Null_Element) return Document_Type
   is
      use SXML.Generator;
   begin
      return Document_Type
         (E ("svg",
             A ("width", Width) +
             A ("height", Height) +
             A ("xmlns", "http://www.w3.org/2000/svg") +
             A ("xmlns:xlink", "http://www.w3.org/1999/xlink"),
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
         when Moveto =>
            return (case C.Mode is when Relative => "m", when Absolute => "M")
               & Img (C.X) & "," & Img (C.Y);
         when Lineto =>
            return (case C.Mode is when Relative => "l", when Absolute => "L")
               & Img (C.X) & "," & Img (C.Y);
         when TShorthand =>
            return (case C.Mode is when Relative => "t", when Absolute => "T")
               & Img (C.X) & "," & Img (C.Y);
         when Horizontal =>
            return (case C.Mode is when Relative => "h", when Absolute => "H")
               & Img (C.H_X);
         when Vertical =>
            return (case C.Mode is when Relative => "v", when Absolute => "V")
               & Img (C.V_Y);
         when Curveto =>
            return (case C.Mode is when Relative => "c", when Absolute => "C")
               & Img (C.C_X1) & "," & Img (C.C_Y1) & " "
               & Img (C.C_X2) & "," & Img (C.C_Y2) & " "
               & Img (C.C_X) & "," & Img (C.C_Y);
         when Smooth =>
            return (case C.Mode is when Relative => "s", when Absolute => "S")
               & Img (C.S_X2) & "," & Img (C.S_Y2) & " "
               & Img (C.S_X) & "," & Img (C.S_Y);
         when Quadratic =>
            return (case C.Mode is when Relative => "q", when Absolute => "Q")
               & Img (C.Q_X1) & "," & Img (C.Q_Y1) & " "
               & Img (C.Q_X) & "," & Img (C.Q_Y);
         when Arc =>
            return (case C.Mode is when Relative => "a", when Absolute => "A")
               & Img (C.RX) & "," & Img (C.RY) & " "
               & Img (C.X_Rotation) & " "
               & (if C.Large then "1" else "0") & " "
               & (if C.Sweep then "1" else "0") & " "
               & Img (C.AX) & "," & Img (C.AY);
         when ZClosepath =>
            return (case C.Mode is when Relative => "z", when Absolute => "Z");
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

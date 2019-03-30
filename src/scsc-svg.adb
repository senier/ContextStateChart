with SXML.Generator;

package body SCSC.SVG
   with SPARK_Mode => On
is

   use SXML.Generator;

   ----------------
   -- Create_SVG --
   ----------------

   function Create_SVG (Width  : Natural;
                        Height : Natural;
                        Child  : SXML.Document_Base_Type := SXML.Null_Document;
                        Defs   : SXML.Document_Base_Type := SXML.Null_Document;
                        Style  : String := "") return SXML.Document_Type
   is
      D : constant SXML.Document_Type := E ("defs", Defs);
      S : constant SXML.Document_Base_Type :=
         E ("style",
            A ("type", "test/css"),
            C (Style & " .connector_connector_arc { visiblity: hidden !important; } "
                     & " .annular_sector_arc { visibility: hidden !important; } "
                     & " .text { stroke: none !important; } "
                     & " .scsc_arc { fill: none !important; } "));
      use type SXML.Document_Base_Type;
   begin
      return
         (E ("svg",
             A ("width", Width) +
             A ("height", Height) +
             A ("xmlns", "http://www.w3.org/2000/svg") +
             A ("xmlns:xlink", "http://www.w3.org/1999/xlink"),
             (if Child /= SXML.Null_Document or Defs /= SXML.Null_Document or Style'Length > 0
              then S + D + SXML.Document_Type (Child)
              else SXML.Null_Document)));
   end Create_SVG;

   ---------------
   -- To_String --
   ---------------

   function To_String (Document : SXML.Document_Type) return String
   is
      use SXML;

      Result : Result_Type;
      Last   : Natural;
      Temp   : String (1 .. 100000);
   begin
      SXML.Serialize.To_String
         (Document => Document,
          Data     => Temp,
          Last     => Last,
          Result   => Result);

      if Result = Result_OK
      then
         return Temp (1 .. Last);
      else
         return "#INVALID#: " & Result'Img;
      end if;
   end To_String;

   procedure To_String (Document :        SXML.Document_Type;
                        Output   :    out String;
                        Last     :    out Integer;
                        Stack    : in out SXML.Serialize.Stack_Type)
   is
      use SXML;
      Result : Result_Type;
   begin
      SXML.Serialize.To_String
         (Document => Document,
          Data     => Output,
          Last     => Last,
          Result   => Result,
          Buffer   => Stack);

      if Result /= Result_OK
      then
         Last := -1;
      end if;
   end To_String;

   ---------------
   -- To_String --
   ---------------

   function To_String (C : Path_Command_Type) return String;

   function To_String (C : Path_Command_Type) return String
   is
      function Img (N : Integer) return String;

      function Img (N : Integer) return String
      is
      begin
         return (if N >= 0
                 then N'Image (N'Image'First + 1 .. N'Image'Last)
                 else N'Image);
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

   ----------
   -- Path --
   ----------

   function Path (Commands : Path_Commands_Type;
                  ID       : String := "";
                  Class    : String := "") return SXML.Document_Type
   is
      function Total_Len return Natural;

      function Total_Len return Natural
      is
         Result : Natural := 0;
      begin
         for Command of Commands
         loop
            Result := Result + To_String (Command)'Length + 1;
         end loop;
         return Result;
      end Total_Len;

      Length : constant Natural := Total_Len;
   begin

      declare
         D : String (1 .. Length);
         L : Natural := 1;
      begin
         for Command of Commands
         loop
            declare
               C : constant String := To_String (Command);
            begin
               D (L .. L + C'Length - 1) := C;
               D (L + C'Length) := ' ';
               L := L + C'Length + 1;
            end;
         end loop;
         return
            (E ("path", A ("d", D)

                        + (if Class'Length > 0
                           then A ("class", Class)
                           else Null_Attributes)

                        + (if ID /= ""
                           then A ("id", ID)
                           else Null_Attributes)));
      end;
   end Path;

   -----------
   -- Group --
   -----------

   function Group (Element : SXML.Document_Type;
                   ID      : String := "") return SXML.Document_Type
   is
   begin
      return E ("g", (if ID /= "" then A ("id", ID) else Null_Attributes), Element);
   end Group;

   ------------
   -- Circle --
   ------------

   function Circle (Center : Types.Point;
                    Radius : Natural;
                    ID     : String := "";
                    Class  : String := "") return SXML.Document_Type
   is
   begin
      return
         (E ("circle",
             (if ID /= "" then A ("id", ID) else Null_Attributes) +
             A ("class", (if Class /= "" then "circle " & Class else "circle")) +
             A ("cx", Center.X) +
             A ("cy", Center.Y) +
             A ("r", Radius)));
   end Circle;

   ----------
   -- Text --
   ----------

   function Text (Position  : Types.Point;
                  Data      : String;
                  Align     : Align_Type := Align_Centered;
                  DX        : Types.Length := Types.Invalid_Length;
                  DY        : Types.Length := Types.Invalid_Length;
                  Path_Name : String := "";
                  ID        : String := "";
                  Class     : String := "") return SXML.Document_Type
   is
      use type Types.Length;

      Offset : constant Attributes_Type := A ("startOffset",
                                               (case Align is
                                                   when Align_Start    => "0%",
                                                   when Align_Centered => "50%",
                                                   when Align_End      => "100%"));

      S : constant String := "text-anchor: " &
         (case Align is
             when Align_Start    => "start",
             when Align_Centered => "middle",
             when Align_End      => "end");

      T : constant SXML.Document_Type := (if Path_Name /= ""
                                          then E ("textPath",
                                                  A ("xlink:href", "#" & Path_Name) + Offset + A ("style", S),
                                                  C (Data))
                                          else C (Data));
   begin
      return
         (E ("text", (if ID /= "" then A ("id", ID) else Null_Attributes)
                   + A ("class", (if Class = "" then "text" else "text " & Class))
                   + (if Path_Name = "" then A ("x", Position.X) else Null_Attributes)
                   + (if Path_Name = "" then A ("y", Position.Y) else Null_Attributes)
                   + (if DX /= Types.Invalid_Length then A ("dx", DX.Image) else Null_Attributes)
                   + (if DY /= Types.Invalid_Length then A ("dy", DY.Image) else Null_Attributes),
             T));
   end Text;

   ------------
   -- Marker --
   ------------

   function Marker (Element : SXML.Document_Type;
                    Width   : Natural;
                    Height  : Natural;
                    RefX    : Float;
                    RefY    : Float;
                    ID      : String) return SXML.Document_Type
   is
   begin
      return
         (E ("marker",
             A ("id", ID) +
             A ("orient", "auto") +
             A ("markerWidth", Width) +
             A ("markerHeight", Height) +
             A ("refX", RefX) +
             A ("refY", RefY),
             Element
         ));
   end Marker;

end SCSC.SVG;

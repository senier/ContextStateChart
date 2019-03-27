with Ada.Containers.Generic_Array_Sort;
with SCSC.Math;

package body SCSC.Graph is

   ---------------
   -- Get_Label --
   ---------------

   function Get_Label (Edge : Edge_Type) return String is (Edge.Label_Text (1 .. Edge.Label_Len));

   --------------
   -- Get_Dest --
   --------------

   function Get_Dest (Edge : Edge_Type) return Natural is (Edge.Dest);

   -------------------
   -- Get_Dest_Port --
   -------------------

   function Get_Dest_Port (Edge : Edge_Type) return Port_Type is (Edge.Dest_Port);

   -------------------
   -- Set_Dest_Port --
   -------------------

   procedure Set_Dest_Port (Data       : in out Data_Type;
                            Node_Index :        Natural;
                            Edge_Index :        Natural;
                            Port       :        Port_Type) is
   begin
      Data (Node_Index).Edges_Data (Edge_Index).Dest_Port := Port;
   end Set_Dest_Port;

   ---------------------
   -- Set_Source_Port --
   ---------------------

   procedure Set_Source_Port (Data       : in out Data_Type;
                              Node_Index :        Natural;
                              Edge_Index :        Natural;
                              Port       :        Port_Type) is
   begin
      Data (Node_Index).Edges_Data (Edge_Index).Source_Port := Port;
   end Set_Source_Port;

   -----------------
   -- Create_Node --
   -----------------

   function Create_Node (Weight      : Positive   := 1;
                         Level       : Integer    := 0;
                         Label       : String     := "";
                         Inner_Ports : Positive   := 1;
                         Outer_Ports : Positive   := 1;
                         Edges       : Edges_Type := Null_Edges) return Node_Type
   is
      (Node_Type'(Weight      => Weight,
                  Level       => Level,
                  Ports       => (Primitives.Pos_Inner => Inner_Ports, Primitives.Pos_Outer => Outer_Ports),
                  Label_Text  => Label & (Label_Type'First + Label'Length .. Label_Type'Last => ' '),
                  Label_Len   => Label'Length,
                  Edges_Data  => Edges & Edges_Type'(Edges'Length + 1 .. Edges_Data_Type'Last => Null_Edge),
                  Edges_Len   => Edges'Length));

   ----------------
   -- Get_Weight --
   ----------------

   function Get_Weight (Node : Node_Type) return Positive is (Node.Weight);

   ----------------
   -- Set_Weight --
   ----------------

   procedure Set_Weight (Node   : in out Node_Type;
                         Weight :        Positive) is
   begin
      Node.Weight := Weight;
   end Set_Weight;

   ---------------
   -- Get_Label --
   ---------------

   function Get_Label (Node : Node_Type) return String is (Node.Label_Text (1 .. Node.Label_Len));

   ---------------
   -- Get_Edges --
   ---------------

   function Get_Edges (Node : Node_Type) return Edges_Type is (Node.Edges_Data (1 .. Node.Edges_Len));

   ---------------
   -- Get_Ports --
   ---------------

   function Get_Ports (Node : Node_Type) return Ports_Type is (Node.Ports);

   ---------------
   -- Set_Ports --
   ---------------

   procedure Set_Ports (Node  : in out Node_Type;
                        Ports :        Ports_Type) is
   begin
      Node.Ports := Ports;
   end Set_Ports;

   ------------------
   -- Create_Polar --
   ------------------

   function Create_Polar
     (Center  : Types.Point;
      Radius  : Natural;
      Spacing : Spacing_Type;
      Padding : Natural := 0) return Graph_Params_Type
   is
      Result : Graph_Params_Type := (Center         => Center,
                                     Radius         => Radius,
                                     Spacing        => (others => 0),
                                     Spacing_Length => Spacing'Length,
                                     Padding        => Padding);
   begin
      Result.Spacing (Spacing'Range) := Spacing;
      return Result;
   end Create_Polar;

   ----------------
   -- Get_Center --
   ----------------

   function Get_Center (Params : Graph_Params_Type) return Types.Point is (Params.Center);

   -----------------
   -- Get_Spacing --
   -----------------

   function Get_Spacing (Params : Graph_Params_Type) return Spacing_Type is
      (Params.Spacing (Params.Spacing'First .. Params.Spacing'First + Spacing_Index'Val (Params.Spacing_Length) - 1));

   ----------------
   -- Get_Radius --
   ----------------

   function Get_Radius (Params : Graph_Params_Type) return Natural is (Params.Radius);

   ------------------
   -- Create_Graph --
   ------------------

   function Create_Graph
     (Params          : Graph_Params_Type;
      Data            : Data_Type;
      Positions       : Positions_Type := (1 .. 0 => 1);
      GID             : String := "";
      Style           : String := "";
      Connector_Style : String := "";
      Text_Style      : String := "") return SXML.Document_Type
   is
      use type SXML.Offset_Type;

      type Level_Type (Valid : Boolean := False) is
      record
         case Valid is
            when False => null;
            when True  => Value : Integer;
         end case;
      end record;

      function "<" (Left, Right : Level_Type) return Boolean is (Left.Value < Right.Value);

      type Levels_Type is array (Natural range <>) of Level_Type;
      type Annular_Sectors_Type is array (Data'Range) of Primitives.Annular_Sector_Params_Type;

      type Params_Type is
      record
         Length  : SXML.Offset_Type;
         Sectors : Annular_Sectors_Type;
      end record;

      ------------------------------------------------------------------------

      function Get_Levels return Levels_Type;

      function Get_Levels return Levels_Type
      is
         Levels : Levels_Type (0 .. 100) := (others => (Valid => False));
         Last   : Natural;
         procedure Sort is new Ada.Containers.Generic_Array_Sort (Natural, Level_Type, Levels_Type);
      begin
         for D of Data
         loop
            for L in Levels'Range
            loop
               if not Levels (L).Valid
               then
                  Levels (L) := (Valid => True, Value => D.Level);
                  Last := L;
                  exit;
               elsif Levels (L).Value = D.Level
               then
                  exit;
               end if;
            end loop;
         end loop;
         Sort (Levels (Levels'First .. Last));
         return Levels (Levels'First .. Last);
      end Get_Levels;

      ------------------------------------------------------------------------

      function Create_Connector (P      : Params_Type;
                                 Edge   : Edge_Type;
                                 Offset : Positive;
                                 CID    : String) return SXML.Document_Type;

      function Create_Connector (P      : Params_Type;
                                 Edge   : Edge_Type;
                                 Offset : Positive;
                                 CID    : String) return SXML.Document_Type
      is
         Source_Port : constant Port_Type := Edge.Source_Port;
         Dest_Port   : constant Port_Type := Edge.Dest_Port;
      begin
         return Primitives.Connector
            (Center     => Params.Center,
             Start      => Types.P (Primitives.Port (Params    => P.Sectors (Offset),
                                                     Position  => Source_Port.Pos,
                                                     Port_No   => Source_Port.Num,
                                                     Num_Ports => Data (Offset).Ports (Source_Port.Pos))),
             Stop       => Types.P (Primitives.Port (Params    => P.Sectors (Edge.Dest),
                                                     Position  => Dest_Port.Pos,
                                                     Port_No   => Dest_Port.Num,
                                                     Num_Ports => Data (Edge.Dest).Ports (Dest_Port.Pos))),
             Radius     => Edge.Radius,
             COID       => CID,
             Text       => Edge.Get_Label,
             Marker_End => "End_Arrow",   -- FIXME: Use global CSS instead
             Textstyle  => Text_Style,
             Direction  => Edge.Dir,
             Style      => Connector_Style);
      end Create_Connector;

      ------------------------------------------------------------------------

      function Nodes_Per_Level (Level : Integer) return Natural;

      function Nodes_Per_Level (Level : Integer) return Natural
      is
         Nodes : Natural := 0;
      begin
         for D of Data
         loop
            if D.Level = Level
            then
               Nodes := Nodes + 1;
            end if;
         end loop;
         return Nodes;
      end Nodes_Per_Level;

      ------------------------------------------------------------------------

      function Calculate_Offset (Spacing : Spacing_Type;
                                 Level   : Natural) return Integer;

      function Calculate_Offset (Spacing : Spacing_Type;
                                 Level   : Natural) return Integer is
         Result : Integer := 0;
      begin
         for I in Natural (Spacing'First) .. Level loop
            Result := Result + (if I in Natural (Spacing'First) .. Natural (Spacing'Last)
                                then Integer (Spacing (Spacing_Index (I)))
                                else 20);
         end loop;

         return Result;

      end Calculate_Offset;

      ------------------------------------------------------------------------

      function Calculate_Params return Params_Type;

      function Calculate_Params return Params_Type
      is
         I       : Positive;
         Start   : Types.Angle := 0.0;
         Weights : Natural;
         Result  : Params_Type;
         Levels  : constant Levels_Type := Get_Levels;
      begin
         Result.Length := 0;

         for L in Levels'Range
         loop
            Weights := 0;
            for D of Data
            loop
               if D.Level = Levels (L).Value
               then
                  Weights := Weights + D.Weight;
               end if;
            end loop;

            for Index in Data'Range
            loop
               I := (if Positions'Length > 0 then Positions (Index) else Index);
               if Data (I).Level = Levels (L).Value
               then
                  declare
                     use type Types.Angle;
                     use Primitives;
                     Offset  : constant Natural := Calculate_Offset (Params.Spacing, L);
                     Spacing : constant Types.Angle := Types.Angle
                        (Math.Arcsin (Float (Params.Padding) / Float (Offset + Params.Radius / 2),
                                      Cycle => 360.0));
                     Circle  : constant Float := 360.0 - (Float (Nodes_Per_Level (Levels (L).Value)) * Float (Spacing));
                     Size    : constant Types.Angle := Types.Angle (Circle * Float (Data (I).Weight) / Float (Weights));
                     Stop    : constant Types.Angle := Start + Size;
                     AP      : constant Annular_Sector_Params_Type := Polar (Center => Params.Center,
                                                                             Offset => Offset,
                                                                             Radius => Params.Radius,
                                                                             Start  => Start,
                                                                             Stop   => Start + Size);
                     AS      : constant SXML.Document_Type := Annular_Sector (Params    => AP,
                                                                            Text      => Data (I).Get_Label,
                                                                            Style     => Style,
                                                                            Textstyle => Text_Style,
                                                                            ASID      => GID & "_AS_" & To_ID (I));
                  begin
                     Result.Sectors (I) := AP;
                     Result.Length := Result.Length + AS'Length;
                     Start := Stop + Spacing;
                  end;
               end if;
            end loop;
         end loop;

         for Index in Data'Range
         loop
            I := (if Positions'Length > 0 then Positions (Index) else Index);
            for E of Data (I).Get_Edges
            loop
               declare
                  Connector_ID : constant String := GID & "_C_" & To_ID (I) & "_" & To_ID (E.Dest);
                  C : constant SXML.Document_Type := Create_Connector (Result, E, I, Connector_ID);
               begin
                  Result.Length := Result.Length + C'Length;
               end;
            end loop;
         end loop;

         return Result;
      end Calculate_Params;

      P       : constant Params_Type := Calculate_Params;
      Sectors : SXML.Document_Type (1 .. SXML.Index_Type (P.Length)) := (others => SXML.Null_Node);
      Offset  : SXML.Offset_Type := 0;
      I       : Positive;
   begin
      for Index in Data'Range
      loop
         I := (if Positions'Length > 0 then Positions (Index) else Index);
         declare
            Sector : constant SXML.Document_Type :=
               Primitives.Annular_Sector (Params    => P.Sectors (Index),
                                          Text      => Data (I).Get_Label,
                                          Style     => Style,
                                          Textstyle => Text_Style,
                                          ASID      => GID & "_AS_" & To_ID (I));
         begin
            SXML.Append (Sectors, Offset, Sector);
            Offset := Offset + Sector'Length;
            for J in Data (I).Get_Edges'Range
            loop
               declare
                  E : constant Edge_Type := Data (I).Get_Edges (J);
                  Connector_ID : constant String := GID & "_C_" & To_ID (I) & "_" & To_ID (J);
                  C : constant SXML.Document_Type := Create_Connector (P, E, I, Connector_ID);
               begin
                  SXML.Append (SXML.Document_Type (Sectors), Offset, C);
                  Offset := Offset + C'Length;
               end;
            end loop;
         end;
      end loop;
      return Sectors;
   end Create_Graph;

   -----------------
   -- Create_Edge --
   -----------------

   function Create_Edge (Dest        : Natural;
                         Dir         : Primitives.Dir_Type;
                         Radius      : Integer;
                         Source_Port : Port_Type;
                         Dest_Port   : Port_Type;
                         Label       : String := "") return Edge_Type
   is
      Label_Length : constant Natural := (if Label'Length > Label_Type'Last - Label_Type'First + 1
                                          then Label_Type'Last - Label_Type'First + 1
                                          else Label'Length);
   begin
      return
         (Dest        => Dest,
          Dir         => Dir,
          Radius      => Radius,
          Source_Port => Source_Port,
          Dest_Port   => Dest_Port,
          Label_Text  => Label (Label'First .. Label'First + Label_Length - 1)
                         & (Label_Type'First + Label_Length .. Label_Type'Last => ' '),
          Label_Len   => Label_Length);
   end Create_Edge;

end SCSC.Graph;

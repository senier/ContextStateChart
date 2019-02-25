with SXML;
with SCSC.Primitives;
with Ada.Containers.Generic_Array_Sort;

package body SCSC.Graph is

   -----------
   -- Label --
   -----------

   function Label (Edge : Edge_Type) return String is (Edge.Label_Text (1 .. Edge.Label_Len));

   ----------
   -- Node --
   ----------

   function Node (Weight      : Positive   := 1;
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

   ------------
   -- Weight --
   ------------

   function Weight (Node : Node_Type) return Positive is (Node.Weight);

   -----------
   -- Label --
   -----------

   function Label (Node : Node_Type) return String is (Node.Label_Text (1 .. Node.Label_Len));

   -----------
   -- Edges --
   -----------

   function Edges (Node : Node_Type) return Edges_Type is (Node.Edges_Data (1 .. Node.Edges_Len));

   -----------
   -- Edges --
   -----------

   function Ports (Node : Node_Type) return Ports_Type is (Node.Ports);

   -----------
   -- Polar --
   -----------

   function Polar
     (Center        : Types.Point;
      Offset        : Natural;
      Radius        : Natural;
      Layer_Spacing : Natural;
      Padding       : Natural := 0) return Graph_Params_Type is
      (Graph_Params_Type'(Center        => Center,
                          Offset        => Offset,
                          Radius        => Radius,
                          Layer_Spacing => Layer_Spacing,
                          Padding       => Padding));

   ------------
   -- Center --
   ------------

   function Center (Params : Graph_Params_Type) return Types.Point is (Params.Center);

   ------------
   -- Offset --
   ------------

   function Offset (Params : Graph_Params_Type) return Natural is (Params.Offset);

   ------------
   -- Radius --
   ------------

   function Radius (Params : Graph_Params_Type) return Natural is (Params.Radius);

   -----------
   -- Graph --
   -----------

   function Graph
     (Params          : Graph_Params_Type;
      Data            : Data_Type;
      Style           : String := "";
      Connector_Style : String := "";
      Text_Style      : String := "") return SVG.Element_Type
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
                                 Offset : Positive) return SVG.Element_Type
      is
         Source_Port : constant Port_Type := Edge.Source_Port;
         Dest_Port   : constant Port_Type := Edge.Dest_Port;
      begin
         return Primitives.Connector
            (Center     => Params.Center,
             Start      => Primitives.Port (Params    => P.Sectors (Offset),
                                            Position  => Source_Port.Pos,
                                            Port_No   => Source_Port.Num,
                                            Num_Ports => Data (Offset).Ports (Source_Port.Pos)),
             Stop       => Primitives.Port (Params    => P.Sectors (Edge.Dest),
                                            Position  => Dest_Port.Pos,
                                            Port_No   => Dest_Port.Num,
                                            Num_Ports => Data (Edge.Dest).Ports (Dest_Port.Pos)),
             Radius     => Edge.Radius,
             Text       => Edge.Label,
             Marker_End => "End_Arrow",   -- FIXME: Use global CSS instead
             Textstyle  => Text_Style,
             Direction  => Edge.Dir,
             Style      => Connector_Style);
      end Create_Connector;

      ------------------------------------------------------------------------

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

      function Calculate_Params return Params_Type
      is
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

            for I in Data'Range
            loop
               if Data (I).Level = Levels (L).Value
               then
                  declare
                     use type Types.Angle;
                     use Primitives;
                     Offset  : constant Natural := Params.Offset + Params.Layer_Spacing * L;
                     Spacing : constant Types.Angle := Types.Angle
                        (Math.Arcsin (Float (Params.Padding) / Float (Offset + Params.Radius / 2),
                                      Cycle => 360.0));
                     Circle  : Float := 360.0 - (Float (Nodes_Per_Level (Levels (L).Value)) * Float (Spacing));
                     Size : Types.Angle := Types.Angle (Circle * Float (Data (I).Weight) / Float (Weights));
                     Stop : Types.Angle := Start + Size;
                     AP   : Annular_Sector_Params_Type := Polar (Center => Params.Center,
                                                                 Offset => Offset,
                                                                 Radius => Params.Radius,
                                                                 Start  => Start,
                                                                 Stop   => Start + Size);
                     AS   : SVG.Element_Type := Annular_Sector (Params    => AP,
                                                                Text      => Data (I).Label,
                                                                Style     => Style,
                                                                Textstyle => Text_Style);
                  begin
                     Result.Sectors (I) := AP;
                     Result.Length := Result.Length + AS'Length;
                     Start := Stop + Spacing;
                  end;
               end if;
            end loop;
         end loop;

         for I in Data'Range
         loop
            for E of Data (I).Edges
            loop
               declare
                  C : SVG.Element_Type := Create_Connector (Result, E, I);
               begin
                  Result.Length := Result.Length + C'Length;
               end;
            end loop;
         end loop;

         return Result;
      end Calculate_Params;

      P       : constant Params_Type := Calculate_Params;
      Sectors : SVG.Element_Type (1 .. SXML.Index_Type (P.Length)) := (others => SXML.Null_Node);
      Offset  : SXML.Offset_Type := 0;
   begin
      for I in Data'Range
      loop
         declare
            use type SXML.Index_Type;
            use type SVG.Element_Type;

            Sector : SVG.Element_Type := Primitives.Annular_Sector (Params    => P.Sectors (I),
                                                                    Text      => Data (I).Label,
                                                                    Style     => Style,
                                                                    Textstyle => Text_Style);
         begin
            SXML.Append (SXML.Document_Type (Sectors), Offset, SXML.Document_Type (Sector));
            Offset := Offset + Sector'Length;
            for E of Data (I).Edges
            loop
               declare
                  C : SVG.Element_Type := Create_Connector (P, E, I);
               begin
                  SXML.Append (SXML.Document_Type (Sectors), Offset, SXML.Document_Type (C));
                  Offset := Offset + C'Length;
               end;
            end loop;
         end;
      end loop;
      return Sectors;
   end Graph;

end SCSC.Graph;

with SXML;
with SCSC.Primitives;

package body SCSC.Graph is

   -----------
   -- Label --
   -----------

   function Label (Edge : Edge_Type) return String is (Edge.Label_Text (1 .. Edge.Label_Len));

   ----------
   -- Node --
   ----------

   function Node (Weight      : Positive   := 1;
                  Label       : String     := "";
                  Inner_Ports : Positive   := 1;
                  Outer_Ports : Positive   := 1;
                  Edges       : Edges_Type := Null_Edges) return Node_Type
   is
      (Node_Type'(Weight      => Weight,
                  Inner_Ports => Inner_Ports,
                  Outer_Ports => Outer_Ports,
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
   -- Polar --
   -----------

   function Polar
     (Center  : Types.Point;
      Offset  : Natural;
      Radius  : Natural;
      Padding : Natural := 0) return Graph_Params_Type is
      (Graph_Params_Type'(Center  => Center,
                          Offset  => Offset,
                          Radius  => Radius,
                          Padding => Padding));

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
     (Params    : Graph_Params_Type;
      Data      : Data_Type;
      Style     : String := "";
      Textstyle : String := "") return SVG.Element_Type
   is
      use type SXML.Offset_Type;

      type Angles_Type is
      record
         Start : Types.Angle;
         Stop  : Types.Angle;
      end record;

      type Angles_Array_Type is array (Data'Range) of Angles_Type;

      type Params_Type is
      record
         Length : SXML.Offset_Type;
         Angles : Angles_Array_Type;
      end record;

      function Calculate_Params return Params_Type
      is
         Start   : Types.Angle := 0.0;
         Weights : Natural     := 0;
         Result  : Params_Type := (Length => 0, Angles => (others => (0.0, 0.0)));
         Spacing : Types.Angle := Types.Angle
            (Math.Arcsin (Float (Params.Padding) / Float (Params.Offset + Params.Radius / 2),
                          Cycle => 360.0));
         Circle  : Float := 360.0 - (Float (Data'Length) * Float (Spacing));
      begin
         for D of Data
         loop
            Weights := Weights + D.Weight;
         end loop;

         for I in Data'Range
         loop
            declare
               use type Types.Angle;
               Size : Types.Angle := Types.Angle (Circle * Float (Data (I).Weight) / Float (Weights));
               Stop : Types.Angle := Start + Size;
               AP   : Primitives.Annular_Sector_Params_Type :=
                  Primitives.Polar (Params.Center, Params.Offset, Params.Radius, Start, Start + Size);
               AS   : SVG.Element_Type := Primitives.Annular_Sector (Params    => AP,
                                                                     Text      => Data (I).Label,
                                                                     Style     => Style,
                                                                     Textstyle => Textstyle);
            begin
               Result.Angles (I) := (Start, Stop);
               Result.Length     := Result.Length + AS'Length;
               Start := Stop + Spacing;
            end;
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

            Parameters : Primitives.Annular_Sector_Params_Type :=
               Primitives.Polar (Center => Params.Center,
                                 Offset => Params.Offset,
                                 Radius => Params.Radius,
                                 Start  => P.Angles (I).Start,
                                 Stop   => P.Angles (I).Stop);
            Sector : SVG.Element_Type := Primitives.Annular_Sector (Params    => Parameters,
                                                                    Text      => Data (I).Label,
                                                                    Style     => Style,
                                                                    Textstyle => Textstyle);
         begin
            SXML.Append (SXML.Document_Type (Sectors), Offset, SXML.Document_Type (Sector));
            Offset := Offset + Sector'Length;
         end;
      end loop;
      return Sectors;
   end Graph;

end SCSC.Graph;

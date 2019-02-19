with SCSC.SVG;
with SCSC.Types;
with SCSC.Math;
with SCSC.Primitives;

package SCSC.Graph
is
   type Graph_Params_Type is private;
   type Node_Type is tagged private;
   type Data_Type is array (Positive range <>) of Node_Type;

   type Port_Type is
   record
      Num : Natural;
      Pos : Primitives.Pos_Type;
   end record;

   type Edge_Type is tagged private;
   Null_Edge : constant Edge_Type;

   function Edge (Dest        : Natural;
                  Dir         : Primitives.Dir_Type;
                  Source_Port : Port_Type;
                  Dest_Port   : Port_Type;
                  Label       : String := "") return Edge_Type;
   --  Create edge

   function Label (Edge : Edge_Type) return String;
   --  Return Label

   type Edges_Type is array (Natural range <>) of Edge_Type;
   Null_Edges : constant Edges_Type;

   function Node (Weight      : Positive   := 1;
                  Label       : String     := "";
                  Inner_Ports : Positive   := 1;
                  Outer_Ports : Positive   := 1;
                  Edges       : Edges_Type := Null_Edges) return Node_Type;
   --  Create node

   function Weight (Node : Node_Type) return Positive;
   --  Return weight of node

   function Label (Node : Node_Type) return String;
   --  Return label

   function Edges (Node : Node_Type) return Edges_Type;
   --  Return edges

   function Polar (Center  : Types.Point;
                   Offset  : Natural;
                   Radius  : Natural;
                   Padding : Natural := 0) return Graph_Params_Type;
   --  Create graph parameters from center point, offset and radius. Ensure
   --  padding (in pixels) between sectors.

   function Center (Params : Graph_Params_Type) return Types.Point;
   --  Return center point from graph parameters

   function Offset (Params : Graph_Params_Type) return Natural;
   --  Return offset from graph parameters

   function Radius (Params : Graph_Params_Type) return Natural;
   --  Return radius from graph parameters

   function Graph (Params    : Graph_Params_Type;
                   Data      : Data_Type;
                   Style     : String := "";
                   Textstyle : String := "") return SVG.Element_Type;
   --  Return graph

private

   subtype Label_Type is String (1 .. 100);
   subtype Edges_Data_Type is Edges_Type (1 .. 50);

   type Node_Type is tagged
   record
      Weight      : Positive;
      Inner_Ports : Positive;
      Outer_Ports : Positive;
      Label_Text  : Label_Type;
      Label_Len   : Natural;
      Edges_Data  : Edges_Data_Type;
      Edges_Len   : Natural;
   end record;

   type Graph_Params_Type is
   record
      Center  : Types.Point;
      Offset  : Natural;
      Radius  : Natural;
      Padding : Natural;
   end record;

   type Edge_Type is tagged
   record
      Dest        : Natural;
      Dir         : Primitives.Dir_Type;
      Source_Port : Port_Type;
      Dest_Port   : Port_Type;
      Label_Text  : Label_Type;
      Label_Len   : Natural;
   end record;

   ----------
   -- Edge --
   ----------

   function Edge (Dest        : Natural;
                  Dir         : Primitives.Dir_Type;
                  Source_Port : Port_Type;
                  Dest_Port   : Port_Type;
                  Label       : String := "") return Edge_Type
   is
      (Dest        => Dest,
       Dir         => Dir,
       Source_Port => Source_Port,
       Dest_Port   => Dest_Port,
       Label_Text  => Label & (Label_Type'First + Label'Length .. Label_Type'Last => ' '),
       Label_Len   => Label'Length);

   Null_Edge  : constant Edge_Type  := Edge (0, Primitives.Dir_Invalid, (0, Primitives.Pos_Invalid), (0, Primitives.Pos_Invalid));
   Null_Edges : constant Edges_Type := (1 .. 0 => Null_Edge);

end SCSC.Graph;

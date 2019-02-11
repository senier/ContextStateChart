with SCSC.SVG;
with SCSC.Types;
with SCSC.Graph_Layout;
with SCSC.Math;

package SCSC.Graph
is
   type Graph_Params_Type is private;
   type Node_Type is tagged private;
   type Data_Type is array (Natural range <>) of Node_Type;

   function Node (Weight : Positive) return Node_Type;
   --  Create node

   function Weight (Node : Node_Type) return Positive;
   --  Return weight of node

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

   function Graph (Params : Graph_Params_Type;
                   Data   : Data_Type;
                   Layout : Graph_Layout.Layout_Type;
                   Style  : String := "") return SVG.Element_Type;
   --  Return graph

private

   type Node_Type is tagged
   record
      Weight : Positive;
   end record;

   type Graph_Params_Type is
   record
      Center  : Types.Point;
      Offset  : Natural;
      Radius  : Natural;
      Padding : Natural;
   end record;

end SCSC.Graph;

with SCSC.Types;
with SCSC.Primitives;
with SXML;

package SCSC.Graph
is
   type Graph_Params_Type is private;
   type Node_Type is tagged private;
   type Data_Type is array (Positive range <>) of Node_Type;

   type Positions_Type is array (Positive range <>) of Positive with
      Predicate => (for all I in Positions_Type'Range =>
                       (for all J in Positions_Type'Range =>
                           (if I /= J then Positions_Type (I) /= Positions_Type (J))));

   type Annular_Sectors_Type is array (Positive range <>) of Primitives.Annular_Sector_Params_Type;

   type Spacing_Index is new Natural range 0 .. 20;
   type Spacing_Type is array (Spacing_Index range <>) of Natural;

   type Port_Type is
   record
      Num : Natural;
      Pos : Primitives.Pos_Type;
   end record;

   type Ports_Type is array (Primitives.Pos_Type range Primitives.Pos_Outer .. Primitives.Pos_Inner) of Natural;

   type Edge_Type is tagged private;
   Null_Edge : constant Edge_Type;

   type Energy_Params_Type is private;

   function Create_Edge (Dest        : Natural;
                         Dir         : Primitives.Dir_Type;
                         Radius      : Integer;
                         Source_Port : Port_Type;
                         Dest_Port   : Port_Type;
                         Label       : String := "") return Edge_Type;
   --  Create edge

   function Get_Label (Edge : Edge_Type) return String;
   --  Return Label

   function Get_Dest (Edge : Edge_Type) return Natural;
   --  Return destination node

   function Get_Dest_Port (Edge : Edge_Type) return Port_Type;
   --  Return destination port

   procedure Set_Dest_Port (Data       : in out Data_Type;
                            Node_Index :        Natural;
                            Edge_Index :        Natural;
                            Port       :        Port_Type);
   --  Set destination port

   procedure Set_Source_Port (Data       : in out Data_Type;
                              Node_Index :        Positive;
                              Edge_Index :        Natural;
                              Port       :        Port_Type);
   --  Set source port

   function Get_Source_Port (Data       : Data_Type;
                             Node_Index : Positive;
                             Edge_Index : Natural) return Port_Type;
   --  Get source port

   type Edges_Type is array (Natural range <>) of Edge_Type;
   Null_Edges : constant Edges_Type;

   function Create_Node (Weight      : Positive   := 1;
                         Level       : Integer    := 0;
                         Label       : String     := "";
                         Inner_Ports : Positive   := 1;
                         Outer_Ports : Positive   := 1;
                         Edges       : Edges_Type := Null_Edges) return Node_Type;
   --  Create node

   function Get_Weight (Node : Node_Type) return Positive;
   --  Return weight of node

   procedure Set_Weight (Node   : in out Node_Type;
                         Weight : Positive);
   --  Set weight of node

   function Get_Label (Node : Node_Type) return String;
   --  Return label

   function Get_Edges (Node : Node_Type) return Edges_Type;
   --  Return edges

   procedure Set_Edge (Data       : in out Data_Type;
                       Node_Index :        Positive;
                       Edge_Index :        Positive;
                       Edge       :        Edge_Type);
   --  Set value of edge

   function Get_Dir (Edge : Edge_Type) return Primitives.Dir_Type;
   --  Get direction of an edge

   procedure Set_Dir (Edge : in out Edge_Type;
                      Dir  :        Primitives.Dir_Type);
   --  Set direction of an edge

   function Get_Ports (Node : Node_Type) return Ports_Type;
   --  Return ports

   procedure Set_Ports (Node  : in out Node_Type;
                        Ports :        Ports_Type);
   --  Set ports

   function Create_Polar (Center    : Types.Point;
                          Radius    : Natural;
                          Spacing   : Spacing_Type;
                          Padding   : Natural  := 0;
                          Font_Size : Positive := 10) return Graph_Params_Type;
   --  Create graph parameters from center point, offset and radius. Different
   --  layers of the graph have Layer_Spacing pixels of spacing. Ensure padding
   --  (in pixels) between sectors.

   function Get_Center (Params : Graph_Params_Type) return Types.Point;
   --  Return center point from graph parameters

   function Get_Spacing (Params : Graph_Params_Type) return Spacing_Type;
   --  Return spacing array from graph parameters

   function Get_Spacing (Params : Graph_Params_Type;
                         Index  : Spacing_Index) return Natural;
   --  Get spacing at given index

   procedure Set_Spacing (Params : in out Graph_Params_Type;
                          Index  :        Spacing_Index;
                          Value  :        Natural);
   --  Set spacing at given index

   function Get_Radius (Params : Graph_Params_Type) return Natural;
   --  Return radius from graph parameters

   function Create_Graph (Params    : Graph_Params_Type;
                          Sectors   : Annular_Sectors_Type;
                          Length    : Natural;
                          Data      : Data_Type;
                          Positions : Positions_Type;
                          ID        : String := "") return SXML.Document_Type with
      Pre => (Sectors'Length = Data'Length and
              (if Positions'Length > 0
               then Data'Length = Positions'Length
                    and (for all P in Positions'Range => P in Data'Range)));
   --  Create graph

   procedure Layout (Params        :     Graph_Params_Type;
                     Data          :     Data_Type;
                     Energy_Params :     Energy_Params_Type;
                     ID            :     String;
                     Positions     :     Positions_Type;
                     Length        : out Natural;
                     Sectors       : out Annular_Sectors_Type;
                     Energy        : out Long_Integer) with
      Pre => Data'Length = Sectors'Length
             and Data'Length = Positions'Length;
   --  Layout graph and calculate energy

   procedure Identity (Positions : out Positions_Type);
   --  Initialize @Positions@ with identity mapping

   function Create_Energy_Params (Factor_Sector_Too_Wide          : Long_Integer :=   10;
                                  Factor_Sector_Too_Narrow        : Long_Integer := 1000;
                                  Factor_Arc_Label_Too_Wide       : Long_Integer := 1000;
                                  Factor_Arc_Label_Too_Narrow     : Long_Integer := 1000;
                                  Factor_Level_Spacing_Too_Wide   : Long_Integer :=   10;
                                  Factor_Level_Spacing_Too_Narrow : Long_Integer :=  500;
                                  Factor_Radius_Spacing           : Long_Integer :=    5;
                                  Text_Border                     : Long_Integer :=   20;
                                  Font_Size                       : Positive     :=   10) return Energy_Params_Type;
   --  Initialize energy calculation parameters

   function Calculate_Energy (Annular_Sector_Params : Primitives.Annular_Sector_Params_Type;
                              Energy_Params         : Energy_Params_Type;
                              Label                 : String) return Long_Integer;

   function Calculate_Energy (Graph_Params  : Graph.Graph_Params_Type;
                              Energy_Params : Energy_Params_Type;
                              Graph_Data    : Graph.Data_Type;
                              Sectors       : Graph.Annular_Sectors_Type;
                              Positions     : Graph.Positions_Type) return Long_Integer;

   function Calculate_Energy (Connector_Params : Primitives.Connector_Params_Type;
                              Energy_Params    : Energy_Params_Type) return Long_Integer;
private

   subtype Label_Type is String (1 .. 100);
   subtype Edges_Data_Type is Edges_Type (1 .. 50);

   type Node_Type is tagged
   record
      Weight      : Positive;
      Level       : Integer;
      Ports       : Ports_Type;
      Label_Text  : Label_Type;
      Label_Len   : Natural;
      Edges_Data  : Edges_Data_Type;
      Edges_Len   : Natural;
   end record;

   type Graph_Params_Type is
   record
      Center         : Types.Point;
      Radius         : Natural;
      Spacing        : Spacing_Type (Spacing_Index);
      Spacing_Length : Natural;
      Padding        : Natural;
      Font_Size      : Positive;
   end record;

   type Edge_Type is tagged
   record
      Dest        : Natural;
      Dir         : Primitives.Dir_Type;
      Source_Port : Port_Type;
      Dest_Port   : Port_Type;
      Label_Text  : Label_Type;
      Label_Len   : Natural;
      Radius      : Integer;
   end record;

   function Create_Connector (Graph_Params : Graph_Params_Type;
                              Graph_Data   : Data_Type;
                              Sectors      : Annular_Sectors_Type;
                              Edge         : Edge_Type;
                              Index        : Positive) return Primitives.Connector_Params_Type;

   Null_Edge  : constant Edge_Type :=
      (Dest        => 0,
       Dir         => Primitives.Dir_Invalid,
       Radius      => 0,
       Source_Port => (0, Primitives.Pos_Invalid),
       Dest_Port   => (0, Primitives.Pos_Invalid),
       Label_Text  => (others => ' '),
       Label_Len   => 0);

   Null_Edges : constant Edges_Type := (1 .. 0 => Null_Edge);

   type Level_Type (Valid : Boolean := False) is
   record
      case Valid is
         when False => null;
         when True  => Value : Integer;
      end case;
   end record;

   function "<" (Left, Right : Level_Type) return Boolean is (Left.Value < Right.Value);

   type Levels_Type is array (Natural range <>) of Level_Type;

   function Get_Levels (Data : Data_Type) return Levels_Type;

   function Nodes_Per_Level (Data  : Data_Type;
                             Level : Integer) return Natural;

   function Calculate_Offset (Params : Graph_Params_Type;
                              Level  : Natural) return Integer;

   type Energy_Params_Type is
   record
      Factor_Sector_Too_Wide          : Long_Integer;
      Factor_Sector_Too_Narrow        : Long_Integer;
      Factor_Arc_Label_Too_Wide       : Long_Integer;
      Factor_Arc_Label_Too_Narrow     : Long_Integer;
      Factor_Level_Spacing_Too_Wide   : Long_Integer;
      Factor_Level_Spacing_Too_Narrow : Long_Integer;
      Factor_Radius_Spacing           : Long_Integer;
      Text_Border                     : Long_Integer;
      Font_Size                       : Positive;
   end record;

   function Create_Energy_Params (Factor_Sector_Too_Wide          : Long_Integer :=   10;
                                  Factor_Sector_Too_Narrow        : Long_Integer := 1000;
                                  Factor_Arc_Label_Too_Wide       : Long_Integer := 1000;
                                  Factor_Arc_Label_Too_Narrow     : Long_Integer := 1000;
                                  Factor_Level_Spacing_Too_Wide   : Long_Integer :=   10;
                                  Factor_Level_Spacing_Too_Narrow : Long_Integer :=  500;
                                  Factor_Radius_Spacing           : Long_Integer :=    5;
                                  Text_Border                     : Long_Integer :=   20;
                                  Font_Size                       : Positive     :=   10) return Energy_Params_Type is
   ((Factor_Sector_Too_Wide,
     Factor_Sector_Too_Narrow,
     Factor_Level_Spacing_Too_Wide,
     Factor_Level_Spacing_Too_Narrow,
     Factor_Arc_Label_Too_Wide,
     Factor_Arc_Label_Too_Narrow,
     Factor_Radius_Spacing,
     Text_Border,
     Font_Size));

   function Labeled_Arc_Energy (Arc         : Primitives.Arc_Params_Type;
                                Label       : String;
                                Font_Size   : Integer;
                                Text_Border : Long_Integer;
                                Too_Wide    : Long_Integer;
                                Too_Narrow  : Long_Integer) return Long_Integer;

end SCSC.Graph;

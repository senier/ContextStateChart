with Ada.Containers.Generic_Array_Sort;
with SCSC.SVG;
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

   --------------
   -- Set_Edge --
   --------------

   procedure Set_Edge (Data       : in out Data_Type;
                       Node_Index :        Positive;
                       Edge_Index :        Positive;
                       Edge       :        Edge_Type)
   is
   begin
      Data (Node_Index).Edges_Data (Edge_Index) := Edge;
   end Set_Edge;

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

   function Create_Polar (Center    : Types.Point;
                          Radius    : Natural;
                          Spacing   : Spacing_Type;
                          Padding   : Natural  := 0;
                          Font_Size : Positive := 10) return Graph_Params_Type
   is
      Result : Graph_Params_Type := (Center         => Center,
                                     Radius         => Radius,
                                     Spacing        => (others => 0),
                                     Spacing_Length => Spacing'Length,
                                     Padding        => Padding,
                                     Font_Size      => Font_Size);
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

   -----------------
   -- Get_Spacing --
   -----------------

   function Get_Spacing (Params : Graph_Params_Type;
                         Index  : Spacing_Index) return Natural is (Params.Spacing (Index));

   -----------------
   -- Set_Spacing --
   -----------------

   procedure Set_Spacing (Params : in out Graph_Params_Type;
                          Index  :        Spacing_Index;
                          Value  :        Natural)
   is
   begin
      Params.Spacing (Index) := Value;
   end Set_Spacing;

   ----------------
   -- Get_Radius --
   ----------------

   function Get_Radius (Params : Graph_Params_Type) return Natural is (Params.Radius);

   ----------------
   -- Get_Levels --
   ----------------

   function Get_Levels (Data : Data_Type) return Levels_Type
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

   ----------------------
   -- Create_Connector --
   ----------------------

   function Create_Connector (Params    : Graph_Params_Type;
                              Data      : Data_Type;
                              Sectors   : Annular_Sectors_Type;
                              Edge      : Edge_Type;
                              Index     : Positive;
                              ID        : String) return SXML.Document_Type
   is
      Source_Port : constant Port_Type := Edge.Source_Port;
      Dest_Port   : constant Port_Type := Edge.Dest_Port;

      Start : constant Types.Point := Types.P (Primitives.Port (Params    => Sectors (Index),
                                                                Position  => Source_Port.Pos,
                                                                Port_No   => Source_Port.Num,
                                                                Num_Ports => Data (Index).Ports (Source_Port.Pos)));

      Stop : constant Types.Point  := Types.P (Primitives.Port (Params    => Sectors (Edge.Dest),
                                                                Position  => Dest_Port.Pos,
                                                                Port_No   => Dest_Port.Num,
                                                                Num_Ports => Data (Edge.Dest).Ports (Dest_Port.Pos)));

      P : constant Primitives.Connector_Params_Type := Primitives.Cartesian (Center    => Params.Center,
                                                                             Start     => Start,
                                                                             Stop      => Stop,
                                                                             Radius    => Edge.Radius,
                                                                             Direction => Edge.Dir);
   begin
      return Primitives.Connector (Params => P,
                                   ID     => ID,
                                   Text   => Edge.Get_Label);
   end Create_Connector;

   ---------------------
   -- Nodes_Per_Level --
   ---------------------

   function Nodes_Per_Level (Data  : Data_Type;
                             Level : Integer) return Natural
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

   ----------------------
   -- Calculate_Offset --
   ----------------------

   function Calculate_Offset (Params : Graph_Params_Type;
                              Level  : Natural) return Integer is
      Result : Integer := 0;
   begin
      for I in Natural (Params.Spacing'First) .. Level loop
         Result := Result + (if I <= Natural (Params.Spacing'Last)
                             then Params.Radius + Integer (Params.Spacing (Spacing_Index (I)))
                             else Params.Radius);
      end loop;

      return Result;

   end Calculate_Offset;

   ------------
   -- Layout --
   ------------

   procedure Layout (Params    :     Graph_Params_Type;
                     Data      :     Data_Type;
                     ID        :     String;
                     Positions :     Positions_Type;
                     Length    : out Natural;
                     Sectors   : out Annular_Sectors_Type;
                     Energy    : out Long_Integer)
   is
      I       : Positive;
      Start   : Types.Angle := 0.0;
      Weights : Natural;
      Levels  : constant Levels_Type := Get_Levels (Data);
   begin
      Length := 0;
      Energy := 0;

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
            I := Positions (Index);
            if Data (I).Level = Levels (L).Value
            then
               declare
                  use type Types.Angle;
                  use Primitives;
                  Offset  : constant Natural := Calculate_Offset (Params, L);
                  Spacing : constant Types.Angle := Types.Angle
                     (Math.Arcsin (Float (Params.Padding) / Float (Offset + Params.Radius / 2),
                                   Cycle => 360.0));
                  Circle  : constant Float := 360.0 - (Float (Nodes_Per_Level (Data, Levels (L).Value))
                                                              * Float (Spacing));
                  Size    : constant Types.Angle := Types.Angle (Circle * Float (Data (I).Weight) / Float (Weights));
                  Stop    : constant Types.Angle := Start + Size;
                  AP      : constant Annular_Sector_Params_Type := Polar (Center => Params.Center,
                                                                          Offset => Offset,
                                                                          Radius => Params.Radius,
                                                                          Start  => Start,
                                                                          Stop   => Start + Size);
                  AS      : constant SXML.Document_Type := Annular_Sector (Params => AP,
                                                                           Text   => Data (I).Get_Label,
                                                                           ID     => ID & "_AS_" & To_ID (I));
               begin
                  Sectors (I) := AP;
                  Length := Length + AS'Length;
                  Start := Stop + Spacing;
               end;
            end if;
         end loop;
      end loop;

      for I in Data'Range
      loop
         for E of Data (I).Get_Edges
         loop
            declare
               Connector_ID : constant String := ID & "_C_" & To_ID (I) & "_" & To_ID (E.Dest);
               C : constant SXML.Document_Type := Create_Connector (Params    => Params,
                                                                    Data      => Data,
                                                                    Sectors   => Sectors,
                                                                    Edge      => E,
                                                                    Index     => I,
                                                                    ID        => Connector_ID);
            begin
               Length := Length + C'Length;
            end;
         end loop;
      end loop;

   end Layout;

   ------------------
   -- Create_Graph --
   ------------------

   function Create_Graph (Params    : Graph_Params_Type;
                          Sectors   : Annular_Sectors_Type;
                          Length    : Natural;
                          Data      : Data_Type;
                          Positions : Positions_Type;
                          ID        : String := "") return SXML.Document_Type
   is
      Offset  : SXML.Offset_Type := 0;
      I       : Positive;
   begin

      declare
         S : SXML.Document_Type (1 .. SXML.Index_Type (Length)) := (others => SXML.Null_Node);
         use type SXML.Offset_Type;
      begin
         for Index in Data'Range
         loop
            I := Positions (Index);
            declare
               Sector : constant SXML.Document_Type :=
                  Primitives.Annular_Sector (Params => Sectors (I),
                                             Text   => Data (I).Get_Label,
                                             ID     => ID & "_AS_" & To_ID (I));
            begin
               SXML.Append (S, Offset, Sector);
               Offset := Offset + Sector'Length;
               for J in Data (I).Get_Edges'Range
               loop
                  declare
                     E : constant Edge_Type := Data (I).Get_Edges (J);
                     Connector_ID : constant String := ID & "_C_" & To_ID (I) & "_" & To_ID (J);
                     C : constant SXML.Document_Type := Create_Connector (Params    => Params,
                                                                          Data      => Data,
                                                                          Sectors   => Sectors,
                                                                          Edge      => E,
                                                                          Index     => I,
                                                                          ID        => Connector_ID);
                  begin
                     SXML.Append (SXML.Document_Type (S), Offset, C);
                     Offset := Offset + C'Length;
                  end;
               end loop;
            end;
         end loop;
         return SVG.Group (Element => S,
                           ID      => ID);
      end;
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

   --------------
   -- Identity --
   --------------

   procedure Identity (Positions : out Positions_Type)
   is
   begin
      for P in Positions'Range
      loop
         Positions (P) := P;
      end loop;
   end Identity;

   -------------
   -- Get_Dir --
   -------------

   function Get_Dir (Edge : Edge_Type) return Primitives.Dir_Type is (Edge.Dir);

   -------------
   -- Set_Dir --
   -------------

   procedure Set_Dir (Edge : in out Edge_Type;
                      Dir  :        Primitives.Dir_Type)
   is
   begin
      Edge.Dir := Dir;
   end Set_Dir;

end SCSC.Graph;

with SCSC.Primitives;
with SCSC.Random;
with SCSC.Shuffle;
with SXML.Parser;
with SXML.Query;

package body SCSC.GEXF is

   type Level_Type (Default : Boolean := True) is
   record
      case Default is
         when False =>
            First : Natural;
            Last  : Natural;
         when True =>
            null;
      end case;
   end record;

   type Levels_Type is array (Natural range <>) of Level_Type;

   ------------------
   -- Parse_Levels --
   ------------------

   function Parse_Levels (Levels : String) return Levels_Type;

   function Parse_Levels (Levels : String) return Levels_Type
   is
      function Get_Num_Levels return Natural;

      function Get_Num_Levels return Natural
      is
         Result : Natural := 1;
      begin
         for C of Levels
         loop
            if C = '<'
            then
               Result := Result + 1;
            end if;
         end loop;
         return Result;
      end Get_Num_Levels;

      Num_Levels : constant Natural := Get_Num_Levels;
   begin
      declare
         Result : Levels_Type (1 .. Num_Levels) := (others => (Default => True));
         First  : Natural := Levels'First;
         Last   : Natural := Levels'First - 1;
         Pos    : Natural := 1;
      begin
         loop
            while Last < Levels'Last and then Levels (Last + 1) /= '<'
            loop
               Last := Last + 1;
            end loop;

            Result (Pos) := (if First > Last
                             then (Default => True)
                             else (Default => False, First => First, Last => Last));

            Pos := Pos + 1;
            exit when Last >= Levels'Last;

            Last  := Last + 1;
            First := Last + 1;

         end loop;
         return Result;
      end;
   end Parse_Levels;

   ---------------
   -- Get_Level --
   ---------------

   function Get_Level (Levels     : String;
                       Level_List : Levels_Type;
                       Name       : String) return Natural;

   function Get_Level (Levels     : String;
                       Level_List : Levels_Type;
                       Name       : String) return Natural
   is
      Default_Pos : Natural := 0;
   begin
      for I in Level_List'Range
      loop
         if not Level_List (I).Default
         then
            if Name = Levels (Level_List (I).First .. Level_List (I).Last)
            then
               return I;
            end if;
         else
            Default_Pos := I;
         end if;
      end loop;
      return Default_Pos;
   end Get_Level;

   -------------------
   -- Default_Level --
   -------------------

   function Default_Level (Level_List : Levels_Type) return Natural;

   function Default_Level (Level_List : Levels_Type) return Natural is
   begin
      for I in Level_List'Range
      loop
         if Level_List (I).Default then
            return I;
         end if;
      end loop;
      return 0;
   end Default_Level;

   --------------
   -- To_Level --
   --------------

   procedure To_Level (Document   : SXML.Document_Type;
                       State      :     SXML.Query.State_Type;
                       Levels     :     String;
                       Level_List :     Levels_Type;
                       ID         :     String;
                       Level      : out Natural);

   procedure To_Level (Document   : SXML.Document_Type;
                       State      :     SXML.Query.State_Type;
                       Levels     :     String;
                       Level_List :     Levels_Type;
                       ID         :     String;
                       Level      : out Natural)
   is
      Kind   : constant SXML.Query.State_Type :=
                  SXML.Query.Path (State, Document, "/node/attvalues/attvalue[@for=" & ID & "]");
      use type SXML.Result_Type;
   begin
      if Kind.Result /= SXML.Result_OK
      then
         Level := Default_Level (Level_List);
         return;
      end if;

      declare
         Value : constant String := SXML.Query.Attribute (Kind, Document, "value");
      begin
         if Value = "" then
            return;
         end if;

         Level := Get_Level (Levels, Level_List, Value);
      end;

   end To_Level;

   -----------------
   -- Node_Offset --
   -----------------

   function Node_Offset (Document : SXML.Document_Type;
                         Root     : SXML.Query.State_Type;
                         Node_ID  : String) return Integer;

   function Node_Offset (Document : SXML.Document_Type;
                         Root     : SXML.Query.State_Type;
                         Node_ID  : String) return Integer
   is
      Edge : SXML.Query.State_Type := SXML.Query.Path
                                        (State        => Root,
                                         Document     => Document,
                                         Query_String => "/gexf/graph/nodes/node");
      Offset : Integer := 0;
      use type SXML.Result_Type;
   begin
      while Edge.Result = SXML.Result_OK loop
         if SXML.Query.Is_Open (Document, Edge) then
            declare
               ID : constant String := SXML.Query.Attribute (Edge, Document, "id");
            begin
               if ID = Node_ID then
                  return Offset;
               end if;
            end;
            Offset := Offset + 1;
         end if;
         Edge := SXML.Query.Sibling (Edge, Document);
      end loop;
      return -1;
   end Node_Offset;

   --------------
   -- To_Edges --
   --------------

   subtype Radius is Integer range -100 .. 100;
   package Random_Radius is new SCSC.Random (Radius);

   subtype Dir is Primitives.Dir_Type range Primitives.Dir_CW .. Primitives.Dir_CCW;
   package Random_Dir is new SCSC.Random (Dir);

   subtype Pos is Primitives.Pos_Type range Primitives.Pos_Outer .. Primitives.Pos_Inner;
   package Random_Pos is new SCSC.Random (Pos);

   function To_Edges (Document   : SXML.Document_Type;
                      Root       : SXML.Query.State_Type;
                      Node       : SXML.Query.State_Type;
                      Label_ID   : String;
                      First_Node : Natural) return Graph.Edges_Type;

   function To_Edges (Document   : SXML.Document_Type;
                      Root       : SXML.Query.State_Type;
                      Node       : SXML.Query.State_Type;
                      Label_ID   : String;
                      First_Node : Natural) return Graph.Edges_Type is
      Edge : SXML.Query.State_Type;
   begin
      declare
         ID        : constant String := SXML.Query.Attribute (Node, Document, "id");
         Num_Edges : Natural := 0;
         use type SXML.Result_Type;
      begin
         if ID = "" then
            return Graph.Null_Edges;
         end if;

         Edge := SXML.Query.Path
                     (State        => Root,
                      Document     => Document,
                      Query_String => "/gexf/graph/edges/edge");

         while Edge.Result = SXML.Result_OK
         loop
            if
               SXML.Query.Has_Attribute (Edge, Document, "source")
               and then SXML.Query.Attribute (Edge, Document, "source") = ID
            then
               Num_Edges := Num_Edges + 1;
            end if;
            Edge := SXML.Query.Sibling (Edge, Document);
         end loop;

         Edge := SXML.Query.Path
                     (State        => Root,
                      Document     => Document,
                      Query_String => "/gexf/graph/edges/edge");

         declare
            NE : constant Natural := Num_Edges;
            Result   : Graph.Edges_Type (1 .. NE) := (others => Graph.Null_Edge);
            Position : Natural := 1;
         begin
            while Edge.Result = SXML.Result_OK
            loop
               if
                  SXML.Query.Has_Attribute (Edge, Document, "source")
                  and then SXML.Query.Attribute (Edge, Document, "source") = ID
                  and then SXML.Query.Has_Attribute (Edge, Document, "target")
               then
                  declare
                     Target : constant String := SXML.Query.Attribute (Edge, Document, "target");
                     Offset : constant Integer := Node_Offset (Document, Root, Target);
                     Index  : constant Natural := First_Node + Offset;

                     Label_State : constant SXML.Query.State_Type :=
                        SXML.Query.Path (Edge, Document, "/edge/attvalues/attvalue[@for=" & Label_ID & "]");
                     Label  : constant String := (if Label_State.Result = SXML.Result_OK
                                                  then SXML.Query.Attribute (Label_State, Document, "value")
                                                  else "");
                     R  : constant Radius := Random_Radius.Get_Random;
                     D  : constant Primitives.Dir_Type := Random_Dir.Get_Random;
                     SP : constant Primitives.Pos_Type := Random_Pos.Get_Random;
                     DP : constant Primitives.Pos_Type := Random_Pos.Get_Random;
                  begin
                     Result (Position) := Graph.Create_Edge (Dest        => Index,
                                                             Dir         => D,
                                                             Radius      => R,
                                                             Source_Port => (1, SP),
                                                             Dest_Port   => (1, DP),
                                                             Label       => Label);
                  end;
                  Position := Position + 1;
               end if;
               Edge := SXML.Query.Sibling (Edge, Document);
            end loop;
            return Result;
         end;
      end;
   end To_Edges;

   ---------------------
   -- Calculate_Ports --
   ---------------------

   package Random_Natural is new SCSC.Random (Natural);

   procedure Calculate_Ports (Data : in out Graph.Data_Type);

   procedure Calculate_Ports (Data : in out Graph.Data_Type) is
   begin
      for I in Data'Range loop
         declare
            Ports : Natural := Data (I).Get_Edges'Length;
            Inner, Outer : Natural;
            R : constant Natural := Random_Natural.Get_Random;
         begin
            for J in Data'Range loop
               for E of Data (J).Get_Edges loop
                  if E.Get_Dest = I then
                     Ports := Ports + 1;
                  end if;
               end loop;
            end loop;

            Outer := R mod Ports + 1;
            Inner := Ports - Outer;

            Graph.Set_Ports (Data (I), (Outer, Inner));
            Graph.Set_Weight (Data (I), (if Inner > Outer then Inner else Outer));

            declare
               type Pos_Offset_Type is
               record
                  Offset : Natural;
                  Pos    : Primitives.Pos_Type;
               end record;

               type Ports_Type is array (Positive range <>) of Pos_Offset_Type;
               Last : constant Natural := Ports;
               P : Ports_Type (1 .. Last);
               M : Natural := P'First;

               use type Primitives.Pos_Type;

               procedure Shuffle_Ports is new SCSC.Shuffle.Generic_Shuffle (Element_Type => Pos_Offset_Type,
                                                                            Data_Type    => Ports_Type);
            begin
               for I in P'Range loop
                  P (I) := (if I <= Outer
                            then (I, Primitives.Pos_Outer)
                             else (I - Outer, Primitives.Pos_Inner));
               end loop;

               Shuffle_Ports (P);

               for J in Data (I).Get_Edges'Range loop
                  Graph.Set_Source_Port (Data, I, J, (P (M).Offset, P (M).Pos));
                  M := M + 1;
               end loop;

               for K in Data'Range loop
                  for L in Data (K).Get_Edges'Range loop
                     if Data (K).Get_Edges (L).Get_Dest = I then
                        Graph.Set_Dest_Port (Data, K, L, (P (M).Offset, P (M).Pos));
                        M := M + 1;
                     end if;
                  end loop;
               end loop;
            end;
         end;
      end loop;
   end Calculate_Ports;

   ------------
   -- Import --
   ------------

   Scratch : SXML.Document_Type (1 .. 100000);

   procedure Import (GEXF_Data :     String;
                     Data      : out Graph.Data_Type;
                     Last      : out Natural;
                     Label     :     String := "";
                     Level     :     String := "";
                     Levels    :     String := "")
   is
      Match       : SXML.Parser.Match_Type;
      Root        : SXML.Query.State_Type;
      Node        : SXML.Query.State_Type;
      Level_State : SXML.Query.State_Type;
      Label_State : SXML.Query.State_Type;
      Position    : Natural;
      Node_Num    : Natural := 0;
      Level_List  : constant Levels_Type := Parse_Levels (Levels);
      use type SXML.Parser.Match_Type;
      use type SXML.Result_Type;
   begin
      Last := 0;
      Data := (others => Graph.Create_Node);

      SXML.Parser.Parse (GEXF_Data, Scratch, Match, Position);
      if Match /= SXML.Parser.Match_OK
      then
         return;
      end if;

      Root := SXML.Query.Init (Scratch);
      Level_State := SXML.Query.Path
                       (State        => Root,
                        Document     => Scratch,
                        Query_String => "/gexf/graph/attributes[@class=node]/attribute[@title=" & Level & "]");

      Label_State := SXML.Query.Path
                       (State        => Root,
                        Document     => Scratch,
                        Query_String => "/gexf/graph/attributes[@class=edge]/attribute[@title=" & Label & "]");

      if Level_State.Result /= SXML.Result_OK
         or Label_State.Result /= SXML.Result_OK
      then
         return;
      end if;

      declare
         Level_ID : constant String := SXML.Query.Attribute (Level_State, Scratch, "id");
         Label_ID : constant String := SXML.Query.Attribute (Label_State, Scratch, "id");
      begin

         Node := SXML.Query.Path (Root, Scratch, "/gexf/graph/nodes/node");

         loop
            if (Node.Result /= SXML.Result_OK and Node.Result /= SXML.Result_Not_Found) or
               Node_Num >= Data'Length
            then
               return;
            end if;

            exit when Node.Result = SXML.Result_Not_Found;

            if SXML.Query.Is_Open (Scratch, Node)
            then
               declare
                  Level_Num : Natural;
                  Edges     : constant Graph.Edges_Type := To_Edges (Document   => Scratch,
                                                                     Root       => Root,
                                                                     Node       => Node,
                                                                     Label_ID   => Label_ID,
                                                                     First_Node => Data'First);
               begin
                  To_Level (Document   => Scratch,
                            State      => Node,
                            Levels     => Levels,
                            Level_List => Level_List,
                            ID         => Level_ID,
                            Level      => Level_Num);
                  Data (Data'First + Node_Num) :=
                     Graph.Create_Node (Label => SXML.Query.Attribute (Node, Scratch, "label"),
                                        Level => Level_Num,
                                        Edges => Edges);
               end;
               Node_Num := Node_Num + 1;
            end if;
            Node := SXML.Query.Sibling (Node, Scratch);
         end loop;
      end;

      Last := Data'First + Node_Num - 1;

      Calculate_Ports (Data (Data'First .. Last));

   end Import;

end SCSC.GEXF;

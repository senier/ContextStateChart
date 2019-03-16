with SXML.Parser;
with SXML.Query;

with Ada.Text_IO; --  use Ada.Text_IO;

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
      Kind       : SXML.Query.State_Type :=
                     SXML.Query.Path (State, Document, "/node/attvalues/attvalue[@for=" & ID & "]");
      Value      : String (1 .. 100);
      Value_Last : Natural;
      Result     : SXML.Result_Type;
      use type SXML.Result_Type;
   begin
      if Kind.Result /= SXML.Result_OK
      then
         Level := Default_Level (Level_List);
         return;
      end if;

      Kind := SXML.Query.Find_Attribute (Kind, Document, "value");
      if Kind.Result /= SXML.Result_OK
      then
         Level := Default_Level (Level_List);
         return;
      end if;

      SXML.Query.Value (Kind, Document, Result, Value, Value_Last);
      if Result /= SXML.Result_OK
      then
         Level := Default_Level (Level_List);
         return;
      end if;

      Level := Get_Level (Levels, Level_List, Value (Value'First .. Value_Last));

   end To_Level;

   -----------
   -- Label --
   -----------

   function Label (Document : SXML.Document_Type;
                   State    : SXML.Query.State_Type;
                   Max_Len  : Natural := 100) return String;

   function Label (Document : SXML.Document_Type;
                   State    : SXML.Query.State_Type;
                   Max_Len  : Natural := 100) return String
   is
      Result    : SXML.Result_Type;
      Tmp_Last  : Natural;
      Tmp_State : constant SXML.Query.State_Type := SXML.Query.Find_Attribute (State, Document, "label");
      Tmp_End   : constant Natural := Max_Len;
      Tmp       : String (1 .. Tmp_End) := (others => ASCII.NUL);
      use type SXML.Result_Type;
   begin
      if Tmp_State.Result /= SXML.Result_OK
      then
         return "";
      end if;

      SXML.Query.Value (Tmp_State, Document, Result, Tmp, Tmp_Last);
      if Result /= SXML.Result_OK
      then
         return "";
      end if;
      return Tmp (1 .. Tmp_Last);
   end Label;

   --------------
   -- To_Edges --
   --------------

   function To_Edges (Document : SXML.Document_Type;
                      Root     : SXML.Query.State_Type;
                      Node     : SXML.Query.State_Type) return Graph.Edges_Type;

   function To_Edges (Document : SXML.Document_Type;
                      Root     : SXML.Query.State_Type;
                      Node     : SXML.Query.State_Type) return Graph.Edges_Type is
      Result  : SXML.Result_Type;
      ID      : String (1 .. 100);
      ID_Last : Natural;
      Attr    : SXML.Query.State_Type;
      Edge    : SXML.Query.State_Type;
      use type SXML.Result_Type;
      use Ada.Text_IO;
   begin
      Attr := SXML.Query.Find_Attribute (Node, Document, "id");
      if Attr.Result /= SXML.Result_OK then
         return Graph.Null_Edges;
      end if;

      SXML.Query.Value (Attr, Document, Result, ID, ID_Last);
      if Result /= SXML.Result_OK
      then
         return Graph.Null_Edges;
      end if;

      Edge := SXML.Query.Path
                  (State        => Root,
                   Document     => Document,
                   Query_String => "/gexf/graph/edges/edge[@source=" & ID (ID'First .. ID_Last) & "]");

      while Edge.Result = SXML.Result_OK
      loop
         declare
            Target      : SXML.Query.State_Type;
            Target_Last : Natural;
            Target_ID   : String (1 .. 100);
         begin
            Target := SXML.Query.Find_Attribute (Edge, Document, "target");
            if Target.Result /= SXML.Result_OK
            then
               return Graph.Null_Edges;
            end if;

            SXML.Query.Value (Target, Document, Result, Target_ID, Target_Last);
            if Result /= SXML.Result_OK
            then
               return Graph.Null_Edges;
            end if;

            Put_Line (Standard_Error, ID (ID'First .. ID_Last) & " => " & Target_ID (Target_ID'First .. Target_Last));
         end;
         Edge := SXML.Query.Sibling (Edge, Document);
      end loop;

      return Graph.Null_Edges;
   end To_Edges;

   ------------
   -- Import --
   ------------

   Scratch : SXML.Document_Type (1 .. 100000);

   procedure Import (GEXF_Data :     String;
                     Data      : out Graph.Data_Type;
                     Last      : out Natural;
                     Level_ID  :     String := "";
                     Levels    :     String := "")
   is
      Match      : SXML.Parser.Match_Type;
      Result     : SXML.Result_Type;
      Root       : SXML.Query.State_Type;
      Node       : SXML.Query.State_Type;
      Kind       : SXML.Query.State_Type;
      ID         : String (1 .. 3);
      Position   : Natural;
      ID_Last    : Natural;
      Node_Num   : Natural := 0;
      Level_List : constant Levels_Type := Parse_Levels (Levels);
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
      Kind := SXML.Query.Path
                  (State        => Root,
                   Document     => Scratch,
                   Query_String => "/gexf/graph/attributes[@class=node]/attribute[@title=" & Level_ID & "]");
      if Kind.Result /= SXML.Result_OK
      then
         return;
      end if;

      Kind := SXML.Query.Find_Attribute (Kind, Scratch, "id");
      if Kind.Result /= SXML.Result_OK
      then
         return;
      end if;

      SXML.Query.Value (Kind, Scratch, Result, ID, ID_Last);
      if Result /= SXML.Result_OK
      then
         return;
      end if;

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
               Edges     : constant Graph.Edges_Type := To_Edges (Scratch, Root, Node);
            begin
               To_Level (Document   => Scratch,
                         State      => Node,
                         Levels     => Levels,
                         Level_List => Level_List,
                         ID         => ID (ID'First .. ID_Last),
                         Level      => Level_Num);
               Data (Data'First + Node_Num) := Graph.Create_Node (Label => Label (Scratch, Node),
                                                                  Level => Level_Num,
                                                                  Edges => Edges);
            end;
            Node_Num := Node_Num + 1;
         end if;

         Node := SXML.Query.Sibling (Node, Scratch);
      end loop;

      Last := Data'First + Node_Num - 1;

   end Import;

end SCSC.GEXF;

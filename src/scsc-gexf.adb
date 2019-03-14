with SXML.Parser;
with SXML.Query;

package body SCSC.GEXF is

   Scratch : SXML.Document_Type (1 .. 100000);

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

   procedure To_Level (State      :     SXML.Query.State_Type;
                       Levels     :     String;
                       Level_List :     Levels_Type;
                       ID         :     String;
                       Level      : out Natural);

   procedure To_Level (State      :     SXML.Query.State_Type;
                       Levels     :     String;
                       Level_List :     Levels_Type;
                       ID         :     String;
                       Level      : out Natural)
   is
      Kind       : SXML.Query.State_Type :=
                     SXML.Query.Path (State, Scratch, "/node/attvalues/attvalue[@for=" & ID & "]");
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

      Kind := SXML.Query.Find_Attribute (Kind, Scratch, "value");
      if Kind.Result /= SXML.Result_OK
      then
         Level := Default_Level (Level_List);
         return;
      end if;

      SXML.Query.Value (Kind, Scratch, Result, Value, Value_Last);
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

   ------------
   -- Import --
   ------------

   procedure Import (GEXF_Data :     String;
                     Data      : out Graph.Data_Type;
                     Last      : out Natural;
                     Level_ID  :     String := "";
                     Levels    :     String := "")
   is
      Match      : SXML.Parser.Match_Type;
      Result     : SXML.Result_Type;
      State      : SXML.Query.State_Type;
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

      State := SXML.Query.Init (Scratch);
      Kind := SXML.Query.Path
                  (State        => State,
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

      State := SXML.Query.Path (State, Scratch, "/gexf/graph/nodes/node");

      loop
         if (State.Result /= SXML.Result_OK and State.Result /= SXML.Result_Not_Found) or
            Node_Num >= Data'Length
         then
            return;
         end if;

         exit when State.Result = SXML.Result_Not_Found;

         if SXML.Query.Is_Open (Scratch, State)
         then
            declare
               Level_Num : Natural;
            begin
               To_Level (State      => State,
                         Levels     => Levels,
                         Level_List => Level_List,
                         ID         => ID (ID'First .. ID_Last),
                         Level      => Level_Num);
               Data (Data'First + Node_Num) := Graph.Create_Node (Label   => Label (Scratch, State),
                                                                  Level   => Level_Num);
            end;
            Node_Num := Node_Num + 1;
         end if;

         State := SXML.Query.Sibling (State, Scratch);
      end loop;

      Last := Data'First + Node_Num - 1;

   end Import;

end SCSC.GEXF;

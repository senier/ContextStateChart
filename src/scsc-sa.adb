with SCSC.Random;
with Ada.Text_IO;

package body SCSC.SA is

   subtype Effective_Moves is Moves range Move_Decrease_Random_Level_Spacing .. Moves'Last;

   procedure Apply (M            : in out Move_Type;
                    Graph_Params : in out Graph.Graph_Params_Type;
                    Graph_Data   : in out Graph.Data_Type;
                    Sectors      : in out Graph.Annular_Sectors_Type);

   procedure Revert (M            :        Move_Type;
                     Graph_Params : in out Graph.Graph_Params_Type;
                     Graph_Data   : in out Graph.Data_Type;
                     Sectors      : in out Graph.Annular_Sectors_Type);

   function Move (Graph_Params    : Graph.Graph_Params_Type;
                  Optimize_Params : Params_Type;
                  Graph_Data      : Graph.Data_Type;
                  Sectors         : Graph.Annular_Sectors_Type) return Move_Type;

   package Random_Move is new SCSC.Random (Effective_Moves);
   package Random_Spacing_Index is new SCSC.Random (Graph.Spacing_Index);
   package Random_Natural is new SCSC.Random (Natural);

   subtype Valid_Dir_Type is Primitives.Dir_Type range Primitives.Dir_CW .. Primitives.Dir_CCW;
   package Random_Direction is new SCSC.Random (Valid_Dir_Type);

   -------------------
   -- Set_Direction --
   -------------------

   procedure Set_Direction (Graph_Data : in out Graph.Data_Type;
                            Node_Index :        Positive;
                            Edge_Index :        Natural;
                            Direction  :        Primitives.Dir_Type)
   is
      Edges : constant Graph.Edges_Type := Graph.Get_Edges (Graph_Data (Node_Index));
      E     : Graph.Edge_Type           := Edges (Edge_Index);
   begin
      Graph.Set_Dir (E, Direction);
      Graph.Set_Edge (Graph_Data, Node_Index, Edge_Index, E);
   end Set_Direction;

   -----------------------
   -- Swap_Source_Ports --
   -----------------------

   procedure Swap_Source_Ports (Graph_Data   : in out Graph.Data_Type;
                                Node_Index   :        Positive;
                                Edge_Index_1 :        Natural;
                                Edge_Index_2 :        Natural)
   is
      Tmp : Graph.Port_Type;
   begin
      Tmp := Graph.Get_Source_Port (Graph_Data, Node_Index, Edge_Index_1);
      Graph.Set_Source_Port (Data       => Graph_Data,
                             Node_Index => Node_Index,
                             Edge_Index => Edge_Index_1,
                             Port       => Graph.Get_Source_Port (Graph_Data, Node_Index, Edge_Index_2));
      Graph.Set_Source_Port (Graph_Data, Node_Index, Edge_Index_2, Tmp);
   end Swap_Source_Ports;

   -----------
   -- Apply --
   -----------

   procedure Apply (M            : in out Move_Type;
                    Graph_Params : in out Graph.Graph_Params_Type;
                    Graph_Data   : in out Graph.Data_Type;
                    Sectors      : in out Graph.Annular_Sectors_Type)
   is
      Spacing : Natural;
      pragma Unreferenced (Sectors);
   begin
      case M.Kind is
         when Move_Noop =>
            null;

         when Move_Decrease_Random_Level_Spacing =>
            Spacing := Graph.Get_Spacing (Graph_Params, M.Spacing_Index);
            if Spacing >= M.Spacing_Value then
               Graph.Set_Spacing (Graph_Params, M.Spacing_Index, Spacing - M.Spacing_Value);
            else
               M.Spacing_Value := 0;
            end if;

         when Move_Increase_Random_Level_Spacing =>
            Spacing := Graph.Get_Spacing (Graph_Params, M.Spacing_Index);
            if Spacing <= Natural'Last - M.Spacing_Value then
               Graph.Set_Spacing (Graph_Params, M.Spacing_Index, Spacing + M.Spacing_Value);
            else
               M.Spacing_Value := 0;
            end if;

         when Move_Switch_Random_Direction =>
            Set_Direction (Graph_Data, M.Node_Index, M.Edge_Index, M.Direction);

         when Move_Switch_Random_Connector =>
            Swap_Source_Ports (Graph_Data, M.Node_Index_C, M.Edge_Index_1, M.Edge_Index_2);

      end case;
   end Apply;

   -----------
   -- Revert --
   -----------

   procedure Revert (M            :        Move_Type;
                     Graph_Params : in out Graph.Graph_Params_Type;
                     Graph_Data   : in out Graph.Data_Type;
                     Sectors      : in out Graph.Annular_Sectors_Type)
   is
      Spacing : Natural;
      pragma Unreferenced (Sectors);
      use type Primitives.Dir_Type;
   begin
      case M.Kind is
         when Move_Noop =>
            null;

         when Move_Decrease_Random_Level_Spacing =>
            Spacing := Graph.Get_Spacing (Graph_Params, M.Spacing_Index);
            if Spacing <= Natural'Last - M.Spacing_Value then
               Graph.Set_Spacing (Graph_Params, M.Spacing_Index, Spacing + M.Spacing_Value);
            end if;

         when Move_Increase_Random_Level_Spacing =>
            Spacing := Graph.Get_Spacing (Graph_Params, M.Spacing_Index);
            if Spacing >= M.Spacing_Value then
               Graph.Set_Spacing (Graph_Params, M.Spacing_Index, Spacing - M.Spacing_Value);
            end if;

         when Move_Switch_Random_Direction =>
            Set_Direction (Graph_Data, M.Node_Index, M.Edge_Index, (if M.Direction = Primitives.Dir_CW
                                                                   then Primitives.Dir_CCW
                                                                   else Primitives.Dir_CW));

         when Move_Switch_Random_Connector =>
            Swap_Source_Ports (Graph_Data, M.Node_Index_C, M.Edge_Index_1, M.Edge_Index_2);

      end case;
   end Revert;

   ----------
   -- Move --
   ----------

   function Move (Graph_Params    : Graph.Graph_Params_Type;
                  Optimize_Params : Params_Type;
                  Graph_Data      : Graph.Data_Type;
                  Sectors         : Graph.Annular_Sectors_Type) return Move_Type
   is
      pragma Unreferenced (Sectors);
      use type Graph.Spacing_Index;
      Next : constant Moves := Random_Move.Get_Random;
   begin
      case Next is
         when Move_Decrease_Random_Level_Spacing =>
            declare
               S : constant Graph.Spacing_Type  := Graph.Get_Spacing (Graph_Params);
               R : constant Graph.Spacing_Index := Random_Spacing_Index.Get_Random;
            begin
               return
                  (Kind          => Move_Decrease_Random_Level_Spacing,
                   Spacing_Index => S'First + R mod S'Length,
                   Spacing_Value => Optimize_Params.Level_Spacing_Decrease_Step);
            end;
         when Move_Increase_Random_Level_Spacing =>
            declare
               S : constant Graph.Spacing_Type  := Graph.Get_Spacing (Graph_Params);
               R : constant Graph.Spacing_Index := Random_Spacing_Index.Get_Random;
            begin
               return (Kind          => Move_Increase_Random_Level_Spacing,
                       Spacing_Index => S'First + R mod S'Length,
                       Spacing_Value => Optimize_Params.Level_Spacing_Increase_Step);
            end;
         when Move_Switch_Random_Direction =>
            declare
               RN    : constant Natural          := Random_Natural.Get_Random;
               NI    : constant Positive         := Graph_Data'First + RN mod Graph_Data'Length;
               Edges : constant Graph.Edges_Type := Graph.Get_Edges (Graph_Data (NI));
            begin
               if Edges'Length = 0 then
                  return (Kind => Move_Noop);
               end if;

               declare
                  RE : constant Natural        := Random_Natural.Get_Random;
                  EI : constant Integer        := Edges'First + RE mod Edges'Length;
                  R  : constant Valid_Dir_Type := Random_Direction.Get_Random;
               begin
                  return (Kind       => Move_Switch_Random_Direction,
                          Direction  => R,
                          Node_Index => NI,
                          Edge_Index => EI);
               end;
            end;
         when Move_Switch_Random_Connector =>
            declare
               RN    : constant Natural          := Random_Natural.Get_Random;
               NI    : constant Positive         := Graph_Data'First + RN mod Graph_Data'Length;
               Edges : constant Graph.Edges_Type := Graph.Get_Edges (Graph_Data (NI));
            begin
               if Edges'Length < 2 then
                  return (Kind => Move_Noop);
               end if;

               declare
                  RE_1 : constant Natural := Random_Natural.Get_Random;
                  EI_1 : constant Integer := Edges'First + RE_1 mod Edges'Length;
                  RE_2 : constant Natural := Random_Natural.Get_Random;
                  EI_2 : constant Integer := Edges'First + RE_2 mod Edges'Length;
               begin
                  return (Kind         => Move_Switch_Random_Connector,
                          Node_Index_C => NI,
                          Edge_Index_1 => EI_1,
                          Edge_Index_2 => EI_2);
               end;
            end;

         when Move_Noop =>
            return (Kind  => Move_Noop);
      end case;
   end Move;

   -----------------
   -- Print_Debug --
   -----------------

   procedure Print_Debug (Iteration : Long_Integer;
                          M         : Move_Type;
                          Energy_1  : Long_Integer;
                          Energy_2  : Long_Integer;
                          Threshold : Long_Integer)
      with SPARK_Mode => Off
   is
      use Ada.Text_IO;
   begin
      Put_Line (Standard_Error, "I:" & Iteration'Img & " M: " & M.Kind'Img & " E1:" & Energy_1'Img
                & " E2:" & Energy_2'Img & " Threshold:" & Threshold'Img);
   end Print_Debug;

   --------------
   -- Optimize --
   --------------

   procedure Optimize (ID              :        String;
                       Optimize_Params :        Params_Type;
                       Energy_Params   :        Graph.Energy_Params_Type;
                       Positions       :        Graph.Positions_Type;
                       Length          :    out Natural;
                       Graph_Params    : in out Graph.Graph_Params_Type;
                       Graph_Data      : in out Graph.Data_Type;
                       Sectors         : in out Graph.Annular_Sectors_Type)
   is
      Threshold : Float        := Optimize_Params.Initial_Acceptance_Threshold;
      I         : Long_Integer := 0;
      Energy_1  : Long_Integer := Long_Integer'Last;
      Energy_2  : Long_Integer;
   begin
      loop
         declare
            M : Move_Type := Move (Graph_Params, Optimize_Params, Graph_Data, Sectors);
         begin
            if M.Kind /= Move_Noop then
               Apply (M, Graph_Params, Graph_Data, Sectors);
               Graph.Layout (Data          => Graph_Data,
                             Params        => Graph_Params,
                             Energy_Params => Energy_Params,
                             ID            => ID,
                             Positions     => Positions,
                             Length        => Length,
                             Sectors       => Sectors,
                             Energy        => Energy_2);
               if Optimize_Params.Debug then
                  Print_Debug (I, M, Energy_1, Energy_2, Long_Integer (Threshold));
               end if;

               if Energy_2 < Energy_1
               then
                  Energy_1 := Energy_2;
                  I := 0;
               else
                  if Energy_2 - Energy_1 >= Long_Integer (Threshold)
                  then
                     Revert (M, Graph_Params, Graph_Data, Sectors);
                  end if;
                  I := I + 1;
               end if;
            end if;
         end;
         Threshold := Threshold * Optimize_Params.Threshold_Decay;
         exit when I > Optimize_Params.Max_Unsuccessful_Iterations;
      end loop;
   end Optimize;

end SCSC.SA;

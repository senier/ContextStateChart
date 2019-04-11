with SCSC.Random;
with Ada.Text_IO;

package body SCSC.SA is

   type Moves is
      (Move_Noop,
       Move_Decrease_Random_Level_Spacing,
       Move_Increase_Random_Level_Spacing,
       Move_Switch_Random_Direction);

   subtype Effective_Moves is Moves range Move_Decrease_Random_Level_Spacing .. Move_Switch_Random_Direction;

   type Move_Type (Kind : Moves := Move_Noop) is
   record
      case Kind is
         when Move_Decrease_Random_Level_Spacing
            | Move_Increase_Random_Level_Spacing =>
            Index : Graph.Spacing_Index;
            Value : Natural;
         when Move_Switch_Random_Direction =>
            Dir : Primitives.Dir_Type;
            NI  : Positive;
            EI  : Natural;
         when Move_Noop =>
            null;
      end case;
   end record;

   procedure Apply (M       : in out Move_Type;
                    Params  : in out Graph.Graph_Params_Type;
                    Data    : in out Graph.Data_Type;
                    Sectors : in out Graph.Annular_Sectors_Type);

   procedure Revert (M       :        Move_Type;
                     Params  : in out Graph.Graph_Params_Type;
                     Data    : in out Graph.Data_Type;
                     Sectors : in out Graph.Annular_Sectors_Type);

   function Move (Params     : Graph.Graph_Params_Type;
                  Opt_Params : Params_Type;
                  Data       : Graph.Data_Type;
                  Sectors    : Graph.Annular_Sectors_Type) return Move_Type;

   package Random_Move is new SCSC.Random (Effective_Moves);
   package Random_Spacing_Index is new SCSC.Random (Graph.Spacing_Index);
   package Random_Natural is new SCSC.Random (Natural);

   subtype Valid_Dir_Type is Primitives.Dir_Type range Primitives.Dir_CW .. Primitives.Dir_CCW;
   package Random_Direction is new SCSC.Random (Valid_Dir_Type);

   -------------------
   -- Set_Direction --
   -------------------

   procedure Set_Direction (Data : in out Graph.Data_Type;
                            NI   :        Positive;
                            EI   :        Natural;
                            Dir  :        Primitives.Dir_Type)
   is
      Edges : constant Graph.Edges_Type := Graph.Get_Edges (Data (NI));
      E     : Graph.Edge_Type           := Edges (EI);
   begin
      Graph.Set_Dir (E, Dir);
      Graph.Set_Edge (Data, NI, EI, E);
   end Set_Direction;

   -----------
   -- Apply --
   -----------

   procedure Apply (M       : in out Move_Type;
                    Params  : in out Graph.Graph_Params_Type;
                    Data    : in out Graph.Data_Type;
                    Sectors : in out Graph.Annular_Sectors_Type)
   is
      Spacing : Natural;
      pragma Unreferenced (Sectors);
   begin
      case M.Kind is
         when Move_Noop =>
            null;

         when Move_Decrease_Random_Level_Spacing =>
            Spacing := Graph.Get_Spacing (Params, M.Index);
            if Spacing >= M.Value then
               Graph.Set_Spacing (Params, M.Index, Spacing - M.Value);
            else
               M.Value := 0;
            end if;

         when Move_Increase_Random_Level_Spacing =>
            Spacing := Graph.Get_Spacing (Params, M.Index);
            if Spacing <= Natural'Last - M.Value then
               Graph.Set_Spacing (Params, M.Index, Spacing + M.Value);
            else
               M.Value := 0;
            end if;

         when Move_Switch_Random_Direction =>
            Set_Direction (Data, M.NI, M.EI, M.Dir);
      end case;
   end Apply;

   -----------
   -- Revert --
   -----------

   procedure Revert (M       :        Move_Type;
                     Params  : in out Graph.Graph_Params_Type;
                     Data    : in out Graph.Data_Type;
                     Sectors : in out Graph.Annular_Sectors_Type)
   is
      Spacing : Natural;
      pragma Unreferenced (Sectors);
      use type Primitives.Dir_Type;
   begin
      case M.Kind is
         when Move_Noop =>
            null;

         when Move_Decrease_Random_Level_Spacing =>
            Spacing := Graph.Get_Spacing (Params, M.Index);
            if Spacing <= Natural'Last - M.Value then
               Graph.Set_Spacing (Params, M.Index, Spacing + M.Value);
            end if;

         when Move_Increase_Random_Level_Spacing =>
            Spacing := Graph.Get_Spacing (Params, M.Index);
            if Spacing >= M.Value then
               Graph.Set_Spacing (Params, M.Index, Spacing - M.Value);
            end if;

         when Move_Switch_Random_Direction =>
            Set_Direction (Data, M.NI, M.EI, (if M.Dir = Primitives.Dir_CW
                                              then Primitives.Dir_CCW
                                              else Primitives.Dir_CW));
      end case;
   end Revert;

   ----------
   -- Move --
   ----------

   function Move (Params     : Graph.Graph_Params_Type;
                  Opt_Params : Params_Type;
                  Data       : Graph.Data_Type;
                  Sectors    : Graph.Annular_Sectors_Type) return Move_Type
   is
      pragma Unreferenced (Sectors);
      use type Graph.Spacing_Index;
      Next : constant Moves := Random_Move.Get_Random;
   begin
      case Next is
         when Move_Decrease_Random_Level_Spacing =>
            declare
               S : constant Graph.Spacing_Type := Graph.Get_Spacing (Params);
               R : constant Graph.Spacing_Index := Random_Spacing_Index.Get_Random;
            begin
               return
                  (Kind  => Move_Decrease_Random_Level_Spacing,
                   Index => S'First + R mod S'Length,
                   Value => Opt_Params.Level_Spacing_Decrease_Step);
            end;
         when Move_Increase_Random_Level_Spacing =>
            declare
               S : constant Graph.Spacing_Type := Graph.Get_Spacing (Params);
               R : constant Graph.Spacing_Index := Random_Spacing_Index.Get_Random;
            begin
               return (Kind  => Move_Increase_Random_Level_Spacing,
                       Index => S'First + R mod S'Length,
                       Value => Opt_Params.Level_Spacing_Increase_Step);
            end;
         when Move_Switch_Random_Direction =>
            declare
               RN    : constant Natural          := Random_Natural.Get_Random;
               NI    : constant Positive         := Data'First + RN mod Data'Length;
               Edges : constant Graph.Edges_Type := Graph.Get_Edges (Data (NI));
               RE    : constant Natural          := Random_Natural.Get_Random;
               EI    : constant Integer          := (if Edges'Length > 0
                                                     then Edges'First + RE mod Edges'Length
                                                     else -1);
            begin
               if EI = -1 then
                  return (Kind => Move_Noop);
               else
                  declare
                     R : constant Valid_Dir_Type := Random_Direction.Get_Random;
                  begin
                     return (Kind => Move_Switch_Random_Direction,
                             Dir  => R,
                             NI   => NI,
                             EI   => EI);
                  end;
               end if;
            end;
         when Move_Noop =>
            return (Kind  => Move_Noop);
      end case;
   end Move;

   -----------------
   -- Print_Debug --
   -----------------

   procedure Print_Debug (Iteration : Long_Integer;
                          Energy_1  : Long_Integer;
                          Energy_2  : Long_Integer;
                          Threshold : Long_Integer)
      with SPARK_Mode => Off
   is
      use Ada.Text_IO;
   begin
      Put_Line (Standard_Error, "I:" & Iteration'Img & " E1:" & Energy_1'Img & " E2:" & Energy_2'Img
                & " Threshold:" & Threshold'Img);
   end Print_Debug;

   --------------
   -- Optimize --
   --------------

   procedure Optimize (ID         :        String;
                       Opt_Params :        Params_Type;
                       EP         :        Graph.Energy_Params_Type;
                       Length     :    out Natural;
                       Params     : in out Graph.Graph_Params_Type;
                       Data       : in out Graph.Data_Type;
                       Sectors    : in out Graph.Annular_Sectors_Type;
                       Positions  :        Graph.Positions_Type)
   is
      I         : Long_Integer := 0;
      Energy_1  : Long_Integer := Long_Integer'Last;
      Energy_2  : Long_Integer;
      Threshold : Float := Opt_Params.Initial_Acceptance_Threshold;
   begin
      loop
         declare
            M : Move_Type := Move (Params, Opt_Params, Data, Sectors);
         begin
            Apply (M, Params, Data, Sectors);
            Graph.Layout (Params    => Params,
                          EP        => EP,
                          Data      => Data,
                          ID        => ID,
                          Positions => Positions,
                          Length    => Length,
                          Sectors   => Sectors,
                          Energy    => Energy_2);
            if Opt_Params.Debug then
               Print_Debug (I, Energy_1, Energy_2, Long_Integer (Threshold));
            end if;

            if Energy_2 < Energy_1
            then
               Energy_1 := Energy_2;
               I := 0;
            else
               if Energy_2 - Energy_1 >= Long_Integer (Threshold)
               then
                  Revert (M, Params, Data, Sectors);
               end if;
               I := I + 1;
            end if;
         end;
         Threshold := Threshold * Opt_Params.Threshold_Decay;
         exit when I > Opt_Params.Max_Unsuccessful_Iterations;
      end loop;
   end Optimize;

end SCSC.SA;

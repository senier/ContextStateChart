with Ada.Text_IO; use Ada.Text_IO;
with SCSC.SVG;
with SCSC.Primitives;
with SCSC.Types;

procedure Main
is
   use SCSC.Primitives;
   use SCSC.SVG;
   use SCSC.Types;

   Center  : Point := P (200, 200);

   Arrow_End : Element_Type := Path (Commands => ((Moveto, Absolute, 0, 0),
                                                 (Vertical, Absolute, 4),
                                                 (Lineto, Absolute, 4, 2),
                                                 (ZClosepath, Absolute)),
                                    Style => "fill: blue");

   Arrow_Start : Element_Type := Path (Commands => ((Moveto, Absolute, 0, 2),
                                                  (Lineto, Absolute, 4, 4),
                                                  (Lineto, Absolute, 4, 0),
                                                  (ZClosepath, Absolute)),
                                     Style => "fill: blue");

   SVG : SCSC.SVG.Document_Type := SCSC.SVG.Create_SVG
      (
       Width  => 400,
       Height => 400,

       Child  => Circle (Center,   2, Style => "fill: black; stroke: none")
               + Circle (Center, 200, Style => "fill: none; stroke: black")

               + Connector (Center, P (56, 300), P (68, 116),
                            Text      => "Inner/Default",
                            Textstyle => "font-size: 10px; fill: blue; stroke: none",
                            Radius    => 10,
                            Position  => Pos_Inner,
                            Style     => "fill: none; stroke: red",
                            COID      => "C1")

               + Connector (Center, P (65, 100), P (180, 50),
                            Text      => "Inner/Start/CW",
                            Textstyle => "font-size: 10px; fill: blue; stroke: none",
                            Radius    => 20,
                            Align     => SCSC.SVG.Align_Start,
                            Position  => Pos_Inner,
                            Style     => "fill: none; stroke: red",
                            COID      => "C2")

               + Connector (Center, P (200, 30), P (320, 100),
                            Text      => "Inner/Center/CW",
                            Textstyle => "font-size: 10px; fill: blue; stroke: none",
                            Radius    => 15,
                            Align     => SCSC.SVG.Align_Centered,
                            Position  => Pos_Inner,
                            Style     => "fill: none; stroke: red",
                            COID      => "C3")

               + Connector (Center, P (340, 130), P (380, 240),
                            Text      => "Inner/End/CW",
                            Textstyle => "font-size: 10px; fill: blue; stroke: none",
                            Radius    => 15,
                            Align     => SCSC.SVG.Align_End,
                            Position  => Pos_Inner,
                            Style     => "fill: none; stroke: red",
                            COID      => "C4")

               + Connector (Center, P (360, 250), P (270, 330),
                            Text      => "Inner/Start/CCW",
                            Textstyle => "font-size: 10px; fill: blue; stroke: none",
                            Radius    => 15,
                            Align     => SCSC.SVG.Align_Start,
                            Direction => Dir_CCW,
                            Position  => Pos_Inner,
                            Style     => "fill: none; stroke: red",
                            COID      => "C5")

               + Connector (Center, P (250, 340), P (140, 330),
                            Text      => "Inner/Center/CCW",
                            Textstyle => "font-size: 10px; fill: blue; stroke: none",
                            Radius    => 25,
                            Align     => SCSC.SVG.Align_Centered,
                            Direction => Dir_CCW,
                            Position  => Pos_Inner,
                            Style     => "fill: none; stroke: red",
                            COID      => "C6")

               + Connector (Center, P (140, 280), P (80, 130),
                            Text      => "Inner/End/CCW",
                            Textstyle => "font-size: 10px; fill: blue; stroke: none",
                            Radius    => 15,
                            Align     => SCSC.SVG.Align_End,
                            Direction => Dir_CCW,
                            Position  => Pos_Inner,
                            Style     => "fill: none; stroke: red",
                            COID      => "C7")
               ,

       Defs   => Marker (Element => Arrow_End,   Width => 4, Height => 4, RefX => 0.1, RefY => 2.0, MID => "Arrow_End")
               + Marker (Element => Arrow_Start, Width => 4, Height => 4, RefX => 0.1, RefY => 2.0, MID => "Arrow_Start")
      );
begin
   Put_Line (SCSC.SVG.To_String (SVG));
end Main;

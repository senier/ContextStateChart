with Ada.Text_IO; use Ada.Text_IO;
with SCSC.SVG;
with SCSC.Primitives;
with SCSC.Types;

procedure Main
is
   use SCSC.Primitives;
   use SCSC.SVG;
   use SCSC.Types;

   Center  : Point := (200, 200);

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

   SVG : SCSC.SVG.Document_Type := SCSC.SVG.SVG
      (
       Width  => 400,
       Height => 400,

       Child  => Circle (Center,   2, Style => "fill: black; stroke: none")
               + Circle (Center, 200, Style => "fill: none; stroke: black")

               + Connector (Center, (56, 300), (68, 116),
                            Text      => "Inner/Default",
                            Textstyle => "font-size: 10px; fill: blue; stroke: none",
                            Radius    => 10,
                            Position  => Pos_Inner,
                            Style     => "fill: none; stroke: red")

               + Connector (Center, (65, 100), (180, 50),
                            Text      => "Inner/Start/CW",
                            Textstyle => "font-size: 10px; fill: blue; stroke: none",
                            Radius    => 20,
                            Align     => SCSC.SVG.Align_Start,
                            Position  => Pos_Inner,
                            Style     => "fill: none; stroke: red")

               + Connector (Center, (200, 30), (320, 100),
                            Text      => "Inner/Center/CW",
                            Textstyle => "font-size: 10px; fill: blue; stroke: none",
                            Radius    => 15,
                            Align     => SCSC.SVG.Align_Centered,
                            Position  => Pos_Inner,
                            Style     => "fill: none; stroke: red")

               + Connector (Center, (340, 130), (380, 240),
                            Text      => "Inner/End/CW",
                            Textstyle => "font-size: 10px; fill: blue; stroke: none",
                            Radius    => 15,
                            Align     => SCSC.SVG.Align_End,
                            Position  => Pos_Inner,
                            Style     => "fill: none; stroke: red")

               + Connector (Center, (360, 250), (270, 330),
                            Text      => "Inner/Start/CCW",
                            Textstyle => "font-size: 10px; fill: blue; stroke: none",
                            Radius    => 15,
                            Align     => SCSC.SVG.Align_Start,
                            Direction => Dir_CCW,
                            Position  => Pos_Inner,
                            Style     => "fill: none; stroke: red")

               + Connector (Center, (250, 340), (140, 330),
                            Text      => "Inner/Center/CCW",
                            Textstyle => "font-size: 10px; fill: blue; stroke: none",
                            Radius    => 25,
                            Align     => SCSC.SVG.Align_Centered,
                            Direction => Dir_CCW,
                            Position  => Pos_Inner,
                            Style     => "fill: none; stroke: red")

               + Connector (Center, (140, 280), (80, 130),
                            Text      => "Inner/End/CCW",
                            Textstyle => "font-size: 10px; fill: blue; stroke: none",
                            Radius    => 15,
                            Align     => SCSC.SVG.Align_End,
                            Direction => Dir_CCW,
                            Position  => Pos_Inner,
                            Style     => "fill: none; stroke: red")
               ,

       Defs   => Marker (Element => Arrow_End,   Width => 4, Height => 4, RefX => 0.1, RefY => 2.0, ID => "Arrow_End")
               + Marker (Element => Arrow_Start, Width => 4, Height => 4, RefX => 0.1, RefY => 2.0, ID => "Arrow_Start")
      );
begin
   Put_Line (SCSC.SVG.To_String (SVG));
end Main;

Config
  { font = "Bitstream Vera Sans Mono:size=16:antialias=true"
  , bgColor = "#000000"
  , fgColor = "#cccccc"
  , position = Top
  , lowerOnStart = True
  , pickBroadest = False
  , allDesktops = True
  , commands =
      [ Run XMonadLog
      , Run Date
            "%Y-%m-%d <fc=#aa00aa>//</fc> <fc=#ffffff>%H:%M</fc>"
            "date"
            10
      , Run Com "/bin/sh" ["-c","/home/sandy/.tino/bin/spotify-details"] "music" 5
      , Run Battery
          [ "--template" , "<acstatus>"
          , "--Low"      , "10"
          , "--High"     , "75"
          , "--low"      , "#ffffff"
          , "--normal"   , "#ffffff"
          , "--high"     , "#ffffff"

          , "--"
          , "-o"	, "<left> <fc=#aa00aa>//</fc> <fc=#ffffff>0<timeleft></fc>"
          , "-O"	, "<left> <fc=#aa00aa>//</fc> <fc=#ffffff>Charging</fc>"
          , "-i"	, "<fc=#006000>Charged</fc>"
          ] 50
      ]
  , sepChar = "%"
  , alignSep = "}{"
  , template = "   %XMonadLog% }{ %music% <fc=#aa00aa>//</fc> %battery% <fc=#aa00aa>//</fc> <fc=#ffffff>%date%</fc>   "
  }

Config
  { font = "Bitstream Vera Sans Mono:size=16:antialias=true"
  , bgColor = "#555555"
  , fgColor = "#cccccc"
  , position = Static
      { xpos = 0
      , ypos = 0
      , width = 1920
      , height = 16
      }
  , lowerOnStart = True,
  , commands =
      [ Run Date
            "%m-%d <fc=#aa00aa>//</fc> <fc=#ffffff>%H:%M</fc>"
            "date"
            10
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
      , Run StdinReader
      ]
  , sepChar = "%"
  , alignSep = "}{"
  , template = "%StdinReader% }{ %battery% <fc=#aa00aa>//</fc> <fc=#ffffff>%date%</fc>   "
  }

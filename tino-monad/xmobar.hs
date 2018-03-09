-- xmobar config used by Vic Fryzel
-- Author: Vic Fryzel
-- http://github.com/vicfryzel/xmonad-config

-- This is setup for dual 1920x1080 monitors, with the right monitor as primary
Config {
    font = "-misc-fixed-*-*-*-*-9-*-*-*-*-*-*-*",
    bgColor = "#000000",
    fgColor = "#ffffff",
    position = Static { xpos = 0, ypos = 0, width = 1600, height = 11 },
    lowerOnStart = True,
    commands = [
        Run Weather "EYVI" ["-t","<tempC>C <skyCondition>","-L","64","-H","77","-n","#CEFFAC","-h","#FFB6B0","-l","#96CBFE"] 36000,
        Run MultiCpu ["-t","Cpu: <total0> <total1> <total2> <total3>","-L","30","-H","60","-h","#FFB6B0","-l","#CEFFAC","-n","#FFFFCC","-w","3"] 10,
        Run Memory ["-t","Mem: <usedratio>%","-H","8192","-L","4096","-h","#FFB6B0","-l","#CEFFAC","-n","#FFFFCC"] 10,
        Run Swap ["-t","Swap: <usedratio>%","-H","1024","-L","512","-h","#FFB6B0","-l","#CEFFAC","-n","#FFFFCC"] 10,
        Run Network "wlp3s0" ["-t","Net: <rx> <tx>","-H","200","-L","10","-h","#FFB6B0","-l","#CEFFAC","-n","#FFFFCC"] 10,
        Run Date "%a %Y-%m-%d <fc=#daa520>%H:%M</fc>" "date" 10,
        Run Battery [ "--template" , "<acstatus>"
                    , "--Low"      , "10"        -- units: %
                    , "--High"     , "75"        -- units: %
                    , "--low"      , "red"
                    , "--normal"   , "#ff8800"
                    , "--high"     , "#009900"

                    , "--" -- battery specific options
                    -- discharging status
                    , "-o"	, "<left>% <fc=#daa520>0<timeleft></fc>"
                    -- AC "on" status
                    , "-O"	, "<left>% <fc=#dAA520>Charging</fc>"
                    -- charged status
                    , "-i"	, "<fc=#006000>Charged</fc>"
                    ] 50,
        -- Run Volume "default" "Master" [] 10,
        Run StdinReader
    ],
    sepChar = "%",
    alignSep = "}{",
    template = "%StdinReader% }{ %EYVI%   %battery%   %memory%   %wlp3s0%   <fc=#00aacc>%date%</fc>   " --   |   Volume: <fc=#b2b2ff>%default:Master%</fc>"
}

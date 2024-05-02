Config { overrideRedirect = False 
       , allDesktops = True
       , font = "xft:CaskaydiaMono Nerd Font:size=12:antialias=true" 
       , bgColor = "#181819" 
       , fgColor = "#e2e2e3" 
       , position = TopHM 30 100 20 15 5
       , commands = [ Run Weather "EGPF" [ "--template"
                                         , "<weather> <tempC>°C" 
                                         , "-L", "0" 
                                         , "-H", "25" , "--low" , "lightblue" 
                                         , "--normal", "#f8f8f2" , "--high" , "red"
                                         ] 36000 
                    , Run Cpu [ "-L"
                              , "3"
                              , "-H"
                              , "50" , "--high" , "red" 
                              , "--normal", "green" ] 10
                    ,Run Memory ["--template", "Mem: <usedratio>%"] 10 
                    , Run Swap [] 10 
                    , Run Date "<fc=#B48EAD>  %d-%m-%Y %H:%M:%S</fc>" "date" 10 
                    , Run Kbd [("us(alt-intl)", "<fc=#85d3f2>   us-intl</fc>")]
                    , Run Volume "default" "Master" [] 10
                    , Run XMonadLog 
                    ]
             , sepChar = "%" 
             , alignSep = "}{" 
             , template = "%XMonadLog% }{ %default:Master% %date% "
        }

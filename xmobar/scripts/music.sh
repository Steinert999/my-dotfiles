#! /bin/bash
#
# Writes the title and artist of the song currently being played by MPD or MOC to STDOUT, formatted for xmobar
# If MPD is playing a song or is paused, its information will be written. If not, MOC will be checked similarly.
# If neither are playing a song or are paused, nothing will be written.
# Note: if MPD isn't playing some errors will be written to STDERR; don't worry - xmobar only looks at STDOUT

TITLE_COLOR="#85d3f2"         # The colour to be used to draw the song title when playing
ARTIST_COLOR="#f39660"    # The colour to be used to draw the song artist when playing
PAUSE_COLOR="#e6e6e5"      # The colour to be used to draw both the song title and artist when paused
MENU_COLOR="#b39df3"

MPDSTATE=$(mpc | sed -e '2 !d' -e 's/^.*\[//' -e 's/\].*$//')

# buttons
pause_button='<action=`mpc pause` button=1></action>'
play_button='<action=`mpc play` button=1></action>'
previous_button='<action=`mpc prev` button=1>󰒮</action>'
next_button='<action=`mpc next` button=1>󰒭</action>'
if [ $MPDSTATE == "playing" ]; then
  # MPD is playing
  menu="<fc=$MENU_COLOR>$previous_button $pause_button $next_button</fc>"
  current_song="$(mpc current | sed 's/\ \[.*//')"
  artist="<fc=$ARTIST_COLOR>$(echo $current_song | sed 's/ -.*//')</fc>"
  title="<fc=$TITLE_COLOR>$(echo $current_song | sed 's/[^-]*- //')</fc>"

  echo "$menu $artist - $title"
  #echo "<fc=$ARTIST_COLOR>$(mpc current | sed 's/\ \[.*//' |  sed "s/ - /\<\/fc\> - \<fc=$TCOL>/")</fc>"
elif [ $MPDSTATE == "paused" ]; then
  # MPD is paused
  menu="<fc=$MENU_COLOR>$previous_button $play_button $next_button</fc>"

  echo "$menu <fc=$PAUSE_COLOR>$(mpc current | sed 's/\ \[.*//')</fc>"
fi

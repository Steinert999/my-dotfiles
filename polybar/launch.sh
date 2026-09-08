#!/usr/bin/env bash

## Copyright (C) 2020-2024 Aditya Shakya <adi1090x@gmail.com>

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
CARD="$(light -L 2>/dev/null | grep 'backlight' | head -n1 | cut -d'/' -f3)"
BAT="$(acpi -b 2>/dev/null)"
RFILE="$DIR/.module"

# Fix backlight and battery modules if needed
fix_modules() {
	if [[ -z "$CARD" ]]; then
		sed -i -e 's/backlight/bna/g' "$DIR"/config.ini
	elif [[ "$CARD" != *"intel_"* ]]; then
		sed -i -e 's/backlight/brightness/g' "$DIR"/config.ini
	fi

	if [[ -z "$BAT" ]]; then
		sed -i -e 's/battery/btna/g' "$DIR"/config.ini
	fi
}

# Launch the three pill bars on each monitor
launch_bar() {
	killall -q polybar
	while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

	for mon in $(polybar --list-monitors | cut -d":" -f1); do
		MONITOR=$mon polybar -q left   -c "$DIR"/config.ini &
		MONITOR=$mon polybar -q center -c "$DIR"/config.ini &
		MONITOR=$mon polybar -q right  -c "$DIR"/config.ini &
	done
}

if [[ ! -f "$RFILE" ]]; then
	fix_modules
	touch "$RFILE"
fi

launch_bar

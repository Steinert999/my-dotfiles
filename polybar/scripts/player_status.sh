#!/usr/bin/env bash
status=$(playerctl status 2>/dev/null)
if [ "$status" = "Playing" ]; then
    printf "\uf04c"   # nf-fa-pause
else
    printf "\uf04b"   # nf-fa-play
fi

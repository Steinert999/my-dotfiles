#!/bin/sh
updates=$(checkupdates 2> /dev/null | wc -l )

if [ "$updates" -gt 0 ]; then
    message="<fc=#f39660>󰚰 $updates esperando atualização</fc>"
else
    message="<fc=#a7df78> tudo atualizado</fc>"
fi
    echo $message

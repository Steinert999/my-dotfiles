#!/usr/bin/env bash
#
# Mostra o source de áudio padrão ativo (nome curto) via PipeWire - otimizado
#

# Cache de 3 segundos para reduzir chamadas ao pw-metadata
CACHE_FILE="/tmp/polybar_audio_input_cache"
CACHE_DURATION=3

# Verifica cache
if [[ -f "$CACHE_FILE" ]]; then
    CACHE_TIME=$(stat -c %Y "$CACHE_FILE" 2>/dev/null || echo 0)
    CURRENT_TIME=$(date +%s)
    AGE=$((CURRENT_TIME - CACHE_TIME))
    
    if [[ $AGE -lt $CACHE_DURATION ]]; then
        cat "$CACHE_FILE"
        exit 0
    fi
fi

# Execução real
current_name=$(pw-metadata 2>/dev/null | awk -F"'" \
    '/default\.audio\.source/ { match($4, /"name":"([^"]+)"/, a); print a[1] }')

shorten() {
    case "$1" in
        *"usb"*|*"USB"*)    echo "USB" ;;
        *"pci"*|*"analog"*) echo "Analog" ;;
        bluez_input.*)      echo "BT" ;;
        *)                  echo "${1:0:10}" ;;
    esac
}

shorten "$current_name" | tee "$CACHE_FILE"

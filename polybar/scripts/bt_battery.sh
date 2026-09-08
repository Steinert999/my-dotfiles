#!/usr/bin/env bash
#
# Mostra a porcentagem de bateria de dispositivos Bluetooth conectados
# nf-fa-battery_full = U+F240
# nf-fa-battery_three_quarters = U+F241
# nf-fa-battery_half = U+F242
# nf-fa-battery_quarter = U+F243
# nf-fa-battery_empty = U+F244
# nf-md-battery_arrow_down (descarregando) = U+F105E

# Cache de 10 segundos para reduzir chamadas ao bluetoothctl
CACHE_FILE="/tmp/polybar_bt_battery_cache"
CACHE_DURATION=10

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

# Verifica se bluetooth está ligado
if ! bluetoothctl show | grep -q "Powered: yes"; then
    echo "" | tee "$CACHE_FILE"
    exit 0
fi

# Pega devices pareados
bt_version=$(bluetoothctl version 2>/dev/null | awk '{print $2}' | head -n1)
if [ -n "$bt_version" ] && echo "$bt_version < 5.65" | bc -l 2>/dev/null | grep -q 1; then
    paired_cmd="paired-devices"
else
    paired_cmd="devices Paired"
fi

mapfile -t paired < <(bluetoothctl $paired_cmd 2>/dev/null | grep Device | cut -d ' ' -f 2)

# Procura por device conectado com bateria
for mac in "${paired[@]}"; do
    if bluetoothctl info "$mac" 2>/dev/null | grep -q "Connected: yes"; then
        # Tenta pegar a bateria via bluetoothctl
        battery=$(bluetoothctl info "$mac" 2>/dev/null | grep "Battery Percentage" | awk -F'[()]' '{print $2}' | tr -d '%')
        
        # Se não conseguir via bluetoothctl, tenta via upower
        if [[ -z "$battery" ]]; then
            # Converte MAC para formato do upower
            upower_path=$(upower -e | grep -i "$(echo $mac | tr ':' '_')")
            if [[ -n "$upower_path" ]]; then
                battery=$(upower -i "$upower_path" 2>/dev/null | grep "percentage" | awk '{print $2}' | tr -d '%')
            fi
        fi
        
        if [[ -n "$battery" ]]; then
            # Escolhe ícone baseado no nível da bateria
            if [[ $battery -ge 90 ]]; then
                icon=""
            elif [[ $battery -ge 60 ]]; then
                icon=""
            elif [[ $battery -ge 40 ]]; then
                icon=""
            elif [[ $battery -ge 20 ]]; then
                icon=""
            else
                icon=""
            fi
            
            # Gradiente de cor: 100% verde (#9ed072) -> 20% vermelho (#fc5d7c)
            # Interpolação RGB baseada na porcentagem
            if [[ $battery -ge 80 ]]; then
                color="#9ed072"  # Verde
            elif [[ $battery -ge 60 ]]; then
                color="#a8d877"  # Verde-amarelado
            elif [[ $battery -ge 40 ]]; then
                color="#d4c47f"  # Amarelo
            elif [[ $battery -ge 20 ]]; then
                color="#e89a7b"  # Laranja
            else
                color="#fc5d7c"  # Vermelho
            fi
            
            # Dispositivo Bluetooth conectado está sempre descarregando
            discharging_icon="󱁞"

            echo "%{F$color}$icon $discharging_icon $battery%%{F-}" | tee "$CACHE_FILE"
            exit 0
        fi
    fi
done

# Nenhum device com bateria conectado
echo "" | tee "$CACHE_FILE"

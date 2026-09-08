#!/usr/bin/env bash
#
# Status bluetooth otimizado para polybar
# Retorna: nome do device conectado, "On" se ligado sem device, ou "Off"
#

# Cache de 2 segundos para reduzir chamadas ao bluetoothctl
CACHE_FILE="/tmp/polybar_bt_cache"
CACHE_DURATION=2

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

# Execução real (apenas se cache expirou)
power_on() {
    bluetoothctl show | grep -q "Powered: yes"
}

device_connected() {
    bluetoothctl info "$1" 2>/dev/null | grep -q "Connected: yes"
}

if ! power_on; then
    echo "Off" | tee "$CACHE_FILE"
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

# Verifica conexões
connected_names=()
for mac in "${paired[@]}"; do
    if device_connected "$mac"; then
        alias=$(bluetoothctl info "$mac" 2>/dev/null | grep "Alias" | cut -d ' ' -f 2-)
        connected_names+=("$alias")
    fi
done

# Gera output e salva no cache
if [[ ${#connected_names[@]} -eq 0 ]]; then
    echo "On" | tee "$CACHE_FILE"
elif [[ ${#connected_names[@]} -eq 1 ]]; then
    echo "${connected_names[0]}" | tee "$CACHE_FILE"
else
    echo "${#connected_names[@]} devices" | tee "$CACHE_FILE"
fi

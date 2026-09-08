#!/usr/bin/env bash

## Weather for polybar
## Mostra a temperatura atual e a condição da regiao (detectada por IP via wttr.in)
## Ícones Nerd Font (nf-weather / nf-md)

# Cache de 15 minutos para nao sobrecarregar a API
CACHE_FILE="/tmp/polybar_weather_cache"
CACHE_DURATION=900

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

# Busca dados do wttr.in (regiao detectada automaticamente pelo IP)
data=$(curl -s --max-time 8 'https://wttr.in/?format=j1' 2>/dev/null)

# Sem rede / falha: mantem o ultimo cache se existir, senao mostra nada
if [[ -z "$data" ]]; then
    if [[ -f "$CACHE_FILE" ]]; then
        cat "$CACHE_FILE"
    else
        echo ""
    fi
    exit 0
fi

temp=$(echo "$data" | jq -r '.current_condition[0].temp_C' 2>/dev/null)
code=$(echo "$data" | jq -r '.current_condition[0].weatherCode' 2>/dev/null)

if [[ -z "$temp" || "$temp" == "null" ]]; then
    if [[ -f "$CACHE_FILE" ]]; then
        cat "$CACHE_FILE"
    else
        echo ""
    fi
    exit 0
fi

# Mapeia weatherCode (WWO) para um icone Nerd Font (nf-weather)
case "$code" in
    113)                    icon="";;                 # ceu limpo / sol
    116)                    icon="";;                 # parcialmente nublado
    119|122)                icon="";;                 # nublado
    143|248|260)            icon="";;                 # neblina
    176|263|266|293|296|353) icon="";;               # chuva fraca
    299|302|305|308|356|359) icon="";;               # chuva forte
    179|182|185|227|230|317|320|323|326|368|371|374|377) icon="";; # neve
    200|386|389|392|395)    icon="";;                 # trovoada
    *)                      icon="";;
esac

echo "$icon ${temp}°C" | tee "$CACHE_FILE"

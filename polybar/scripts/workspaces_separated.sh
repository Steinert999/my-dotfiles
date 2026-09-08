#!/usr/bin/env bash
#
# Mostra workspaces independentes por tela com separador visual
# 4 workspaces por tela (total 8: 0_1..0_4 | 1_1..1_4)
#

# Cores
pink="#b39df3"      # workspace ativo
green="#9ed072"     # workspace ocupado
gray="#3d4555"      # workspace vazio
red="#fc5d7c"       # workspace urgente
sep_color="#76cce0" # separador

# Símbolos
filled="●"
empty="○"
sep="%{F${sep_color}}%{T3}│%{T-}%{F-} "

# Total de workspaces por tela
ws_per_screen=4

# Obtém workspace atual via wmctrl
current=$(wmctrl -d 2>/dev/null | awk '$2 == "*" {print $1}')

# Obtém lista de workspaces com janelas (exclui -1 que são janelas sticky)
occupied=$(wmctrl -l 2>/dev/null | awk '$2 >= 0 {print $2}' | sort -u | tr '\n' ' ')

output=""

# Tela 0 (workspaces 0-3: 0_1, 0_2, 0_3, 0_4)
for ((i=0; i<ws_per_screen; i++)); do
    if [[ $i -eq $current ]]; then
        symbol="%{F${pink}}${filled}%{F-}"
    elif echo "$occupied" | grep -qw "$i"; then
        symbol="%{F${green}}${filled}%{F-}"
    else
        symbol="%{F${gray}}${empty}%{F-}"
    fi
    
    output+="%{A1:xdotool key super+$((i+1)):}${symbol}%{A} "
done

# Separador
output+="$sep"

# Tela 1 (workspaces 4-7: 1_1, 1_2, 1_3, 1_4)
for ((i=ws_per_screen; i<ws_per_screen*2; i++)); do
    if [[ $i -eq $current ]]; then
        symbol="%{F${pink}}${filled}%{F-}"
    elif echo "$occupied" | grep -qw "$i"; then
        symbol="%{F${green}}${filled}%{F-}"
    else
        symbol="%{F${gray}}${empty}%{F-}"
    fi
    
    # Tecla correspondente (1-4 também, mas no monitor 1)
    key=$((i - ws_per_screen + 1))
    output+="%{A1:xdotool key super+${key}:}${symbol}%{A} "
done

echo "$output"

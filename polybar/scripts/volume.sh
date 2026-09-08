#!/usr/bin/env bash
# nf-fa-volume-up   = U+F028
# nf-fa-volume-off  = U+F026

# Get active sink (sink com streams ativos ou sink padrão)
get_active_sink() {
	# Primeiro tenta pegar sink com streams ativos
	active_sink=$(wpctl status 2>/dev/null | grep -A 1000 "Streams:" | grep "output_FL.*>" | head -1 | sed 's/.*> //' | sed 's/:.*//' | xargs)
	
	if [[ -n "$active_sink" ]]; then
		# Pega o ID do sink pelo nome (apenas da seção Sinks, antes de Sources)
		sink_id=$(wpctl status 2>/dev/null | sed -n '/Sinks:/,/Sources:/p' | grep "$active_sink" | grep "│" | awk '{print $2}' | sed 's/\..*//' | head -1)
		if [[ -n "$sink_id" ]] && [[ "$sink_id" =~ ^[0-9]+$ ]]; then
			echo "$sink_id"
			return
		fi
	fi
	
	# Fallback: sink padrão (marcado com *)
	sink_id=$(wpctl status 2>/dev/null | sed -n '/Sinks:/,/Sources:/p' | grep "\*" | awk '{print $3}' | sed 's/\..*//' | head -1)
	if [[ -n "$sink_id" ]] && [[ "$sink_id" =~ ^[0-9]+$ ]]; then
		echo "$sink_id"
	else
		echo "@DEFAULT_AUDIO_SINK@"
	fi
}

sink=$(get_active_sink)
info=$(wpctl get-volume "$sink" 2>/dev/null)
vol=$(echo "$info" | awk '{printf "%d", $2 * 100}')
muted=$(echo "$info" | grep -c "MUTED")

if [ "$muted" -gt 0 ]; then
    printf "\uf026 ---"
else
    printf "\uf028 %s%%" "$vol"
fi

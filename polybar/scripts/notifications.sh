#!/usr/bin/env bash

## Notifications for polybar (dunst)
## Mostra estado das notificacoes via dunstctl:
##   - sino normal quando ativas / sino cortado em "nao perturbe" (paused)
##   - contagem do historico (quantas notificacoes chegaram) ao lado do icone
## Cliques (configurados no modules.ini):
##   esquerdo -> alterna nao perturbe
##   direito  -> abre a lista do historico no rofi
##   meio     -> limpa o historico

BELL=""
BELL_SLASH=""

# Dunst nao esta rodando: nao mostra nada
if ! command -v dunstctl >/dev/null 2>&1; then
    echo ""
    exit 0
fi

# Contagem total: historico (ja exibidas) + em espera (fila enquanto pausado)
history=$(dunstctl count history 2>/dev/null)
waiting=$(dunstctl count waiting 2>/dev/null)
[[ -z "$history" ]] && history=0
[[ -z "$waiting" ]] && waiting=0
count=$((history + waiting))

paused=$(dunstctl is-paused 2>/dev/null)
if [[ "$paused" == "true" ]]; then
    icon="$BELL_SLASH"
else
    icon="$BELL"
fi

if [[ "$count" -gt 0 ]]; then
    echo "$icon $count"
else
    echo "$icon"
fi

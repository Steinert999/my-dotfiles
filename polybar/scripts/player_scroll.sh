#!/usr/bin/env bash
# Script para exibir player com scroll automático quando texto é muito longo
# Usa timestamp para calcular posição do scroll (sem arquivo de estado)

# Configurações
MAX_LENGTH=30         # Tamanho máximo antes de começar a rolar
SCROLL_SPEED=1      # Caracteres por segundo

# Pega metadata do playerctl
if ! playerctl status 2>/dev/null | grep -qE "Playing|Paused"; then
    echo "Sem áudio em execução"
    exit 0
fi

ARTIST=$(playerctl metadata artist 2>/dev/null)
TITLE=$(playerctl metadata title 2>/dev/null)

# Se não conseguir pegar metadata, sai
if [[ -z "$ARTIST" && -z "$TITLE" ]]; then
    echo "Sem áudio em execução"
    exit 0
fi

# Monta o texto completo
if [[ -n "$ARTIST" && -n "$TITLE" ]]; then
    FULL_TEXT="$ARTIST - $TITLE"
elif [[ -n "$TITLE" ]]; then
    FULL_TEXT="$TITLE"
else
    FULL_TEXT="$ARTIST"
fi

TEXT_LENGTH=${#FULL_TEXT}

# Se o texto for menor que MAX_LENGTH, exibe normal
if [[ $TEXT_LENGTH -le $MAX_LENGTH ]]; then
    echo "$FULL_TEXT"
    exit 0
fi

# Texto longo - implementa scroll baseado em tempo
# Adiciona espaços suficientes para transição suave
SPACING=" ... "  # ~30 espaços
PADDED_TEXT="$FULL_TEXT$SPACING"
PADDED_LENGTH=${#PADDED_TEXT}

# Calcula offset baseado no tempo atual (milissegundos)
CURRENT_MS=$(date +%s%3N)
# Divide por velocidade para controlar quantos ms por caractere
MS_PER_CHAR=$((60 / SCROLL_SPEED))
OFFSET=$(( (CURRENT_MS / MS_PER_CHAR) % PADDED_LENGTH ))

# Cria o texto visível (loop circular)
VISIBLE=""
for ((i=0; i<MAX_LENGTH; i++)); do
    CHAR_INDEX=$(( (OFFSET + i) % PADDED_LENGTH ))
    VISIBLE="${VISIBLE}${PADDED_TEXT:$CHAR_INDEX:1}"
done

echo "$VISIBLE"

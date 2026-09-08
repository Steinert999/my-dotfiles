#!/usr/bin/env bash
uptime -p | sed 's/up //' | sed 's/ hours\{0,1\}/h/' | sed 's/ minutes\{0,1\}/m/' | sed 's/, / /'

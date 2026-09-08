#!/usr/bin/env bash

## Copyright (C) 2020-2026 Aditya Shakya <adi1090x@gmail.com>

# Calculate total storage usage across all partitions

calculate_storage() {
    # Use df with proper mount points (with spaces)
    df -BG / /home \
        "/run/media/steinert/NVME INTERNO" \
        "/run/media/steinert/SSD INTERNO 1" \
        "/run/media/steinert/SSD INTERNO 2" \
        "/run/media/steinert/HD INTERNO 1" \
        "/run/media/steinert/HD INTERNO 2" 2>/dev/null | tail -n +2 | awk '
    {
        gsub(/G/, "", $2)
        gsub(/G/, "", $3)
        gsub(/G/, "", $4)
        total_size += $2
        total_used += $3
        total_free += $4
    }
    END {
        if (total_size > 0) {
            percentage = int((total_used * 100) / total_size)
            
            # Convert to TB if > 1024GB
            if (total_free > 1024) {
                free_tb = total_free / 1024
                size_tb = total_size / 1024
                printf "%.1fT/%.1fT livre (%d%%)\n", free_tb, size_tb, percentage
            } else {
                printf "%dG/%dG livre (%d%%)\n", total_free, total_size, percentage
            }
        } else {
            print "N/A"
        }
    }'
}

calculate_storage

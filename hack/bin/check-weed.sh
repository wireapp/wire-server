#!/usr/bin/env bash

# Define ANSI color code for red
RED='\033[0;31m'
NC='\033[0m' # No Color (reset)

echo "Checking for weed…"
echo "Make sure you have compiled everything with the correct settings."

output=$(weeder -N)

# Check if the output is empty
if [[ -z "$output" ]]; then
    echo "No weed found! 🚫🪴"
else
    echo "We found some weed!"
    echo -e "${RED}$output${NC}"
fi

#!/bin/sh
# to be assimilated

np=$(consul kv get now-playing | jq -r '.artist? + .series? + " - " + .title')
echo $np
notify-send -t 10000 "$np"

#!/usr/bin/env bash

[ -z "$MONITORING_DELAY" ] && MONITORING_DELAY="30"

url="${SERVER_URL//\//\\/}"
sed -i "s/Elm.Main.fullscreen({ url : \"\" });/Elm.Main.fullscreen({ url : \"$url\" });/" static/index.html

monitor -m -p ./checks --delay $MONITORING_DELAY

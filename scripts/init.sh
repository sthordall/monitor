#!/usr/bin/env bash

url="${SERVER_URL//\//\\/}"
sed -i "s/Elm.Main.fullscreen({ url : \"\" });/Elm.Main.fullscreen({ url : \"$url\" });/" static/index.html

monitor -m -p ./checks

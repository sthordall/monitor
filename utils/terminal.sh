#!/usr/bin/env bash

command -v xfce4-terminal >/dev/null 2>&1 || { echo >&2 "[xfce4-terminal] is required, but not installed.  Aborting."; exit 1; }

if [[ "$#" -ne 1 ]]; then
  echo "ERROR: Expected arguments are missing: CMD"
  exit 1
fi

cmd=$1

xfce4-terminal --geometry 100x40 -e "${cmd:-top}"

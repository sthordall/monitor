#!/usr/bin/env bash

command -v find >/dev/null 2>&1 || { echo >&2 "[find] is required, but not installed.  Aborting."; exit 1; }
command -v printf >/dev/null 2>&1 || { echo >&2 "[printf] is required, but not installed.  Aborting."; exit 1; }
command -v notify-send >/dev/null 2>&1 || { echo >&2 "[notify-send] is required, but not installed.  Aborting."; exit 1; }
command -v date >/dev/null 2>&1 || { echo >&2 "[date] is required, but not installed.  Aborting."; exit 1; }

ENABLE_FILE_LOGGING=0

FLAG=$1
ROOT="$(dirname "$(readlink -f "$0")")"
LOCK="$ROOT/lock"
CHECKS_OUTPUT="$ROOT/checks-output"
PASSED_OUTPUT="$ROOT/passed-output"
FAILED_OUTPUT="$ROOT/failed-output"
WARNED_OUTPUT="$ROOT/warned-output"
NOTIFY_TIME=$((5 * 1000))
IMAGE_SIZE=24

function print_timespan () {
  duration=$1
  min=$(($duration / 60))
  sec=$(($duration % 60))
  printf "%02d:%02d sec" $min $sec
}

function run_checks () {
  echo "" > $CHECKS_OUTPUT
  checks=($(find "$ROOT/../checks" -type f -iregex ".*\.sh$"))
  offset=$((${#ROOT} + 8))
  failed_checks=0
  warned_checks=0
  passed_checks=0
  for check in "${checks[@]}"; do
    SECONDS=0
    echo "# ${check:$offset} ..." >> $CHECKS_OUTPUT
    $check >> $CHECKS_OUTPUT
    rc=$?
    duration=$SECONDS
    if [ "$rc" -eq "0" ]; then
      passed_checks=$(($passed_checks + 1))
      echo " >> PASSED, took `print_timespan $duration`" >> $CHECKS_OUTPUT
    elif [ "$rc" -eq "1" ]; then
      failed_checks=$(($failed_checks + 1))
      echo " >> FAILED, took `print_timespan $duration`" >> $CHECKS_OUTPUT
    else
      warned_checks=$(($warned_checks + 1))
      echo " >> WARNED, took `print_timespan $duration`" >> $CHECKS_OUTPUT
    fi
    echo "" >> $CHECKS_OUTPUT
  done
  echo "$failed_checks" > $FAILED_OUTPUT
  echo "$warned_checks" > $WARNED_OUTPUT
  echo "$passed_checks" > $PASSED_OUTPUT
}

output=$(cat $CHECKS_OUTPUT 2> /dev/null)
passed=$(cat $PASSED_OUTPUT 2> /dev/null)
failed=$(cat $FAILED_OUTPUT 2> /dev/null)
warned=$(cat $WARNED_OUTPUT 2> /dev/null)
icon=$(if [ "${failed:=0}" -gt "0" ]; then echo "sick"; elif [ "${warned:=0}" -gt "0" ]; then echo "dizzy"; else echo "healthy"; fi)

function xnotify () {
  notify-send -t $NOTIFY_TIME -i "$ROOT/icons/$icon.png" "$1" "$2"
}

function start_watch_terminal () {
  ($ROOT/terminal.sh "watch -c \"$ROOT/test.sh\"" &)
}

function dump_history_log () {
  if [ "$ENABLE_FILE_LOGGING" -eq "1" ]; then
    dump=0
    if [ "$failed" -gt "0" ]; then
      mkdir -p "$ROOT/logs/$(date +%Y-%m-%d)" > /dev/null
      filename="$ROOT/logs/$(date +%Y-%m-%d)/$(date +%H%M%S)-failed.log"
      dump=1
    # elif [ "$warned" -gt "0" ]; then
    #   mkdir -p "$ROOT/logs/$(date +%Y-%m-%d)" > /dev/null
    #   filename="$ROOT/logs/$(date +%Y-%m-%d)/$(date +%H%M%S)-warned.log"
    #   dump=1
    fi
    if [ "$dump" -eq "1" ]; then
      printf "Failed: $failed\nWarned: $warned\nPassed: $passed\n\n$output" > $filename
    fi
  fi
}

( flock -x 200
if [ "$FLAG" == "-n" ]; then
  if [ "$failed" -gt "0" ]; then
    xnotify "Sick" "System has some failing checks"
  elif [ "$warned" -gt "0" ]; then
    xnotify "Dizzy" "System has some warning checks"
  else
    xnotify "Healthy" "System seams to be fully operational"
  fi
  $(start_watch_terminal)
else
  $(run_checks)
  echo "<click>$ROOT/monitor.sh -n</click>"
  echo "<tool>Failed: $failed; Warned: $warned; Passed: $passed</tool>"
  echo "<img>$ROOT/icons/$icon$IMAGE_SIZE.png</img>"
  $(dump_history_log)
fi
) 200> "$LOCK"

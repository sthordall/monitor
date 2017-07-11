#!/usr/bin/env bash

command -v find >/dev/null 2>&1 || { echo >&2 "[find] is required, but not installed.  Aborting."; exit 1; }
command -v printf >/dev/null 2>&1 || { echo >&2 "[printf] is required, but not installed.  Aborting."; exit 1; }

SHOW_PASSED=0

ROOT="$(dirname "$(readlink -f "$0")")"

RED='\033[1;31m'
YELLOW='\033[1;33m'
GREEN='\033[1;32m'
CYAN='\033[0;36m'
GRAY='\033[1;30m'
NC='\033[0m'

function format_passed () {
  line='--------------------------------------------------------------------------------'
  PROC_NAME=$1
  printf "${RED}#${NC} ${CYAN}%s${NC} ${GRAY}%s${NC} [${GREEN}PASSED${NC}]\n" $PROC_NAME "${line:${#PROC_NAME}}"
}

function format_warned () {
  line='--------------------------------------------------------------------------------'
  PROC_NAME=$1
  printf "${RED}#${NC} ${CYAN}%s${NC} ${GRAY}%s${NC} [${YELLOW}WARNED${NC}]\n" $PROC_NAME "${line:${#PROC_NAME}}"
}

function format_failed () {
  line='--------------------------------------------------------------------------------'
  PROC_NAME=$1
  printf "${RED}#${NC} ${CYAN}%s${NC} ${GRAY}%s${NC} [${RED}FAILED${NC}]\n" $PROC_NAME "${line:${#PROC_NAME}}"
}

failed_checks=0
warned_checks=0
passed_checks=0

function format_duration () {
  duration=$1
  min=$(($duration / 60))
  sec=$(($duration % 60))
  printf "  ${YELLOW}|${NC} ${CYAN}Took:${NC} ${YELLOW}%02d:%02d${NC} ${CYAN}sec${NC}\n" $min $sec
}

function format_output () {
  IFS='|' read -r -a output <<< "$1"
  for line in "${output[@]}"; do
    echo -e "  ${YELLOW}|${NC} ${GRAY}$line${NC}"
  done
}

echo ""
checks=($(find "$ROOT/../checks" -type f -iregex ".*\.sh$"))
offset=$((${#ROOT} + 8))
for check in "${checks[@]}"; do
  SECONDS=0
  output=$($check)
  rc=$?
  duration=$SECONDS
  output=$(echo "${output[0]}" | sed ':a;N;$!ba;s/\n/|/g')
  if [ "$rc" -eq "0" ]; then
    passed_checks=$(($passed_checks + 1))
    if [ "$SHOW_PASSED" -eq "1" ]; then
      format_passed "${check:$offset}"
      format_duration $duration
    fi
  elif [ "$rc" -eq "1" ]; then
    failed_checks=$(($failed_checks + 1))
    format_failed "${check:$offset}"
    format_output "$output"
    format_duration $duration
  else
    warned_checks=$(($warned_checks + 1))
    format_warned "${check:$offset}"
    format_output "$output"
    format_duration $duration
  fi
done

echo ""
echo -e "${GRAY}Failed:${NC} ${RED}$failed_checks${NC}"
echo -e "${GRAY}Warned:${NC} ${YELLOW}$warned_checks${NC}"
echo -e "${GRAY}Passed:${NC} ${GREEN}$passed_checks${NC}"
echo ""

if [ "$failed_checks" -gt "0" ]; then
  exit 1
elif [ "$warned_checks" -gt "0" ]; then
  exit 2
fi

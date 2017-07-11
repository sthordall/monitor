#!/usr/bin/env bash

command -v curl >/dev/null 2>&1 || { echo >&2 "[curl] is required, but not installed.  Aborting."; exit 1; }
command -v jq >/dev/null 2>&1 || { echo >&2 "[jq] is required, but not installed.  Aborting."; exit 1; }
command -v sed >/dev/null 2>&1 || { echo >&2 "[sed] is required, but not installed.  Aborting."; exit 1; }
command -v sort >/dev/null 2>&1 || { echo >&2 "[sort] is required, but not installed.  Aborting."; exit 1; }

ROOT="$(dirname "$(readlink -f "$0")")"
NAME=$(basename "$0")

if [ -z "$RABBITMQ_ADDRESS" ]; then
  echo "RabbitMQ address is not set"
  exit 1
fi

if [ -z "$RABBITMQ_CREDS" ]; then
  echo "RabbitMQ credentials are not set"
  exit 1
fi

ADDRESS=$RABBITMQ_ADDRESS
CREDS=$RABBITMQ_CREDS
CURL_MAX_TIME=5

function trim_quotes () {
  echo $1 | sed 's/^\"//' | sed 's/\"$//'
}

result=$(curl --max-time $CURL_MAX_TIME --fail --fail-early -sb -i -u $CREDS "$ADDRESS/api/nodes")
rc=$?
if [ ! "$rc" -eq "0" ]; then
  echo "Server seams to be offline"
  exit 1
fi

NODES=($(echo $result | \
  jq -c '.[] | "!", .name, .mem_alarm, .mem_used, .mem_limit' | \
  sed ':a;N;$!ba;s/\n/ /g' | \
  sed 's/ /=/g' | \
  sed 's/^\"\!\"=//g' | \
  sed 's/=\"\!\"=/\n/g'))

# Example output:
# "rabbit@rabbit-1"=false=105953184=3281149952
# "rabbit@rabbit-2"=false=1522663272=3281149952
# "rabbit@rabbit-3"=false=1377115576=3281149952

if [ "${#NODES}" -eq "0" ]; then
  echo "No nodes found"
  exit 1
fi

alarm_detected=0
for line in "${NODES[@]}"; do
  IFS='=' read -r -a xs <<< "$line"
  node=$(trim_quotes "${xs[0]}")
  alarm="${xs[1]}"
  used="${xs[2]}"
  limit="${xs[3]}"
  if [ "$alarm" != "false" ]; then
    alarm_detected=1
    echo "Node [$node] has memory alarm (used / limit): $used / $limit"
  fi
done

if [ "$alarm_detected" -eq "1" ]; then
  exit 1
fi

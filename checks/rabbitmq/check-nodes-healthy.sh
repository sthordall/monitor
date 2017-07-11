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

NODES=($(echo $result | jq '.[] | .name'))

if [ "${#NODES}" -eq "0" ]; then
  echo "No nodes found"
  exit 1
fi

failing_nodes=0
for node in "${NODES[@]}"; do
  node=$(trim_quotes $node)
  result=$(curl --max-time $CURL_MAX_TIME --fail --fail-early -sb -i -u $CREDS "$ADDRESS/api/healthchecks/node/$node")
  rc=$?
  if [ ! "$rc" -eq "0" ]; then
    echo "Server seams to be offline"
    exit 1
  fi
  xs=($(echo $result | jq '.status, .reason'))
  status="${xs[0]}"
  reason="${xs[@]:1}"
  if [ "$status" != "\"ok\"" ]; then
    echo "Node [$node] failing: $reason"
    failing_nodes=$((failing_nodes + 1))
  fi
done

if [ "$failing_nodes" -gt "0" ]; then
  exit 1
fi

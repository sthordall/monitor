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
  jq -c '.[] | .name, .partitions' | \
  sed ':a;N;$!ba;s/\n/ /g' | \
  sed 's/\] /\]\n/g' | \
  sed 's/ /=/g' | \
  sed 's/\[//g' | \
  sed 's/\]//g'))

# Example output (with network partition detected):
# "rabbit@rabbit-1"="rabbit@rabbit-3"
# "rabbit@rabbit-2"=
# "rabbit@rabbit-3"="rabbit@rabbit-1"

# Example output (without network partition):
# "rabbit@rabbit-1"=
# "rabbit@rabbit-2"=
# "rabbit@rabbit-3"=

if [ "${#NODES}" -eq "0" ]; then
  echo "No nodes found"
  exit 1
fi

partition_detected=0
for line in "${NODES[@]}"; do
  IFS='=' read -r -a xs <<< "$line"
  node=$(trim_quotes "${xs[0]}")
  ns="${xs[1]}"
  if [ ! -z "$ns" ]; then
    partition_detected=1
    IFS=',' read -r -a pnodes <<< "$ns"
    echo "Node [$node] was partitioned from:"
    for pnode in "${pnodes[@]}"; do
      pnode=$(trim_quotes "$pnode")
      echo "  * $pnode"
    done
  fi
done

if [ "$partition_detected" -ne "0" ]; then
  exit 1
fi

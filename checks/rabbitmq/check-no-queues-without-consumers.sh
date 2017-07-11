#!/usr/bin/env bash

command -v curl >/dev/null 2>&1 || { echo >&2 "[curl] is required, but not installed.  Aborting."; exit 1; }
command -v jq >/dev/null 2>&1 || { echo >&2 "[jq] is required, but not installed.  Aborting."; exit 1; }
command -v sed >/dev/null 2>&1 || { echo >&2 "[sed] is required, but not installed.  Aborting."; exit 1; }
command -v sort >/dev/null 2>&1 || { echo >&2 "[sort] is required, but not installed.  Aborting."; exit 1; }

ROOT="$(dirname "$(readlink -f "$0")")"
NAME=$(basename "$0")
INPUT_FILE="$ROOT/${NAME%.*}.input"
INPUT=($(cat $INPUT_FILE 2> /dev/null))

if [ -z "$INPUT" ]; then
  echo "Configuration for check is not set"
  exit 1
fi

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
IGNORE_QUEUES="${INPUT[0]}"
CURL_MAX_TIME=5

result=$(curl --max-time $CURL_MAX_TIME --fail --fail-early -sb -i -u $CREDS "$ADDRESS/api/queues")
rc=$?
if [ ! "$rc" -eq "0" ]; then
  echo "Server seams to be offline"
  exit 1
fi

QUEUES=($( \
  echo $result | \
  jq '.[] | .name, .consumers, .exclusive' | \
  sed ':a;N;$!ba;s/\n/ /g' | \
  sed -r 's/" ([0-9]+|null) (false|true)\s*/"=\1=\2\n/g' | \
  sort ))

errors=0
warnings=0
for queue in "${QUEUES[@]}"; do
  if [[ ! $queue =~ $IGNORE_QUEUES ]]; then
    IFS='=' read -r -a xs <<< "$queue"
    queue="${xs[0]}"
    if [ "$queue" != "null" ]; then
      n="${xs[1]}"
      exclusive="${xs[2]}"
      if [ "$n" == "null" -a "$exclusive" == "true" ]; then
        echo "Exclusive queue [$queue] has no consumers"
        warnings=$(($warnings + 1))
      else
        if [ "${n:-0}" -eq "0" ]; then
          echo "Queue [$queue] has no consumers"
          errors=$(($errors + 1))
        fi
      fi
    fi
  fi
done

if [ "$errors" -gt "0" ]; then
  exit 1
fi

if [ "$warnings" -gt "0" ]; then
  exit 2
fi

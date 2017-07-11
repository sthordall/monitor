#!/usr/bin/env bash

command -v docker >/dev/null 2>&1 || { echo >&2 "[docker] is required, but not installed.  Aborting."; exit 1; }
command -v awk >/dev/null 2>&1 || { echo >&2 "[awk] is required, but not installed.  Aborting."; exit 1; }
command -v curl >/dev/null 2>&1 || { echo >&2 "[curl] is required, but not installed.  Aborting."; exit 1; }
command -v jq >/dev/null 2>&1 || { echo >&2 "[jq] is required, but not installed.  Aborting."; exit 1; }
command -v sed >/dev/null 2>&1 || { echo >&2 "[sed] is required, but not installed.  Aborting."; exit 1; }

ROOT="$(dirname "$(readlink -f "$0")")"
NAME=$(basename "$0")
INPUT_FILE="$ROOT/${NAME%.*}.input"
INPUT=($(cat $INPUT_FILE 2> /dev/null))

if [ -z "$INPUT" ]; then
  echo "Configuration for check is not set"
  exit 1
fi

if [ -z "$DOCKER_HOST" ]; then
  echo "Docker host is not set"
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

function trim_quotes () {
  echo $1 | sed 's/^\"//' | sed 's/\"$//'
}

FILTER=$(trim_quotes ${INPUT[0]})
CURL_MAX_TIME=5

result=$(curl --max-time $CURL_MAX_TIME --fail --fail-early -sb -i -u $RABBITMQ_CREDS "$RABBITMQ_ADDRESS/api/connections")
rc=$?
if [ ! "$rc" -eq "0" ]; then
  echo "Server seams to be offline"
  exit 1
fi

connections=($(echo $result | jq '.[] | .client_properties | .connection_name'))

if [ "${#connections}" -eq "0" ]; then
  echo "No connections found"
  exit 1
fi

services=($(docker service ls | grep -v '^ID' | awk '{print $2"/"$4}' | grep $FILTER))

if [ "${#services}" -eq "0" ]; then
  echo "No services found"
  exit 1
fi

errors=0
for service in "${services[@]}"; do
  IFS='/' read -r -a xs <<< "$service"
  name="${xs[0]}"
  n="${xs[2]}"
  matching_connections=0
  for connection in "${connections[@]}"; do
    if [[ $connection =~ "\"$name" ]]; then
      matching_connections=$(($matching_connections + 1))
    fi
  done
  if [ "$matching_connections" -lt "$n" ]; then
    echo "Not all instances of [$name] service can connect to RabbitMQ ($matching_connections / $n)"
    errors=$(($errors + 1))
  fi
done

if [ "$errors" -gt 0 ]; then
  exit 1
fi

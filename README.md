# monitor

Simple extensible monitoring service exposing RESTful API.

The monitor is built as a docker image exposing `/checks` as a volume such that
it is possible to place any bash scripts that performs some arbitrary checks.
There is only one agreement that those scripts needs to obey - in case of a
success return code should always be `0`, in case of a warning - `2`, in case of
an error - any positive number different from `2`.

By default `./checks` folder is packaged with some checks involving docker swarm
cluster and RabbitMQ. For that `monitor` image allows to pass on following
environment variables that are used by these scripts:

* `DOCKER_HOST` (e.g. `127.0.0.1:2375`)
* `RABBITMQ_ADDRESS` (e.g. `http://127.0.0.1:15672`)
* `RABBITMQ_CREDS` (e.g. `guest:guest`)

## If you are happy using Nix or NixOS

If you are one of those lucky members of Nix/NixOS community, you can simple
build your docker image by:

```{.bash}
nix-build docker.nix
docker load < result
```

To run the image just:

```{.bash}
docker run --rm \
  -p 3000:3000 \
  -e DOCKER_HOST=127.0.0.1:2375 \
  -e RABBITMQ_ADDRESS=http://127.0.0.1:15672 \
  -e RABBITMQ_CREDS=guest:guest \
  monitor monitor -m --delay 5 -p .
```

This will run all the scripts in `/checks` volume (by default having check
scripts to verify the integrity of docker swarm and RabbitMQ clusters) with 5
seconds pause in between. It will also start simple RESTful API serving two
endpoints:

1. `/status` - returns JSON array of results of invocation of each and every
   script
2. `/health` - returns text representation of UTC timestamp indicating when
   checks have been performed last time (should update every 5 seconds). This
   part is not really RESTful as you might notice, but it is helpful to have an
   indication that service is kicking.

In case you would like to extend on the Haskell solution, just:

```{.bash}
nix-shell
```

> The development environment that is brought to you by `shell.nix` (implicitly
> used by `nix-shell`) contains some helper packages that will allow you to use
> you Vim editor as pretty good Haskell IDE.

## Example of starting in a swarm cluster

```{.bash}
$ docker service create \
    --name monitor \
    --network net \
    --replicas 1 \
    -p 3000:3000 \
    --mount type=bind,source=/var/run/docker.sock,target=/var/run/docker.sock \
    -e DOCKER_HOST=unix:///var/run/docker.sock \
    -e RABBITMQ_ADDRESS=http://rabbit-3:15672 \
    -e RABBITMQ_CREDS=guest:guest \
    -e RABBITMQ_CONNECTOR_INFO=rabbit-1:5672\|rabbit-2:5672\|rabbit-3:5672\|\|/\|\|guest:guest \
    monitor
```

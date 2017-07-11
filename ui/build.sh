#!/usr/bin/env bash

target=build

mkdir -p $target

elm make src/Main.elm  --output $target/js/app.js --yes

cp src/index.html $target/
cp -r src/css $target/
cp -r src/fonts $target/
cp -r src/js $target/

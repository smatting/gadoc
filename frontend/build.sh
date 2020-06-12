#!/usr/bin/env bash

spago build
parcel build  --no-content-hash --public-url "./" --no-source-maps index.html

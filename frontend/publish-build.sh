#!/usr/bin/env bash

rm -rf dist
mkdir -p dist
./build.sh
tarball="$(git rev-parse --short HEAD).tar.gz"
$(tar -czf /tmp/$tarball dist)
aws --profile gadoc s3 cp /tmp/$tarball s3://xahv0eel/$tarball
echo "https://xahv0eel.s3.eu-central-1.amazonaws.com/$tarball"

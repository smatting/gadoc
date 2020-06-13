#!/usr/bin/env bash
mkdir -p dist/
cp -v ../generated-docs/html/docstate.js \
      ../generated-docs/html/modules.js \
      ../generated-docs/html/targets.js \
      ./dist/

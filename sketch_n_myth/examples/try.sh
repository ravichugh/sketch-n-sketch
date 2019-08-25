#!/bin/bash
curl -v \
  -H "Content-Type: application/json" \
  -d "@$1" \
  -X POST \
  localhost:9090/eval

#!/bin/bash
curl \
  -H "Content-Type: application/json" \
  -d "@$1" \
  -X POST \
  http://localhost:9090

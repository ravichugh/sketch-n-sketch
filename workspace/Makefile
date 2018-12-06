SHELL := /bin/bash

all: install
	cd ..; editor&
	node generate.js --watch --autosync

input:
	node generate.js --backward

output:
	node generate.js --forward

watch:
	node generate.js --watch --autosync

install:
	npm install
	npm install -g http-server-editor
SHELL := /bin/bash

all: install
	cd ..; editor&
	hyde --watch --autosync

input:
	hyde --backward

output:
	hyde

watch:
	hyde --watch --autosync

install:
	npm install
	npm install -g http-server-editor
	npm install -g hyde-build-tool
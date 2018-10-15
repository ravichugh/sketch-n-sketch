ELMMAKE=elm-make
  # for locally-modified Elm compiler, create a symlink to something like:
  # /PATH/TO/elm-make/.stack-work/install/x86_64-osx/lts-3.22/7.10.2/bin/elm-make

out_from_Native :=aceCodeBox.js outputCanvas.js aceTooltips.js animationLoop.js fileHandler.js deucePopupPanelInfo.js \
 dotGraph.js colorScheme.js syntaxHighlight.js focus.js keyBlocker.js solverServer.js

out_from_ace-builds :=ace.js mode-little.js mode-elm.js theme-chrome.js

html: elm-stuff/packages
	$(ELMMAKE) Main.elm --output ../build/out/sns.js
	cd Native && cp $(out_from_Native) ../../build/out/
	cd ../ace-builds/src && cp $(out_from_ace-builds) ../../build/out/
	cp ../viz.js/viz.js ../build/out/
	mkdir -p ../build/out/img
	cp -r ../img/* ../build/out/img/
	# sed -i '' 's/var Elm = {};/var Elm = {};\nElm["EvalUpdate"] = Elm["EvalUpdate"] || {};\nElm["EvalUpdate"].api = _user$$project$$EvalUpdate$$api;/g' ../build/out/sns.js

all: elm-stuff/packages prelude examples html

elm-stuff/packages:
	# Use our custom version of core that allows anything in sets.
	#
	# Wacky, but have to do it this way to avoid circular dependency b/c Basics is auto-included in everything.
	#
	# Based on tests/run-tests.sh in elm-lang/core.
	elm-package install
	rm -rf elm-stuff/packages/elm-lang/core/5.1.1
	cd elm-stuff/packages/elm-lang/core && git clone --branch flexible_sets https://github.com/brianhempel/core.git && mv core 5.1.1
	cd ../../../..

# `make clean` gets rid of a lot of stuff so the subsequent build may be slow.
# Use this as a sort of "quick clean" that gets rid of less and lets the next build not be too slow.
remove_build_artifacts:
	rm -rf elm-stuff/build-artifacts/0.*/user
	rm -rf ../tests/elm-stuff/build-artifacts/0.*/user

# TODO there are some files in ../build/out that aren't in .gitignore, but if we could copy those into
# ../build/out from somwhere else during an html build, then clean could just delete everything under
# ../build/out
clean: remove_build_artifacts
	rm -rf elm-stuff/*
	cd ../build/out && rm -rf sns.js $(out_from_Native) $(out_from_ace-builds) viz.js img/*

prelude:
	python3 ../scripts/expandTemplate.py Prelude

examples:
	python3 ../scripts/expandTemplate.py Examples

lint:
	scripts/lint # Just looks for unused vars for now, and only in files dirty in git.

lintall:
	scripts/lint *.elm | less --RAW-CONTROL-CHARS

loc:
	wc -l \
    ../build/out/index.html ../build/out/main.css \
    Lang.elm LangSvg.elm Syntax.elm TopLevelExp.elm \
		LeoLang.elm \
		Info.elm Pos.elm \
		ParserUtils.elm LangParserUtils.elm BinaryOperatorParser.elm \
    FastParser.elm LangUnparser.elm \
		LeoParser.elm LeoUnparser.elm \
		ValUnparser.elm \
    Types2.elm Eval.elm ShapeWidgets.elm \
    Solver.elm LocEqn.elm Sync.elm \
    UpdateStack.elm UpdateRegex.elm Update.elm \
    Model.elm \
    Canvas.elm \
    View.elm Layout.elm \
    Controller.elm \
    History.elm \
    LangTools.elm LangSimplify.elm ValueBasedTransform.elm \
    Blobs.elm Draw.elm ExpressionBasedTransform.elm \
    DeuceWidgets.elm Deuce.elm DeuceTools.elm \
    Palettes.elm \
		OutputTools.elm \
    DependenceGraph.elm CodeMotion.elm Native/dotGraph.js \
    Ace.elm AceCodeBox.elm Native/aceCodeBox.js \
    OutputCanvas.elm Native/outputCanvas.js \
    AnimationLoop.elm Native/animationLoop.js \
    File.elm FileHandler.elm Native/fileHandler.js \
    DeucePopupPanelInfo.elm Native/deucePopupPanelInfo.js \
    ColorScheme.elm Native/colorScheme.js \
    SyntaxHighlight.elm Native/syntaxHighlight.js \
    Main.elm Config.elm \
    Utils.elm Either.elm Results.elm Updatable.elm \
    HtmlUtils.elm \
    PreludeTemplate.elm ExamplesTemplate.elm \
    ColorNum.elm Keys.elm \
    DefaultIconTheme.elm \
    Native/ImpureGoodies.js

littleloc:
	# wc -l ../examples/*
	../scripts/exampleStats.sh
	../scripts/exampleStats.sh | wc -l
	../scripts/exampleStats.sh | awk '{ sum += $$1 } END { print sum }'

tests:
	cd ../tests && ./test.sh && cd ../src

benchmarks:
	cd ../tests && ./benchmarks.sh && cd ../src

testbenchmarks:
	cd ../tests && ./benchmarks.sh --test && cd ../src

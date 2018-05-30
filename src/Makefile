ELMMAKE=elm-make
  # for locally-modified Elm compiler, create a symlink to something like:
  # /PATH/TO/elm-make/.stack-work/install/x86_64-osx/lts-3.22/7.10.2/bin/elm-make

all: elm-stuff/packages
	$(ELMMAKE) Main.elm --output ../build/out/sns.js
	sed -i '' 's/var Elm = {};/var Elm = {};\nElm["EvalUpdate"] = Elm["EvalUpdate"] || {};\nElm["EvalUpdate"].api = _user$$project$$EvalUpdate$$api;/g' ../build/out/sns.js

html: elm-stuff/packages
	$(ELMMAKE) Main.elm --output ../build/out/sns.js
	cp Native/aceCodeBox.js ../build/out/
	cp Native/outputCanvas.js ../build/out/
	cp Native/aceTooltips.js ../build/out/
	cp Native/animationLoop.js ../build/out/
	cp Native/fileHandler.js ../build/out/
	cp Native/deucePopupPanelInfo.js ../build/out/
	cp Native/dotGraph.js ../build/out/
	cp Native/colorScheme.js ../build/out/
	cp Native/syntaxHighlight.js ../build/out/
	cp Native/focus.js ../build/out/
	cp Native/keyBlocker.js ../build/out/
	cp Native/solverServer.js ../build/out/
	cp ../ace-builds/src/ace.js ../build/out/
	cp ../ace-builds/src/mode-little.js ../build/out/
	cp ../ace-builds/src/mode-elm.js ../build/out/
	cp ../ace-builds/src/theme-chrome.js ../build/out/
	cp ../viz.js/viz.js ../build/out/
	mkdir -p ../build/out/img
	cp ../img/sketch-n-sketch-logo.png ../build/out/img/
	cp ../img/light_logo.svg ../build/out/img/
	cp ../img/*.png ../build/out/img/
	sed -i '' 's/var Elm = {};/var Elm = {};\nElm["EvalUpdate"] = Elm["EvalUpdate"] || {};\nElm["EvalUpdate"].api = _user$$project$$EvalUpdate$$api;/g' ../build/out/sns.js

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

remove_build_artifacts:
	rm -r elm-stuff/build-artifacts/0.*/user; rm -r ../tests/elm-stuff/build-artifacts/0.*/user; true

clean: remove_build_artifacts prelude examples html

prelude:
	../scripts/expandTemplate.py Prelude

examples:
	../scripts/expandTemplate.py Examples

lint:
	scripts/lint # Just looks for unused vars for now, and only in files dirty in git.

lintall:
	scripts/lint *.elm | less --RAW-CONTROL-CHARS

loc:
	wc -l \
    ../build/out/index.html ../build/out/main.css \
    Lang.elm LangSvg.elm Syntax.elm TopLevelExp.elm \
		ElmLang.elm \
		Info.elm Pos.elm \
		ParserUtils.elm LangParserUtils.elm BinaryOperatorParser.elm \
    FastParser.elm LangUnparser.elm \
		ElmParser.elm ElmUnparser.elm \
		ValUnparser.elm \
    Types.elm Eval.elm ShapeWidgets.elm \
    Solver.elm LocEqn.elm Sync.elm \
    UpdateStack.elm UpdateRegex.elm Update.elm \
    InterfaceModel.elm \
    Canvas.elm \
    SleekView.elm SleekLayout.elm \
    InterfaceController.elm \
    History.elm \
    LangTools.elm LangSimplify.elm ValueBasedTransform.elm \
    Blobs.elm Draw.elm ExpressionBasedTransform.elm \
    DeuceWidgets.elm Deuce.elm DeuceTools.elm \
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

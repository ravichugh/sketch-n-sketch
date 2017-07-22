ELMMAKE=elm-make
  # for locally-modified Elm compiler, create a symlink to something like:
  # /PATH/TO/elm-make/.stack-work/install/x86_64-osx/lts-3.22/7.10.2/bin/elm-make

all:
	$(ELMMAKE) Main.elm --output ../build/out/sns.js

html:
	$(ELMMAKE) Main.elm --output ../build/out/sns.js
	cp Native/aceCodeBox.js ../build/out/
	cp Native/aceTooltips.js ../build/out/
	cp Native/animationLoop.js ../build/out/
	cp Native/fileHandler.js ../build/out/
	cp Native/dotGraph.js ../build/out/
	cp ../ace-builds/src/ace.js ../build/out/
	cp ../ace-builds/src/mode-little.js ../build/out/
	cp ../ace-builds/src/theme-chrome.js ../build/out/
	cp ../viz.js/viz.js ../build/out/
	mkdir -p ../build/out/img
	cp ../img/sketch-n-sketch-logo.png ../build/out/img/
	cp ../img/light_logo.svg ../build/out/img/
	cp ../img/deuce_logo.png ../build/out/img/

remove_build_artifacts:
	rm -r elm-stuff/build-artifacts/0.*/user; rm -r ../tests/elm-stuff/build-artifacts/0.*/user; true

clean: remove_build_artifacts prelude examples html

prelude:
	../scripts/expandTemplate.py Prelude

examples:
	../scripts/expandTemplate.py Examples

loc:
	wc -l \
    Lang.elm LangSvg.elm \
    FastParser.elm LangUnparser.elm \
    Types.elm Eval.elm ShapeWidgets.elm \
    Solver.elm LocEqn.elm Sync.elm \
    InterfaceModel.elm \
    Canvas.elm \
    SleekView.elm SleekLayout.elm \
    InterfaceController.elm \
    LangTools.elm LangSimplify.elm ValueBasedTransform.elm \
    Blobs.elm Draw.elm ExpressionBasedTransform.elm \
    DeuceWidgets.elm Deuce.elm DeuceTools.elm \
    DependenceGraph.elm CodeMotion.elm Native/dotGraph.js \
    Ace.elm AceCodeBox.elm Native/aceCodeBox.js \
    AnimationLoop.elm Native/animationLoop.js \
    FileHandler.elm Native/fileHandler.js \
    Main.elm Config.elm \
    Utils.elm Either.elm Updatable.elm \
    HtmlUtils.elm \
    PreludeTemplate.elm ExamplesTemplate.elm \
    ColorNum.elm Keys.elm \
    DefaultIconTheme.elm \
    UserStudy.elm UserStudyLog.elm \
    Native/UserStudyLog.js Native/ImpureGoodies.js

littleloc:
	# wc -l ../examples/*
	../scripts/exampleStats.sh
	../scripts/exampleStats.sh | wc -l
	../scripts/exampleStats.sh | awk '{ sum += $$1 } END { print sum }'

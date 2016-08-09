all:
	elm-make Main.elm --output ../build/out/sns.js

html:
	elm-make Main.elm --output ../build/out/sns.js
	cp Native/codeBox.js ../build/out/
	cp Native/toggleAnimationLoopback.js ../build/out/
	cp ../ace-builds/src/ace.js ../build/out/
	cp ../ace-builds/src/mode-little.js ../build/out/
	cp ../ace-builds/src/theme-chrome.js ../build/out/
	mkdir -p ../build/out/img
	cp img/*svg ../build/out/img/

clean:
	rm -r elm-stuff/build-artifacts/0.*/user
	rm -r ../tests/elm-stuff/build-artifacts/0.*/user

prelude:
	scripts/expandTemplate.py Prelude

examples:
	scripts/expandTemplate.py Examples

loc:
	wc -l \
    Lang.elm LangSvg.elm \
    LangParser2.elm LangUnparser.elm \
    OurParser2.elm Utils.elm Types.elm \
    Eval.elm Sync.elm LocEqn.elm \
    InterfaceModel.elm InterfaceView2.elm InterfaceController.elm \
    LangTools.elm LangTransform.elm ValueBasedTransform.elm \
    InterfaceStorage.elm Storage.elm \
    Ace.elm CodeBox.elm Native/codeBox.js \
    Native/toggleAnimationLoopback.js \
    Main.elm Config.elm \
    PreludeTemplate.elm ExamplesTemplate.elm \
    ColorNum.elm Keys.elm

littleloc:
	# wc -l ../examples/*
	scripts/exampleStats.sh
	scripts/exampleStats.sh | wc -l
	scripts/exampleStats.sh | awk '{ sum += $$1 } END { print sum }'

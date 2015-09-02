all:
	elm-make Main.elm --output ../build/out/sns.js > LOG.txt 2>&1

html:
	elm-make Main.elm --output ../build/out/sns.js
	cp Native/codeBox.js ../build/out/
	cp ../ace-builds/src/ace.js ../build/out/
	cp ../ace-builds/src/mode-little.js ../build/out/
	cp ../ace-builds/src/theme-chrome.js ../build/out/

prelude:
	scripts/expandTemplate.py Prelude

examples:
	scripts/expandTemplate.py Examples

loc:
	wc -l \
    Lang.elm LangSvg.elm \
    LangParser2.elm LangUnparser.elm \
    OurParser2.elm Utils.elm \
    Eval.elm Sync.elm \
    InterfaceModel.elm InterfaceView2.elm InterfaceController.elm \
    InterfaceStorage.elm Storage.elm \
    Codebox.elm Native/codeBox.js \
    Main.elm Config.elm \
    PreludeTemplate.elm ExamplesTemplate.elm \
    ColorNum.elm

littleloc:
	wc -l ../examples/*

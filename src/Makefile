html:
	elm-make Main.elm --output ../build/out/sns.js

prelude:
	scripts/expandTemplate.py Prelude

examples:
	scripts/expandTemplate.py Examples

loc:
	wc -l \
    Lang.elm LangParser.elm LangSvg.elm Eval.elm Sync.elm \
    InterfaceModel.elm InterfaceView2.elm InterfaceController.elm Main.elm \
    Config.elm \
    PreludeTemplate.elm ExamplesTemplate.elm \
    OurParser.elm Utils.elm

littleloc:
	wc -l ../examples/*

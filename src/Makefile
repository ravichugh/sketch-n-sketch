html:
	elm-make Main.elm --output ../build/out/sns.js

prelude:
	scripts/expandTemplate.py Prelude

examples:
	scripts/expandTemplate.py Examples

loc:
	wc -l \
    Lang.elm LangSvg.elm Eval.elm Sync.elm \
    InterfaceModel.elm InterfaceView2.elm InterfaceController.elm Main.elm \
    Config.elm \
    PreludeTemplate.elm ExamplesTemplate.elm \
    LangParser2.elm LangUnparser.elm \
    OurParser2.elm Utils.elm

littleloc:
	wc -l ../examples/*

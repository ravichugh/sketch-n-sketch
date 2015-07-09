html:
	elm-make Main.elm --output ../build/out/sns.js

prelude:
	scripts/makePrelude.py

examples:
	TODO

loc:
	wc -l \
    Lang.elm LangParser.elm LangSvg.elm Eval.elm Sync.elm \
    InterfaceModel.elm InterfaceView2.elm InterfaceController.elm Main.elm \
    Config.elm \
    OurParser.elm Utils.elm

littleloc:
	wc -l ../examples/prelude.little Examples.elm

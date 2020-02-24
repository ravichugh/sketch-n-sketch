build:
	dune build src/main.exe

serve:
	make build && python3 serve.py

clean:
	dune clean

deps:
	opam install \
		utop dune yojson ppx_deriving ppx_deriving_yojson

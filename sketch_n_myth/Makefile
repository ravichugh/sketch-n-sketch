serve:
	dune exec src/main.exe

build:
	dune build src/main.exe

clean:
	dune clean

deps:
	opam install \
		utop dune tls lwt cohttp cohttp-lwt-unix yojson ppx_deriving \
		ppx_deriving_yojson

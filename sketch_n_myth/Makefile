build:
	dune build src/main.exe

serve:
	dune exec src/main.exe

clean:
	dune clean

deps:
	opam install \
		utop dune tls lwt cohttp cohttp-lwt-unix yojson ppx_deriving \
		ppx_deriving_yojson

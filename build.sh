mkdir -p _build
ocamlfind ocamlc -pp refmt -linkpkg -package graphics -g -impl index.re  -o resnake
mv index.cmi _build/index.cmi
mv index.cmo _build/index.cmo
mv index _build/index

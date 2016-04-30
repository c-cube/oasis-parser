OASISFormat: 0.4
Name:        oasis_parser
Version:     0.1
Synopsis:    Simple parser for _oasis files
Authors:     Simon Cruanes
License:     MIT
Plugins:     META (0.4), StdFiles (0.4), DevFiles (0.4)

Library oasis_parser
  Path:       src/
  BuildTools: ocamlbuild
  Modules:    Oasis_ast, Oasis_parser, Oasis_parser_co

Executable oasis_check
  Path:         src/tools/
  MainIs:       oasis_check.ml
  BuildDepends: oasis_parser
  Install:      false
  CompiledObject: best
  BuildTools:   ocamlbuild
  

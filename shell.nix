{ pkgs ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/nixos-unstable.tar.gz";
  }) {}
}:

pkgs.mkShell {
  nativeBuildInputs = with pkgs.ocamlPackages; [
    dune_3
    findlib
    js_of_ocaml-compiler
    ocaml
    ocamlformat
    odoc
  ];
  buildInputs = with pkgs.ocamlPackages; [
    ppxlib
    prelude
  ];
}

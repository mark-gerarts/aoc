{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  buildInputs = [ pkgs.glpk ];

  shellHook = ''
    export LD_LIBRARY_PATH="${pkgs.glpk}/lib''${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
  '';
}

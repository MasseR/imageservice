{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let shell = buildEnv {
      name = "shell";
      buildInputs = [
        purescript
        psc-package
        psc-package2nix
        compile
        repl
        nodePackages.browserify
        nodejs
      ];
      paths = [];
    };
    deps = with builtins;
      let cp = k: v: ''
        mkdir -p $out/${k}
        cp -r ${v}/* $out/${k}/
      '';
      in
    pkgs.runCommandNoCC "mkdeps" {}
    ''
      ${concatStringsSep "\n" (pkgs.lib.mapAttrsToList cp packages.inputs)}
    '';
    compile = writeScriptBin "compile" ''
      find src ${deps}/*/src -type f -path '*src*' -name '*.purs' -exec purs compile {} +
    '';
    repl = writeScriptBin "repl" ''
      find src ${deps}/*/src -type f -path '*src*' -name '*.purs' -exec purs repl {} +
    '';
    packages = pkgs.callPackage ./packages.nix {};
    psc-package2nix = callPackage (fetchFromGitHub {
      owner = "justinwoo";
      repo = "psc-package2nix";
      rev = "da2368886961e08c5f0b5b3f78aa485fed116d8e";
      sha256 = "05akkd3p9hs03iia9g2swscms7sd0pviflj8rjq1hiak8ajgx6qm";
    }) {};
    frontend = stdenv.mkDerivation {
      name = "frontend";
      src = ./.;
      buildInputs = [
        purescript
        psc-package
        deps
        compile
      ];
      buildPhase = ''
        rm -fr .psc-package/
        rm -fr output/
        compile
        ${pkgs.purescript}/bin/purs bundle output/Main/index.js -o output/Main/app.js
        ${pkgs.nodePackages.browserify}/bin/browserify index.js -o bundle.js
      '';
      installPhase = ''
        mkdir $out/
        cp -r bundle.js $out/index.js
      '';
    };

in

rec {
  inherit shell frontend deps psc-package2nix;
}


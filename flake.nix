{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }: let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};
    inherit (pkgs) lib stdenv stdenvNoCC cmake cmocka mkShell runtimeShell;

    version = "0.1.0";
    meta = {
      description = "Tracing JIT Lisp compiler";
      homepage = "https://github.com/axelf4/lisp";
      license = lib.licenses.gpl3Plus;
      platforms = lib.platforms.all;
      mainProgram = "lisp";
    };

    lisp = stdenv.mkDerivation {
      pname = "lisp";
      inherit version meta;
      src = builtins.path { path = ./.; name = "lisp"; };

      strictDeps = true;
      nativeBuildInputs = [ cmake ];
      checkInputs = [ cmocka ];
      outputs = [ "out" "man" ];

      doCheck = true;
      hardeningDisable = [ "all" ];
      env.CFLAGS = "-march=x86-64-v3 -fomit-frame-pointer";

      preCheck = ''
        export LIBRESTORE_SO=$PWD/librestore.so
      '';
    };

    lisp-wrapped = stdenvNoCC.mkDerivation {
      pname = "lisp-wrapped";
      inherit version meta;

      buildCommand = ''
        mkdir -p $out/bin
        ${lib.getExe lisp} --checkpoint $out/checkpoint
        >$out/bin/lisp cat <<EOF
        #!${runtimeShell}
        exec ${lib.getExe lisp} --restore $out/checkpoint "\$@"
        EOF
        chmod +x $out/bin/lisp
      '';
    };
  in {
    packages.${system} = { inherit lisp lisp-wrapped; default = lisp; };

    devShells.${system}.default = mkShell {
      inputsFrom = [ lisp ];
      packages = with pkgs; [ doxygen valgrind aflplusplus lttng-tools lttng-ust ];

      env.NIX_ENFORCE_NO_NATIVE = 0;
    };
  };
}

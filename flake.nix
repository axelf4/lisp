{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }: let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};
    stdenv = pkgs.gcc15Stdenv;
  in {
    packages.${system}.default = stdenv.mkDerivation {
      name = "lisp";
      src = builtins.path { path = ./.; name = "lisp"; };

      strictDeps = true;
      nativeBuildInputs = [ pkgs.cmake ];
      checkInputs = [ pkgs.cmocka ];

      doCheck = true;

      hardeningDisable = [ "all" ];
      cmakeFlags = [
        "-DCMAKE_INTERPROCEDURAL_OPTIMIZATION=TRUE"
        "-DWITH_TAIL_CALL_INTERP=ON"
      ];
      env.CFLAGS = "-march=x86-64-v3 -fomit-frame-pointer";
    };

    devShells.${system}.default = pkgs.mkShell.override { inherit stdenv; } {
      inputsFrom = [ self.packages.${system}.default ];
      packages = with pkgs; [ doxygen valgrind aflplusplus lttng-tools lttng-ust ];

      env.NIX_ENFORCE_NO_NATIVE = 0;
    };
  };
}

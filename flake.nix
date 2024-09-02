{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs";

  outputs = { self, nixpkgs }: let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};
  in {
    packages.${system}.default = pkgs.gcc14Stdenv.mkDerivation {
      name = "lisp";
      src = builtins.path { path = ./.; name = "lisp"; };

      strictDeps = true;
      nativeBuildInputs = [ pkgs.cmake ];
      buildInputs = with pkgs; [ xxHash ];
      checkInputs = [ pkgs.cmocka ];

      doCheck = true;

      hardeningDisable = [ "all" ];
      cmakeFlags = [ "-DCMAKE_INTERPROCEDURAL_OPTIMIZATION=TRUE" ];
      env.CFLAGS = "-march=x86-64-v3";
    };

    devShells.${system}.default = pkgs.mkShell.override { stdenv = pkgs.gcc14Stdenv; } {
      inputsFrom = [ self.packages.${system}.default ];
      packages = with pkgs; [ doxygen valgrind ];

      env.NIX_ENFORCE_NO_NATIVE = 0;
    };
  };
}

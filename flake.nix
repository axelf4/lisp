{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs";

  outputs = { self, nixpkgs }: let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};
  in {
    packages.${system}.default = pkgs.gcc13Stdenv.mkDerivation {
      name = "lisp";
      src = builtins.path { path = ./.; name = "lisp"; };

      nativeBuildInputs = [ pkgs.cmake ];
      buildInputs = with pkgs; [ xxHash ];
      checkInputs = [ pkgs.cmocka ];

      doCheck = true;

      cmakeFlags = [ "-DCMAKE_INTERPROCEDURAL_OPTIMIZATION=TRUE" ];

      env.CFLAGS = "-O3 -march=x86-64-v3";
    };

    devShells.${system}.default = pkgs.mkShell.override { stdenv = pkgs.gcc13Stdenv; } {
      inputsFrom = [ self.packages.${system}.default ];
      packages = with pkgs; [ doxygen valgrind ];
    };
  };
}

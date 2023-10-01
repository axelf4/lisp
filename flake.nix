{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs";

  outputs = { self, nixpkgs }: let
    system = "x86_64-linux";
    pkgs = import nixpkgs {
      inherit system;
      overlays = [ (final: prev: {
        croaring = pkgs.stdenv.mkDerivation rec {
          pname = "croaring";
          version = "2.0.2";

          src = final.fetchFromGitHub {
            owner = "RoaringBitmap";
            repo = "CRoaring";
            rev = "v${version}";
            hash = "sha256-lskBScll3g8MslyXiDxOxIwCukJm65wwbBMX5PjM1w4=";
          };

          # roaring.pc.in cannot handle absolute CMAKE_INSTALL_*DIRs, nor
          # overridden CMAKE_INSTALL_FULL_*DIRs. With Nix, they are guaranteed
          # to be absolute so the following patch suffices (see #144170).
          patches = [ ./croaring-1.patch ];

          nativeBuildInputs = [ final.cmake ];

          doCheck = true;

          preConfigure = ''
            mkdir -p dependencies/.cache
            ln -s ${final.fetchFromGitHub {
              owner = "clibs";
              repo = "cmocka";
              rev = "f5e2cd7";
              hash = "sha256-Oq0nFsZhl8IF7kQN/LgUq8VBy+P7gO98ep/siy5A7Js=";
            }} dependencies/.cache/cmocka
          '';
        };
      }) ];
    };
  in {
    packages.x86_64-linux.default = pkgs.gcc13Stdenv.mkDerivation {
      name = "lisp";
      src = builtins.path { path = ./.; name = "lisp"; };

      nativeBuildInputs = [ pkgs.cmake ];
      buildInputs = with pkgs; [ croaring xxHash ];
      checkInputs = [ pkgs.cmocka ];

      doCheck = true;
    };

    devShells.x86_64-linux.default = pkgs.mkShell.override { stdenv = pkgs.gcc13Stdenv; } {
      inputsFrom = [ self.packages.x86_64-linux.default ];
      buildInputs = with pkgs; [ valgrind ];
    };
  };
}

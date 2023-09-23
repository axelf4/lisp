{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs";

  outputs = { self, nixpkgs }: let
    system = "x86_64-linux";
    pkgs = import nixpkgs {
      inherit system;
      overlays = [ (final: prev: {
        croaring = pkgs.stdenv.mkDerivation rec {
          pname = "croaring";
          version = "1.3.0";

          src = prev.fetchFromGitHub {
            owner = "RoaringBitmap";
            repo = "CRoaring";
            rev = "v${version}";
            hash = "sha256-InyGzdGa+5eam5kiSZIna7xFnsImoi7Z5iZ7i8vTRIU=";
          };

          nativeBuildInputs = [ prev.cmake ];

          doCheck = true;

          # cmakeFlags = [ "-DENABLE_ROARING_MICROBENCHMARKS=OFF" ];

          # roaring.pc.in cannot handle absolute CMAKE_INSTALL_*DIRs,
          # nor overridden CMAKE_INSTALL_FULL_*DIRs. With Nix, they
          # are guaranteed absolute so the following patch suffices
          # (see #144170).
          patches = [ ./croaring-1.patch ];

          preConfigure = ''
            mkdir -p dependencies/.cache
            ln -s ${prev.fetchFromGitHub {
              owner = "clibs";
              repo = "cmocka";
              rev = "f5e2cd7";
              hash = "sha256-Oq0nFsZhl8IF7kQN/LgUq8VBy+P7gO98ep/siy5A7Js=";
            }} dependencies/.cache/cmocka
            ln -s ${prev.fetchFromGitHub {
              owner = "google";
              repo = "benchmark";
              rev = "f91b6b4";
              hash = "sha256-EAJk3JhLdkuGKRMtspTLejck8doWPd7Z0Lv/Mvf3KFY=";
            }} dependencies/.cache/google_benchmarks
          '';
        };
      }) ];
    };
  in {
    devShells.x86_64-linux.default = pkgs.mkShell {
      buildInputs = [ pkgs.cmake pkgs.croaring pkgs.cmocka ];
    };
  };
}

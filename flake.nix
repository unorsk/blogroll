{
  description = "Blogroll - RSS feed aggregator with static HTML generation";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    haskellNix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      haskellNix,
      flake-utils,
    }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ] (
      system:
      let
        overlays = [ haskellNix.overlay ];

        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };

        # For static builds on Linux, use musl cross-compilation
        isLinux = pkgs.stdenv.hostPlatform.isLinux;

        # Use older UPX version (4.x) which is more stable with static binaries
        upx4 = pkgs.stdenv.mkDerivation {
          pname = "upx";
          version = "4.2.4";

          src = pkgs.fetchurl {
            url = "https://github.com/upx/upx/releases/download/v4.2.4/upx-4.2.4-amd64_linux.tar.xz";
            sha256 = "14s18zdvsg4w9f6kx81asypwg06ji1iz6pz4brcb8bxpgbjv9jkm";
          };

          dontBuild = true;
          dontConfigure = true;

          installPhase = ''
            mkdir -p $out/bin
            cp upx $out/bin/
            chmod +x $out/bin/upx
          '';
        };

        # Static build project (Linux only, uses musl)
        staticProject =
          if isLinux then
            pkgs.pkgsCross.musl64.haskell-nix.cabalProject' {
              src = ./.;
              compiler-nix-name = "ghc9122";

              modules = [
                {
                  packages.blogroll = {
                    # Disable shared libraries for static build
                    enableShared = false;
                    enableStatic = true;

                    # Configure static linking for musl
                    configureFlags = [
                      "--disable-shared"
                      "--enable-static"
                      "--ghc-option=-optl=-static"
                      "--ghc-option=-optl=-pthread"
                    ];

                    # Optimization flags
                    ghcOptions = [
                      "-O2"
                      "-split-sections"
                    ];

                    # Post-build hook to strip and compress
                    postInstall = ''
                      if [ -f $out/bin/blogroll ]; then
                        # Strip the binary first
                        echo "Stripping binary..."
                        ${pkgs.binutils}/bin/strip $out/bin/blogroll

                        # Get original size
                        ORIGINAL_SIZE=$(stat -c%s $out/bin/blogroll)
                        echo "Original size: $ORIGINAL_SIZE bytes"

                        # Try UPX with progressively less aggressive settings
                        echo "Attempting UPX compression..."

                        # Try -9 with older, more stable UPX version
                        if ${upx4}/bin/upx -9 $out/bin/blogroll 2>&1; then
                          COMPRESSED_SIZE=$(stat -c%s $out/bin/blogroll)
                          echo "UPX compression successful with -9: $COMPRESSED_SIZE bytes"
                        else
                          echo "ERROR: UPX compression failed"
                          exit 1
                        fi
                      fi
                    '';
                  };
                }
              ];
            }
          else
            null;

        # Regular dynamic build project (all platforms)
        dynamicProject = pkgs.haskell-nix.cabalProject' {
          src = ./.;
          compiler-nix-name = "ghc9122";

          shell = {
            tools = {
              cabal = { };
              haskell-language-server = { };
            };

            buildInputs = with pkgs; [
              zlib
              pkg-config
            ];
          };

          modules = [
            {
              packages.blogroll = {
                # Optimization flags
                ghcOptions = [
                  "-O2"
                  "-split-sections"
                ];
              };
            }
          ];
        };

        # Use dynamic builds by default for fast iteration
        project = dynamicProject;

        dynamicFlake = dynamicProject.flake { };
        staticFlake = if isLinux && staticProject != null then staticProject.flake { } else null;

      in
      {
        packages = {
          # Default to dynamic build
          default = dynamicFlake.packages."blogroll:exe:blogroll";
          blogroll = dynamicFlake.packages."blogroll:exe:blogroll";

          # Provide static build on Linux (musl + UPX compressed)
          blogroll-static = if isLinux then staticFlake.packages."blogroll:exe:blogroll" else null;
        };

        # Provide the same apps as the default flake output
        apps = {
          default = {
            type = "app";
            program = "${dynamicFlake.packages."blogroll:exe:blogroll"}/bin/blogroll";
          };
        };

        devShells = dynamicFlake.devShells or { };
      }
    );
}

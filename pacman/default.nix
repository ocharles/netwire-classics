{ }:
with import <nixpkgs> {};

let
  haskell = haskellPackages;
  inherit (haskell) cabal cabalInstall lens monadLoops SDL distributive hashable
  reflection semigroupoids semigroups tagged transformers unorderedContainers
  vector;

  netwire = callPackage /home/ollie/work/netwire {};

  SDLgfx = cabal.mkDerivation (self: {
    pname = "SDL-gfx";
    version = "0.6.0";
    sha256 = "14d8fz576rwi6x0dxgc29cdmwn48afja3v5qx3x8q5y61fv8w9v1";
    buildDepends = [ SDL ];
    extraLibraries = [ SDL_gfx ];
    meta = {
      description = "Binding to libSDL_gfx";
      license = self.stdenv.lib.licenses.bsd3;
      platforms = self.ghc.meta.platforms;
    };
  });

  SDLttf = cabal.mkDerivation (self: {
    pname = "SDL-ttf";
    version = "0.6.2";
    sha256 = "0jajnbqnhdd4i8pj8j27m53zwgfs1v06kiwy0s0zml02fdkq8j4a";
    buildDepends = [ SDL ];
    extraLibraries = [ SDL_ttf ];
    meta = {
      description = "Binding to libSDL_ttf";
      license = self.stdenv.lib.licenses.bsd3;
      platforms = self.ghc.meta.platforms;
    };
  });

  linear = cabal.mkDerivation (self: {
    pname = "linear";
    version = "1.1.2";
    sha256 = "1fbps3c0j3h2n9gzjcb288d9cyq87mhnjdm4cj17h6l7nzdlyszg";
    buildDepends = [
      distributive hashable reflection semigroupoids semigroups tagged
      transformers unorderedContainers vector
    ];
    doCheck = false;
    meta = {
      homepage = "http://github.com/ekmett/linear/";
      description = "Linear Algebra";
      license = self.stdenv.lib.licenses.bsd3;
      platforms = self.ghc.meta.platforms;
    };
  });

in cabal.mkDerivation (self: {
  pname = "netwire-classics";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [ cabalInstall lens monadLoops netwire SDL SDLgfx linear ];
  buildTools = [ fish git haskellPackages.ghcMod vim_configurable ];
})

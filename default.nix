{ }:
with import <nixpkgs> {};

let
  haskell = haskellPackages;
  inherit (haskell) cabal cabalInstall netwire SDL lens SDLMixer distributive
  hashable reflection semigroupoids semigroups tagged transformers
  unorderedContainers vector doctest filepath simpleReflect;

  linear = cabal.mkDerivation (self: {
  pname = "linear";
  version = "1.1.2";
  sha256 = "1fbps3c0j3h2n9gzjcb288d9cyq87mhnjdm4cj17h6l7nzdlyszg";
  buildDepends = [
    distributive hashable reflection semigroupoids semigroups tagged
    transformers unorderedContainers vector
  ];
  testDepends = [ doctest filepath lens simpleReflect ];
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
  buildDepends = [ cabalInstall netwire SDL SDLMixer lens linear ];
})

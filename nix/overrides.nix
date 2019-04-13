{ pkgs }:

self: super:

with { inherit (pkgs.stdenv) lib; };

with pkgs.haskell.lib;

{
  hedgehog-generic = (
    with rec {
      hedgehog-genericSource = pkgs.lib.cleanSource ../.;
      hedgehog-genericBasic  = self.callCabal2nix "hedgehog-generic" hedgehog-genericSource { };
    };
    overrideCabal hedgehog-genericBasic (old: {
    })
  );
}

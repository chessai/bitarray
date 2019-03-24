{ pkgs }:

self: super:

with { inherit (pkgs.stdenv) lib; };

with pkgs.haskell.lib;

{
  bitarray = (
    with rec {
      bitarraySource = pkgs.lib.cleanSource ../.;
      bitarrayBasic  = self.callCabal2nix "bitarray" bitarraySource { };
    };
    overrideCabal bitarrayBasic (old: {
    })
  );
}

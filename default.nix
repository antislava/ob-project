{ system ? builtins.currentSystem # TODO: Get rid of this system cruft
, iosSdkVersion ? "10.2"
, withHoogle ? false # to spin up localhost:8080 hoogle use: nix-shell --arg withHoogle true -A shells.ghc --command "hoogle server -p 8080 --local"
}:
with import ./.obelisk/impl { inherit system iosSdkVersion; };
project ./. ({ hackGet, ... }: {
  android.applicationId = "systems.obsidian.obelisk.examples.all";
  android.displayName = "Obelisk Examples";
  inherit withHoogle;
  packages = {
    reflex-dom-echarts = hackGet ./deps/reflex-dom-echarts;
    echarts-jsdom = hackGet ./deps/echarts-jsdom;
  };
})

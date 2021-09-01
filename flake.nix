{
  description = "XMonad Flake";
  outputs = { self }: {
    nixosModules.vidbinaXmonadConfig = {
      home-manager.xsession.windowManager.xmonad = {
        enable = true;
        config = ./xmonad.hs;
      };
    };
  };
}

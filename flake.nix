{
  description = "XMonad Flake";
  outputs = { self }: {
    # xsession.windowManager.xmonad
    nixosModule = { config, ... }: {
      enable = true;
      enableContribAndExtras = true;
      config = ./xmonad.hs;
    };
  };
}

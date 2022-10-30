{
  description = "XMonad Flake";
  outputs = { self }: {
    # xsession.windowManager.xmonad
    nixosModule = {
      enable = true;
      enableContribAndExtras = true;
      config = ./xmonad.hs;
    };
  };
}

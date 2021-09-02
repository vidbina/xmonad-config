{
  description = "XMonad Flake";
  outputs = { self }: {
    nixosModule = { config, ... }: {
      home-manager.users.vidbina.xsession.windowManager.xmonad = {
        enable = true;
        config = ./xmonad.hs;
      };
    };
  };
}

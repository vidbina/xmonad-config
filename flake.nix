{
  description = "XMonad Flake";
  outputs = { self }: {
    nixosModule = { config, ... }: {
      home-manager.users."${config.username}".xsession.windowManager.xmonad = {
        enable = true;
        config = ./xmonad.hs;
      };
    };
  };
}

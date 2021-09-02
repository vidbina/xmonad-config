{
  description = "XMonad Flake";
  outputs = { self }: {
    nixosModule = { config, ... }: {
      forUser = (username: {
        home-manager.users."${username}".xsession.windowManager.xmonad = {
          enable = true;
          config = ./xmonad.hs;
        };
      });
    };
  };
}

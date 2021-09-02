{
  description = "XMonad Flake";
  outputs = { self }: {
    nixosModule = { config, ... }: username: {
      home-manager.users."${username}".xsession.windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
        config = ./xmonad.hs;
      };
    };
  };
}

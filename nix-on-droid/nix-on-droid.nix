{ config, lib, pkgs, ... }:

{
  # Simply install just the packages
  environment.packages = with pkgs; [
    # User-facing stuff that you really really want to have
    vim # or some other editor, e.g. nano or neovim

    # Some common stuff that people expect to have
    procps
    killall
    diffutils
    findutils
    utillinux
    tzdata
    hostname
    man
    gnugrep
    gnupg
    gnused
    gnutar
    bzip2
    gzip
    xz
    zip
    unzip
  ];

  # Backup etc files instead of failing to activate generation if a file already exists in /etc
  environment.etcBackupExtension = ".bak";

  # Read the changelog before changing this value
  system.stateVersion = "24.05";

  # Set up nix for flakes
  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';

  # Set your time zone
  time.timeZone = "Europe/Berlin";

  # Android integration features
  android-integration = {
    termux-open.enable = true;        # Open files/URLs in Android apps
    termux-open-url.enable = true;    # Open URLs via Android VIEW intent  
    xdg-open.enable = true;           # Provides xdg-open compatibility
    termux-setup-storage.enable = true; # Storage permission and symlinks
  };

  # Set default shell
  user.shell = "${pkgs.zsh}/bin/zsh";

  # Configure home-manager
  home-manager = {
    backupFileExtension = "hm-bak";
    useGlobalPkgs = true;

    config =
      { config, lib, pkgs, ... }:
      {
        # Read the changelog before changing this value
        home = {
          stateVersion = "24.05";

          # insert home-manager config
          packages = with pkgs; [
            comma
            claude-code
            fasd
            fzf
            git
            magic-wormhole
            openssh
            starship
            gh
          ];

          activation = {
            copyFont = lib.hm.dag.entryAfter ["writeBoundary"] ''
              $DRY_RUN_CMD install $VERBOSE_ARG -D "${pkgs.fira-code}/share/fonts/truetype/FiraCode-VF.ttf" ${config.home.homeDirectory}/.termux/font.ttf
            '';
          };
        };

        programs.zsh = {
          enable = true;
          enableCompletion = true;
          autosuggestion.enable = true;
          syntaxHighlighting.enable = true;
          
          history = {
            save = 100000;
            size = 100000;
            path = "${config.home.homeDirectory}/.zsh_history";
            ignoreDups = true;
            ignoreSpace = true;
            extended = true;
          };
          
          shellAliases = {
            ll = "ls -l";
            la = "ls -la";
            ".." = "cd ..";
            z = "fasd_cd -d";
          };
          
          initExtra = ''
            # Initialize fasd
            eval "$(fasd --init auto)"
          '';
        };

        programs.fzf = {
          enable = true;
          enableZshIntegration = true;
        };

        programs.starship = {
          enable = true;
          enableZshIntegration = true;
        };

        services.ssh-agent = {
          enable = true;
        };
      };
  };
}

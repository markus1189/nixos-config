{
  config,
  lib,
  pkgs,
  ...
}:

{
  # Simply install just the packages
  environment.packages = with pkgs; [
    # User-facing stuff that you really really want to have
    # vim configured via home-manager programs.vim

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
    gawk
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
    termux-open.enable = true; # Open files/URLs in Android apps
    termux-open-url.enable = true; # Open URLs via Android VIEW intent
    xdg-open.enable = true; # Provides xdg-open compatibility
    termux-setup-storage.enable = true; # Storage permission and symlinks
  };

  # Set default shell
  user.shell = "${pkgs.zsh}/bin/zsh";

  # Configure home-manager
  home-manager = {
    backupFileExtension = "hm-bak";
    useGlobalPkgs = true;

    config =
      {
        config,
        lib,
        pkgs,
        ...
      }:
      {
        # Read the changelog before changing this value
        home = {
          stateVersion = "24.05";

          # insert home-manager config
          packages = with pkgs; [
            ast-grep
            awscli2
            aws-vault
            comma
            claude-code
            curl
            fasd
            fzf
            httpie
            htop
            magic-wormhole
            openssh
            pass
            rclone
            ripgrep
            starship
            gh
            wget

            # Additional essential tools
            jq               # JSON processor
            visidata         # Interactive data exploration
            dig              # DNS lookup utility
            dog              # Modern DNS lookup tool
            nmap             # Network discovery and security auditing
            tmux             # Terminal multiplexer
            gdu              # Fast disk usage analyzer
            tree             # Directory tree visualization
            file             # File type identification
            which            # Command location finder
            less             # Text pager
            unrar            # RAR archive extractor
            nixpkgs-review
            w3m
          ];

          activation = {
            copyFont = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
              $DRY_RUN_CMD install $VERBOSE_ARG -D "${pkgs.fira-code}/share/fonts/truetype/FiraCode-VF.ttf" ${config.home.homeDirectory}/.termux/font.ttf
            '';
          };

          file =
            {
              "visidatarc" = {
                target = ".visidatarc";
                text = ''
                  import json

                  options.disp_date_fmt = '%Y-%m-%d %H:%M:%S.%f%z'
                  options.disp_float_fmt = '{:.04f}'

                  Sheet.bindkey(ALT + '.', 'repeat-input')
                  Sheet.bindkey('z' + ALT + '.', 'repeat-last')

                  def mh_utc_from_unix(seconds):
                    return datetime.datetime.utcfromtimestamp(seconds).strftime('%Y-%m-%dT%H:%M:%SZ')

                  def mh_fromjson(s):
                    return json.loads(s)
                '';
              };

              "claude-code" = {
                target = ".claude/settings.json";
                text = pkgs.lib.strings.toJSON {
                  includeCoAuthoredBy = false;
                  cleanupPeriodDays = 3650;
                  env = {
                    ACTIVE_CLAUDE_CODE_SESSION = "true";
                  };

                  statusLine = {
                    "type" = "command";
                    "command" =
                      let
                        name = "claude-code-statusline";
                        script = pkgs.writeShellApplication {
                          inherit name;
                          runtimeInputs = with pkgs; [ coreutils jq ];
                          text = builtins.readFile ../nixos-shared/claude/claude-code-statusline.sh;
                        };
                      in
                      "${script}/bin/${name}";
                    "padding" = 0;
                  };

                  permissions = {
                    allow = [
                      "Bash(grep:*)"
                      "Bash(mktemp:*)"
                      "Bash(rg:*)"
                      "Bash(git add:*)"
                      "Bash(git fetch:*)"
                      "Bash(git commit:*)"
                      "Bash(git diff:*)"
                      "Bash(git log:*)"
                      "Bash(git branch:*)"
                    ];
                  };
                  env = {
                    BASH_DEFAULT_TIMEOUT_MS = 5 * 60 * 1000; # default = 2 min
                    BASH_MAX_TIMEOUT_MS = 30 * 60 * 1000;
                    MAX_MCP_OUTPUT_TOKENS = 50 * 1000; # default = 25,000
                  };
                };
              };

              "claude-md" = {
                target = ".claude/CLAUDE.md";
                text = builtins.readFile ../nixos-shared/claude/CLAUDE-global.md;
              };
            }
            // (
              # Helper function to automatically discover and configure markdown files
              let
                autoConfigMarkdownFiles =
                  sourceDir: targetSubdir: namePrefix:
                  let
                    files = builtins.readDir sourceDir;
                    isMarkdownFile = name: type: type == "regular" && pkgs.lib.strings.hasSuffix ".md" name;
                    markdownFiles = pkgs.lib.attrsets.filterAttrs isMarkdownFile files;

                    makeEntry = filename: {
                      target = ".claude/${targetSubdir}/${filename}";
                      text = builtins.readFile (sourceDir + "/${filename}");
                    };

                    entries = pkgs.lib.attrsets.mapAttrs' (
                      filename: _:
                      pkgs.lib.attrsets.nameValuePair "${namePrefix}-${pkgs.lib.strings.removeSuffix ".md" filename}" (
                        makeEntry filename
                      )
                    ) markdownFiles;
                  in
                  entries;

                # Auto-configure command files
                commandEntries = autoConfigMarkdownFiles ../nixos-shared/claude/commands "commands" "claude";

                # Auto-configure docs files
                docsEntries = autoConfigMarkdownFiles ../nixos-shared/claude/docs "user-docs" "claude-docs";

              in
              commandEntries // docsEntries
            );
        };

        manual = {
          html.enable = true;
          json.enable = true;
          manpages.enable = true;
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
            c = "env SHELL=bash claude";
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

        programs.direnv = {
          enable = true;
          nix-direnv = {
            enable = true;
          };
          enableZshIntegration = true;
        };

        services.ssh-agent = {
          enable = true;
        };

        services.gpg-agent = {
          enable = true;
          enableSshSupport = true;
          defaultCacheTtl = 36000;
          maxCacheTtl = 86400;
          pinentry.package = pkgs.pinentry-tty;
          extraConfig = ''
            allow-loopback-pinentry
          '';
        };

        programs.git = {
          enable = true;
          userName = "Markus Hauck";
          userEmail = "markus1189@gmail.com";
          
          aliases = {
            co = "checkout";
            s = "status";
          };
          
          extraConfig = {
            init.defaultBranch = "main";
            pull.rebase = true;
            push.default = "simple";
            rebase.autostash = true;
            help.autocorrect = 1;
          };
        };

        programs.vim = {
          enable = true;
          defaultEditor = true;
          
          extraConfig = ''
            " Line numbers
            set number
            set relativenumber
            
            " Indentation
            set expandtab
            set tabstop=2
            set shiftwidth=2
            set softtabstop=2
            set autoindent
            set smartindent
            
            " Search
            set hlsearch
            set incsearch
            set ignorecase
            set smartcase
            
            " Display
            set nowrap
            set scrolloff=8
            set sidescrolloff=8
            set cursorline
            set showmatch
            
            " Files and backups
            set nobackup
            set nowritebackup
            set noswapfile
            
            " Command line
            set wildmenu
            set wildmode=longest:full,full
            
            " Mouse support
            set mouse=a
            
            " Performance
            set lazyredraw
            
            " Status line
            set laststatus=2
            set ruler
            
            " Colors and syntax
            syntax on
            set t_Co=256
            set background=dark
            
            " Better completion
            set completeopt=menuone,noinsert,noselect
            
            " Persistent undo
            set undofile
            set undodir=~/.vim/undodir
            if !isdirectory(&undodir)
              call mkdir(&undodir, 'p')
            endif
            
            " Key mappings
            let mapleader = " "
            
            " Better window navigation
            nnoremap <C-h> <C-w>h
            nnoremap <C-j> <C-w>j
            nnoremap <C-k> <C-w>k
            nnoremap <C-l> <C-w>l
            
            " Clear search highlighting
            nnoremap <leader>h :nohlsearch<CR>
            
            " Quick save and quit
            nnoremap <leader>w :w<CR>
            nnoremap <leader>q :q<CR>
            nnoremap <leader>x :x<CR>
            
            " Toggle paste mode
            set pastetoggle=<F2>
            
            " Better indenting in visual mode
            vnoremap < <gv
            vnoremap > >gv
            
            " Move lines up/down
            nnoremap <A-j> :m .+1<CR>==
            nnoremap <A-k> :m .-2<CR>==
            inoremap <A-j> <Esc>:m .+1<CR>==gi
            inoremap <A-k> <Esc>:m .-2<CR>==gi
            vnoremap <A-j> :m '>+1<CR>gv=gv
            vnoremap <A-k> :m '<-2<CR>gv=gv
            
            " Better search behavior
            nnoremap n nzzzv
            nnoremap N Nzzzv
            
            " Keep cursor centered when joining lines
            nnoremap J mzJ`z
            
            " Better redo
            nnoremap U <C-r>
            
            " File navigation
            nnoremap <leader>e :Explore<CR>
            
            " Buffer navigation
            nnoremap <leader>bn :bnext<CR>
            nnoremap <leader>bp :bprev<CR>
            nnoremap <leader>bd :bdelete<CR>
            
            " Highlight trailing whitespace
            highlight ExtraWhitespace ctermbg=red guibg=red
            match ExtraWhitespace /\s\+$/
            autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
            autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
            autocmd InsertLeave * match ExtraWhitespace /\s\+$/
            autocmd BufWinLeave * call clearmatches()
            
            " Auto-remove trailing whitespace
            autocmd BufWritePre * :%s/\s\+$//e
            
            " Remember cursor position
            autocmd BufReadPost *
              \ if line("'\"") > 0 && line("'\"") <= line("$") |
              \   exe "normal! g`\"" |
              \ endif
            
            " Auto-create directories when saving files
            autocmd BufWritePre * call mkdir(expand('<afile>:p:h'), 'p')
            
            " Better file type detection
            filetype plugin indent on
            
            " Color scheme (works well in most terminals)
            colorscheme desert
          '';
        };
      };
  };
}

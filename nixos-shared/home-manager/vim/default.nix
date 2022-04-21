{
  vim
}:

{
  value = {
    enable = true;
    extraConfig = ''
      " https://swordandsignals.com/2020/12/13/5-lines-in-vimrc.html
      set hlsearch    " highlight all search results
      set ignorecase  " do case insensitive search
      set incsearch   " show incremental search results as you type
      set number      " display line number
      set noswapfile  " disable swap file

      set t_te=       " keep output in terminal
    '';
  };
}

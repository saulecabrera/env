set nocompatible
filetype off

syntax on

" For plugins to load correctly
filetype plugin indent on

let data_dir = has('nvim') ? stdpath('data') . '/site' : '~/.vim'
if empty(glob(data_dir . '/autoload/plug.vim'))
  silent execute '!curl -fLo '.data_dir.'/autoload/plug.vim --create-dirs  https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin()
Plug 'Olical/aniseed'
Plug 'pechorin/any-jump.vim'
call plug#end()

let g:gruvbox_material_background = 'hard'
let g:gruvbox_material_disable_italic_comment = 1
let g:aniseed#env = v:true

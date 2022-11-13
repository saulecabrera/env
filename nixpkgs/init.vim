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
Plug 'rescript-lang/vim-rescript'
Plug 'sainnhe/everforest'
Plug 'evanleck/vim-svelte'
Plug 'whatyouhide/vim-gotham'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
call plug#end()


let mapleader = "\<Space>"
nnoremap <silent> <leader> :<c-u>WhichKey '<Space>'<CR>

"Clipboard
set clipboard=unnamedplus

" Security
set modelines=0

" Show line numbers
set number

" Show file stats
set ruler

" Blink cursor on error instead of beeping (grr)
set visualbell

" Encoding
set encoding=utf-8

" Mouse
set mouse=a

" Whitespace
set wrap
set textwidth=79
set formatoptions=tcqrn1
set tabstop=2
set shiftwidth=2
set softtabstop=2
set expandtab
set noshiftround

" Cursor motion
set scrolloff=3
set backspace=indent,eol,start
set matchpairs+=<:> " use % to jump between pairs
runtime! macros/matchit.vim

" Allow hidden buffers
set hidden

" Rendering
set ttyfast

" Status bar
set laststatus=2

" Last line
set showmode
set showcmd

" Searching
nnoremap / /\v
vnoremap / /\v
set hlsearch
set incsearch
set ignorecase
set smartcase
set showmatch

set guifont=Liberation\ Mono:h12

" Visualize tabs and newlines
set listchars=tab:▸\ ,eol:¬

" Color scheme (terminal)
" set t_Co=256
set termguicolors
colorscheme gruvbox
set background=dark
let g:gruvbox_contrast_dark='hard'
let g:gruvbox_italicize_comments=1
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#show_buffers = 0
let g:airline_powerline_fonts = 1
let g:airline_theme='gruvbox'
let g:solarized_termcolors=256
let g:goyo_width=120

highlight Comment cterm=italic gui=italic

let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"

" Files
map <silent> <leader>ff <cmd>:Files<cr>
map <silent> <leader>f/ <cmd>:Rg<cr>
map <silent> <leader>fr <cmd>:Buffers<cr>

" Git
map <silent> <leader>gs <cmd>:GitFiles?<cr>
map <silent> <leader>gc <cmd>:Commits<cr>
map <silent> <leader>gbc <cmd>:BCommits<cr>


" don't give |ins-completion-menu| messages.
set shortmess+=c

" always show signcolumns
set signcolumn=yes

" Remap keys for gotos
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window
nnoremap <silent> K :call <SID>show_documentation()<CR>

let g:netrw_localrmdir='rm -r'

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" Highlight symbol under cursor on CursorHold
autocmd CursorHold * silent call CocActionAsync('highlight')

" Folding
set foldmethod=indent
set foldnestmax=10
set nofoldenable
set foldlevel=2


nnoremap <silent> <leader>m :Neogit<CR>


" Windows

" Vertical
nnoremap <silent> <leader>wv <C-w>v
" Horizontal
nnoremap <silent> <leader>ws <C-w>s
" Cycle
nnoremap <silent> <leader>wn <C-W>w
" Right
nnoremap <silent> <leader>wl <C-w>l
" Left
nnoremap <silent> <leader>wh <C-w>h
" Down
nnoremap <silent> <leader>wj <C-w>j
" Up
nnoremap <silent> <leader>wk <C-w>k


nnoremap <silent> <leader>w/ <cmd>:Windows<CR>

" Hop
nnoremap gw :HopWord<cr>
nnoremap gl :HopLine<cr>
nnoremap gs :HopPattern<cr>


" Last Buffer
nnoremap <leader><tab> :e#<cr>

" Nicer save
nnoremap <leader>fs :w<cr>

" Neoterm
let g:neoterm_default_mod = 'vertical'
let g:neoterm_size = 100
nnoremap <silent> <leader>tt :Ttoggle<cr>
nnoremap <silent> <leader>tn :Tnew<cr>
autocmd TermOpen * setlocal nonumber norelativenumber

" Mappings

nnoremap <leader><leader> <cmd>:Maps<CR>
 
" Haskell
let g:haskell_enable_quantification = 1   " to enable highlighting of `forall`
let g:haskell_enable_recursivedo = 1      " to enable highlighting of `mdo` and `rec`
let g:haskell_enable_arrowsyntax = 1      " to enable highlighting of `proc`
let g:haskell_enable_pattern_synonyms = 1 " to enable highlighting of `pattern`
let g:haskell_enable_typeroles = 1        " to enable highlighting of type roles
let g:haskell_enable_static_pointers = 1  " to enable highlighting of `static`
let g:haskell_backpack = 1                " to enable highlighting of backpack keywords
let g:haskell_classic_highlighting = 1

function! CheckBackspace() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

inoremap <silent><expr> <TAB>
      \ coc#pum#visible() ? coc#pum#next(1):
      \ CheckBackspace() ? "\<Tab>" :
      \ coc#refresh()
inoremap <expr><S-TAB> coc#pum#visible() ? coc#pum#prev(1) : "\<C-h>"

" Make <CR> to accept selected completion item or notify coc.nvim to format
" <C-g>u breaks current undo, please make your own choice.
inoremap <silent><expr> <CR> coc#pum#visible() ? coc#pum#confirm()
                              \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"


lua << EOF
require'hop'.setup { keys = 'etovxqpdygfblzhckisuran', jump_on_sole_occurrence = false }

-- Load custom tree-sitter grammar for org filetype
require('orgmode').setup_ts_grammar()

-- Tree-sitter configuration
require'nvim-treesitter.configs'.setup {
  -- If TS highlights are not enabled at all, or disabled via `disable` prop, highlighting will fallback to default Vim syntax highlighting
  highlight = {
    enable = true,
    disable = {'org'}, -- Remove this to use TS highlighter for some of the highlights (Experimental)
    additional_vim_regex_highlighting = {'org'}, -- Required since TS highlighter doesn't support all syntax features (conceal)
  },
  ensure_installed = {'org'}, -- Or run :TSUpdate org
}

require('orgmode').setup({
  org_agenda_files = {'~/org/**/*'},
  org_default_notes_file = '~/org/notes.org',
})
EOF

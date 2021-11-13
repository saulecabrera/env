set nocompatible
filetype off

syntax on

" For plugins to load correctly
filetype plugin indent on

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

set guifont=JetBrains\ Mono:h12

" Visualize tabs and newlines
set listchars=tab:▸\ ,eol:¬

" Color scheme (terminal)
" set t_Co=256
set termguicolors
colorscheme gruvbox8_soft
" colorscheme base16-tomorrow-night
set background=dark
let g:gruvbox_contrast_dark='medium'
let g:gruvbox_italicize_comments=1
let g:airline_theme='base16'
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#show_buffers = 0
let g:solarized_termcolors=256

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

" Floaterm
nnoremap <leader>ftt <cmd>:FloatermToggle<CR>i
noremap! <leader>ftt <Esc>:FloatermToggle<CR>i
tnoremap <leader>ftt  <C-\><C-n>:FloatermToggle<CR>

" Mappings

nnoremap <leader><leader> <cmd>:Maps<CR>

" Vimwiki

let wiki = {}
let wiki.path = '~/vimwiki/'
let wiki.nested_syntaxes = {'bash': 'bash', 'haskell': 'haskell', 'rust': 'rust'}
let wiki.syntax = 'markdown'
let wiki.ext = 'md'
let g:vimwiki_list = [wiki]



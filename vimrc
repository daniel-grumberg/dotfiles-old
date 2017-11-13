"Pathogen stuff
execute pathogen#infect()
filetype plugin indent on


set number
set ruler
set relativenumber
set backspace=indent,eol,start
set noswapfile

" Return to last edit position when opening files (You want this!)
autocmd BufReadPost *
     \ if line("'\"") > 0 && line("'\"") <= line("$") |
     \   exe "normal! g`\"" |
     \ endif

" Delete trailing white space on save, useful for Python and CoffeeScript ;)
func! DeleteTrailingWS()
  exe "normal mz"
  %s/\s\+$//ge
  exe "normal `z"
endfunc
autocmd BufWrite * :call DeleteTrailingWS()

" Pressing \ss will toggle and untoggle spell checking
" map <leader>ss :setlocal spell! spelllang=en_gb<cr>
map <leader>ss :setlocal spell! spelllang=en_gb<cr>

" Treat long lines as break lines (useful when moving around in them)
vnoremap a <ESC>
inoremap jk <ESC>
inoremap kj <ESC>
map j gj
map k gk

"Proper tab handling
set tabstop=4
set softtabstop=4
set smarttab
set shiftwidth=4
set expandtab
set smartindent
"set cindent

function! HasPaste()
    if &paste
        return 'PASTE MODE  '
    en
    return ''
endfunction

"Sets the interface
"Enable syntax highlighting
"python from powerline.vim import setup as powerline_setup
"python powerline_setup()
"python del powerline_setup

syntax enable
set bg=dark
set t_Co=256
colorscheme gruvbox
set cc=120
set textwidth=120
set formatoptions+=t
set ch=2
set laststatus=2
set showtabline=2
set noshowmode
set hlsearch
set incsearch


set nocompatible

hi Tab cterm=underline
hi TrailSpace ctermbg=red

match TrailSpace /\s\+$/
autocmd BufWinEnter * match TrailSpace /\s\+$/
autocmd InsertEnter * match TrailSpace /\s\+\%#\@<!$/
autocmd InsertLeave * match TrailSpace /\s\+$/
autocmd BufWinLeave * call clearmatches()

autocmd BufRead,BufNewFile * syntax match Tab /\t/ containedin=ALL

"Makes the screen dark
"highlight Normal ctermbg=None

map j gj
map k gk
map 0 ^
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l


"Vimwiki stuff
let wiki_1 = {}
let wiki_1.path = '/Users/danielgrumberg/vimwiki'
let wiki_1.path_html = '/Users/danielgrumberg/vimwiki_html'
let wiki_1.auto_toc = 1
let g:vimwiki_list = [wiki_1]

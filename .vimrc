" vim-plug Installation
if empty(glob('~/.vim/autoload/plug.vim'))
  silent execute '!curl -fLo ~/.vim/autoload/plug.vim --create-dirs  https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" General settings
syntax on
filetype plugin indent on

set background=dark

set tabstop=4
set shiftwidth=4
set expandtab

" 2 spaces for some file types
autocmd FileType vim,yaml,json setlocal tabstop=2 shiftwidth=2 expandtab

set relativenumber
set number
set ai
set ignorecase
set smartcase
set incsearch
set cinoptions=l1
set hlsearch
set ruler


" Plugins
call plug#begin()
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
call plug#end()


" Key mappings
let mapleader = " "

" General
nnoremap <leader>rc :source $MYVIMRC<cr>

" Buffers
nnoremap <leader>bn :bn<cr>
nnoremap <leader>bp :bp<cr>
nnoremap <leader>bd :bd<cr>
nnoremap <leader>bD :bufdo bd<cr>

" Windows
nnoremap <leader>c :close<cr>
nnoremap <leader>o <C-w>w
nnoremap <leader>wv :vsplit<cr><C-w>x
nnoremap <leader>wh :split<cr><C-w>x
nnoremap <leader>w- :resize -5<cr>
nnoremap <leader>w+ :resize +5<cr>
nnoremap <leader>w< :vertical resize -5<cr>
nnoremap <leader>w> :vertical resize +5<cr>

" Tabs
nnoremap <leader>tn :tabnew<cr>
nnoremap <leader>to :tabonly<cr>
nnoremap <leader>tc :tabclose<cr>
nnoremap <leader>tm :tabmove<Space>
nnoremap <leader>th :tabprevious<cr>
nnoremap <leader>tl :tabnext<cr>

" System clipboard
vnoremap <leader>y "+y
nnoremap <leader>y "+y
nnoremap <leader>Y "+Y
nnoremap <leader>P "+p
vnoremap <leader>P "+p

" Code indentation (maintains visual selection)
vnoremap <leader>< <gv
vnoremap <leader>> >gv

" Quickfix list
nnoremap <leader>co :copen<cr>
nnoremap <leader>cc :cclose<cr>
nnoremap <leader>cn :cnext<cr>
nnoremap <leader>cp :cprevious<cr>

" Location list
nnoremap <leader>lo :lopen<cr>
nnoremap <leader>lc :lclose<cr>

" Terminal
nnoremap <leader>tt :terminal<cr><C-w>x<C-w>w
nnoremap <leader>tv :vertical terminal<cr><C-w>x<C-w>w

" Git
nnoremap <leader>gs :!git status<CR>
nnoremap <leader>gd :!git diff<CR>

" fzf.vim
map <leader>ff :Files<cr>
map <leader>fb :Buffers<cr>
map <leader>fw :Windows<cr>
map <leader>fm :Maps<cr>
map <leader>fr :Rg<cr>
map <leader>fl :BLines<cr>
map <leader>fc :Commands<cr>


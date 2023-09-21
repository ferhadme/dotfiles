set nocompatible
filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'VundleVim/Vundle.vim'
Plugin 'tpope/vim-commentary'
Plugin 'ycm-core/YouCompleteMe'

call vundle#end()

filetype plugin indent on

colorscheme pablo
syntax on
set tabstop=2
set shiftwidth=2
set expandtab
set noesckeys
set relativenumber
set number
set ignorecase
set smartcase
set incsearch
set autochdir
set cinoptions=l1
set hlsearch
set ruler
hi Search ctermbg=LightYellow
hi Search ctermfg=Red
highlight Comment ctermfg=green
map gn :bn<cr>
map gp :bp<cr>
map gd :bd<cr>
nnoremap <space> i<space><esc>l
set list
set listchars=tab:›\ ,trail:•,extends:#,nbsp:.
set encoding=utf-8

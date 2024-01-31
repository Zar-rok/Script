set number relativenumber

" tab = 2 space
set expandtab
set shiftwidth=2
set tabstop=2

" always show the status line
set laststatus=2

set termguicolors

call plug#begin('~/.local/share/nvim/plugged')

Plug 'tpope/vim-sensible'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'miikanissi/modus-themes.nvim'
Plug 'junegunn/fzf.vim'

" Dev

Plug 'tpope/vim-commentary'
Plug 'numirias/semshi', {'do': ':UpdateRemotePlugins'}
Plug 'Vimjas/vim-python-pep8-indent'

" Initialize plugin system
call plug#end()

let g:airline_theme='base16'

colorscheme modus_operandi

let g:python_host_prog = '/usr/bin/python'
let g:python3_host_prog = '/usr/bin/python3'

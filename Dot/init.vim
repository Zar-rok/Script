set number

" tab = 2 space
set expandtab
set shiftwidth=2
set tabstop=2

" always show the status line
set laststatus=2

set termguicolors

call plug#begin('~/.local/share/nvim/plugged')

Plug 'junegunn/goyo.vim'

Plug 'tpope/vim-sensible'

Plug 'morhetz/gruvbox'

Plug 'vim-airline/vim-airline'

Plug 'vim-airline/vim-airline-themes'

" Initialize plugin system
call plug#end()

colorscheme gruvbox

execute 'set background=' . (strftime('%H') < 12 ? 'light' : 'dark')

let g:airline_theme='base16'

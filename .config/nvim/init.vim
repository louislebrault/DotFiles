 " Specify a directory for plugins
" - For Neovim: stdpath('data') . '/plugged'
" - Avoid using standard Vim directory names like 'plugin'
call plug#begin('~/.vim/plugged')

Plug 'tomasiser/vim-code-dark'
Plug 'pangloss/vim-javascript'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'w0rp/ale'
Plug 'preservim/nerdtree'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
"marche pas ces 3 là
Plug 'vim-scripts/taglist.vim'
Plug 'tpope/vim-surround'
Plug 'idbrii/vim-ack'
" Initialize plugin system
call plug#end()

set foldmethod=indent
set shiftwidth=2
set nofoldenable
colorscheme codedark
set number relativenumber

let mapleader=' ' 

imap hh <ESC>

nmap <Leader>p :GFiles ./<CR>
nmap <Leader>t :NERDTreeToggle<CR>
nmap <Leader><Leader>w :w<CR>
nmap <Leader><Leader>q :q<CR>

" stylelint ca a l'air de bien pas marcher
let g:ale_fixers = {
 \ 'javascript': ['eslint'],
 \ 'css': ['stylelint']
 \ }
let g:ale_sign_error = '❌'
let g:ale_sign_warning = '⚠️'

let g:ale_fix_on_save = 1

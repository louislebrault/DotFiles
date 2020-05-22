" Specify a directory for plugins
" - For Neovim: stdpath('data') . '/plugged'
" - Avoid using standard Vim directory names like 'plugin'
call plug#begin('~/.vim/plugged')

Plug 'tomasiser/vim-code-dark'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'w0rp/ale'
Plug 'preservim/nerdtree'

" Initialize plugin system
call plug#end()

set foldmethod=indent
set shiftwidth=2
set nofoldenable
colorscheme codedark
set number

let mapleader='l' 

nnoremap h :
nnoremap <Leader>w <C-w>

nmap <Leader>p :GFiles ./<CR>
nmap <Leader>t :NERDTreeToggle<CR>

cabbrev gf GFiles ./ 
cabbrev nt NERDTreeToggle

" stylelint ca a l'air de bien pas marcher
let g:ale_fixers = {
 \ 'javascript': ['eslint'],
 \ 'css': ['stylelint']
 \ }
let g:ale_sign_error = '❌'
let g:ale_sign_warning = '⚠️'
let g:ale_fix_on_save = 1

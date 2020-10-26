 " Specify a directory for plugins
" - For Neovim: stdpath('data') . '/plugged'
" - Avoid using standard Vim directory names like 'plugin'
call plug#begin('~/.vim/plugged')

Plug 'tomasiser/vim-code-dark'
Plug 'pangloss/vim-javascript'
Plug 'maxmellon/vim-jsx-pretty'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'w0rp/ale'
Plug 'preservim/nerdtree'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'mg979/vim-visual-multi', {'branch': 'master'}
Plug 'zivyangll/git-blame.vim'
" Initialize plugin system
call plug#end()

set foldmethod=indent
set shiftwidth=2
set nofoldenable
colorscheme codedark
set number relativenumber
set autochdir

let mapleader=' ' 

imap hh <ESC>

nmap <Leader>p :ProjectFiles<CR>
nmap <Leader>t :NERDTreeToggle<CR>
nmap <Leader><Leader>w :w<CR>
nmap <Leader><Leader>q :q<CR>

function! s:find_git_root()
  return system('git rev-parse --show-toplevel 2> /dev/null')[:-2]
endfunction

command! ProjectFiles execute 'Files' s:find_git_root()

" stylelint ca a l'air de bien pas marcher
let g:ale_fixers = {
 \ 'javascript': ['eslint'],
 \ 'css': ['stylelint']
 \ }
let g:ale_sign_error = '❌'
let g:ale_sign_warning = '⚠️'

let g:ale_fix_on_save = 1

" remove trailling whitespaces on save
autocmd BufWritePre * %s/\s\+$//e

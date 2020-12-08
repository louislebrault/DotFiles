 " Specify a directory for plugins
" - For Neovim: stdpath('data') . '/plugged'
" - Avoid using standard Vim directory names like 'plugin'
call plug#begin('~/.vim/plugged')

Plug 'pangloss/vim-javascript'
Plug 'maxmellon/vim-jsx-pretty'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'zivyangll/git-blame.vim'
Plug 'airblade/vim-gitgutter'
Plug 'itchyny/lightline.vim' " status bar
Plug 'morhetz/gruvbox' " colorscheme
Plug 'SirVer/ultisnips' " snippets engine
Plug 'tpope/vim-surround'

" Initialize plugin system
call plug#end()

set foldmethod=indent
set shiftwidth=2
set nofoldenable
colorscheme gruvbox
set number relativenumber
set tags=tags; " look for tags file
set autoindent

autocmd BufWritePre * %s/\s\+$//e " remove trailling whitespaces on save

set tabstop=2 shiftwidth=2 expandtab " use spaces for tabs

set colorcolumn=120 " show when line exceed 120 char

let mapleader=' '

nmap <Leader>p :GFiles<CR>
nmap <Leader>f :Files<CR>
nmap <Leader>t :Tags<CR>
nmap <Leader><Leader>t :BTags<CR>
" show fuzzy history. as H is nearly a home row, consider use it for a more
" common action in the future tho
nmap <Leader>h :History<CR>
nmap <Leader>` :w<CR>
nmap <Leader><Leader>q :q<CR>
nmap <Leader><Leader><Leader>q :q!<CR>
" switch split focus
map <Leader>w <C-W>w
" open new vertical split
nmap <Leader>s :vsp<CR>
" open new tab
nmap <Leader>o :tabnew<CR>
" switch tab focus
nmap <Leader>n :tabn<CR>

"======================
" :Ag on cursor
noremap <Leader>a  :Ag! <C-r>=Escape(expand('<cword>'))<CR><CR>

function! Escape(stuff)
    return substitute(escape(a:stuff, '\/.*$^~[]'), "\n", '\\n', "g")
endfunction
"=======================

" lightline theme
let g:lightline = {
      \ 'colorscheme': 'gruvbox',
      \ }

let g:UltiSnipsSnippetDirectories=["UltiSnips", 'mysnippets']
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<s-tab>"

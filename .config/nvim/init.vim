 " Specify a directory for plugins
" - For Neovim: stdpath('data') . '/plugged'
" - Avoid using standard Vim directory names like 'plugin'
call plug#begin('~/.vim/plugged')

Plug 'sheerun/vim-polyglot' " syntax highlight and indent for all languages
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'dense-analysis/ale' " Linting
Plug 'zivyangll/git-blame.vim'
Plug 'airblade/vim-gitgutter'
Plug 'itchyny/lightline.vim' " status bar
Plug 'morhetz/gruvbox' " colorscheme
Plug 'SirVer/ultisnips' " snippets engine
Plug 'tpope/vim-surround'
Plug 'ludovicchabant/vim-gutentags' " automatic ctags generation

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
nmap <Leader>g :BTags<CR>
" show fuzzy history. as H is nearly a home row, consider use it for a more
" common action in the future tho
nmap <Leader>h :History<CR>
nmap <Leader>` :w<CR>
nmap <F4> :w<CR>
nmap <F6> :q<CR>
inoremap <F8> <ESC>:w<CR>
inoremap <F9> <ESC>:wq<CR>
nmap <Leader>e :Tex<CR>
" switch split focus
map <Leader>w <C-W>w
" open new vertical split
nmap <Leader>v :vsp<CR>
" open new tab
nmap <Leader>T :tabnew<CR>
" switch tab focus
nmap <Leader>n :tabn<CR>
" Quickly insert an empty new line without entering insert mode
nnoremap <Leader>o o<Esc>
nnoremap <Leader>O O<Esc>

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

" ======== Ale
let g:ale_linters = {
\  'javascript': ['eslint'],
\  'typescript': ['eslint', 'tslint'],
\  'vue': ['eslint'],
\}

let g:ale_fixers = {
\  'javascript': ['eslint'],
\  'typescript': ['eslint', 'tslint'],
\  'vue': ['eslint'],
\}

" Fix files automatically on save
let g:ale_fix_on_save = 1
" Coc is on charge of LSP, this line may need to be moved at the top of the
" file ? https://github.com/dense-analysis/ale#faq-coc-nvim
let g:ale_disable_lsp = 1
" Only run linters named in ale_linters settings.
let g:ale_linters_explicit = 1

" =========
" trying to make gutentag more lightweight, dunno how effective this is
let g:gutentags_ctags_exclude = [
      \ '*.git', '*.svg', '*.hg', '*.vue',
      \ '*/tests/*',
      \ 'build',
      \ 'dist',
      \ '*sites/*/files/*',
      \ 'bin',
      \ 'node_modules',
      \ 'bower_components',
      \ 'cache',
      \ 'compiled',
      \ 'docs',
      \ 'example',
      \ 'bundle',
      \ 'vendor',
      \ '*.md',
      \ '*-lock.json',
      \ '*.lock',
      \ '*bundle*.js',
      \ '*build*.js',
      \ '.*rc*',
      \ '*.json',
      \ '*.min.*',
      \ '*.map',
      \ '*.bak',
      \ '*.zip',
      \ '*.pyc',
      \ '*.class',
      \ '*.sln',
      \ '*.Master',
      \ '*.csproj',
      \ '*.tmp',
      \ '*.csproj.user',
      \ '*.cache',
      \ '*.pdb',
      \ 'tags*',
      \ 'cscope.*',
      \ '*.css',
      \ '*.less',
      \ '*.scss',
      \ '*.exe', '*.dll',
      \ '*.mp3', '*.ogg', '*.flac',
      \ '*.swp', '*.swo',
      \ '*.bmp', '*.gif', '*.ico', '*.jpg', '*.png',
      \ '*.rar', '*.zip', '*.tar', '*.tar.gz', '*.tar.xz', '*.tar.bz2',
      \ '*.pdf', '*.doc', '*.docx', '*.ppt', '*.pptx',
      \ ]

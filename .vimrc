"The basic settings
set nocp
set ls=2
set expandtab
set tabstop=2
set shiftwidth=2
set softtabstop=2
set ruler
set number
set ignorecase
set modeline
set autoindent
set nobackup
set wrap
set hidden
set backspace=indent,eol,start

" Syntax highlighting
syntax on

" Yes filetype matter
filetype plugin on

" Set a nice Omnifunc - <CTRL>X <CTRL>O
set ofu=syntaxcomplete#Complete

" Markdown
au BufRead,BufNewFile *.md set filetype=markdown
au BufRead,BufNewFile GHI_ISSUE set filetype=markdown

" JavaScript
au BufRead,BufNewFile *.jsx set filetype=javascript.jsx

" Common Lisp
au BufRead,BufNewFile *.asd set filetype=lisp
au BufRead,BufNewFile *.ros set filetype=lisp

" ECT
au BufRead,BufNewFile *.ect set filetype=html

" PHP
au BufRead,BufNewFile *.php setfiletype php


" You can change colors easily in vim. 
" Just type <ESC>:colorscheme and then TAB complete through the options 
" # colorscheme desert
colorscheme default
set background=dark

" Set the color for the popup menu
:highlight Pmenu ctermbg=blue ctermfg=white
:highlight PmenuSel ctermbg=blue ctermfg=red
:highlight PmenuSbar ctermbg=cyan ctermfg=green
:highlight PmenuThumb ctermbg=white ctermfg=red

" Make vim popup behave more like an IDE POPUP
set completeopt=longest,menuone

" Make enter finish the completion popup menu
inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"

set nocompatible
filetype off
filetype plugin indent off

if has('vim_starting')
  set runtimepath+=~/.vim/neobundle/neobundle.vim
endif

call neobundle#begin(expand('~/.vim/neobundle/'))

NeoBundleFetch 'Shougo/neobundle.vim'

NeoBundle 'Shougo/vimshell'
NeoBundle 'Shougo/unite.vim'
" NeoBundle 'scrooloose/syntastic'
NeoBundle 'pmsorhaindo/syntastic-local-eslint.vim'
NeoBundle 'kchmck/vim-coffee-script'
NeoBundle 'plasticboy/vim-markdown'
NeoBundle 'thinca/vim-quickrun'
NeoBundle 'Shougo/vimproc.vim', {
      \ 'build' : {
      \     'windows' : 'tools\\update-dll-mingw',
      \     'cygwin' : 'make -f make_cygwin.mak',
      \     'mac' : 'make -f make_mac.mak',
      \     'linux' : 'make',
      \     'unix' : 'gmake',
      \    },
      \ }
NeoBundle 'thinca/vim-unite-history'
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'kana/vim-submode'
NeoBundle 'fatih/vim-go'
NeoBundle 'dgryski/vim-godef'
NeoBundle 'vim-jp/vim-go-extra'
NeoBundle 'toyamarinyon/vim-swift'
NeoBundle 'glidenote/memolist.vim'
NeoBundle 'kana/vim-operator-user'
NeoBundle 'rhysd/vim-clang-format'
NeoBundle 'cypok/vim-sml'
NeoBundle 'pangloss/vim-javascript'
NeoBundle 'mxw/vim-jsx'
NeoBundle 'https://github.com/kovisoft/slimv'
NeoBundle 'leafgarland/typescript-vim'
NeoBundle 'posva/vim-vue'
NeoBundleLazy 'udalov/kotlin-vim', {
    \ 'autoload' : {'filetypes' : 'kotlin'}
    \ }

call neobundle#end()

filetype on
filetype plugin on
filetype indent on

let g:syntastic_check_on_open = 1
let g:syntastic_mode_map = { 'mode': 'passive',
      \ 'active_filetypes': ['ruby', 'javascript'] }
let g:syntastic_javascript_checkers=['eslint']

" insert modeで開始
let g:unite_enable_start_insert = 1

" grep検索
nnoremap <silent> ,g  :<C-u>Unite grep:. -buffer-name=search-buffer<CR>

" カーソル位置の単語をgrep検索
nnoremap <silent> ,cg :<C-u>Unite grep:. -buffer-name=search-buffer<CR><C-R><C-W>

" grep検索結果の再呼出
nnoremap <silent> ,r  :<C-u>UniteResume search-buffer<CR>

" unite grep に ag(The Silver Searcher) を使う
if executable('ag')
  let g:unite_source_grep_command = 'ag'
  let g:unite_source_grep_command = 'ag'
  let g:unite_source_grep_default_opts =
        \ '-i --vimgrep --hidden --ignore ' .
        \ '''.hg'' --ignore ''.svn'' --ignore ''.git'' --ignore ''.bzr'''
  let g:unite_source_grep_recursive_opt = ''
endif

" Use goimports
let g:go_fmt_command = "goimports"

" aliases
nnoremap s <Nop>
nnoremap sj <C-w>j
nnoremap sk <C-w>k
nnoremap sl <C-w>l
nnoremap sh <C-w>h
nnoremap sJ <C-w>J
nnoremap sK <C-w>K
nnoremap sL <C-w>L
nnoremap sH <C-w>H
nnoremap sn gt
nnoremap sp gT
nnoremap sr <C-w>r
nnoremap s= <C-w>=
nnoremap sw <C-w>w
nnoremap so <C-w>_<C-w>|
nnoremap sO <C-w>=
nnoremap sN :<C-u>bn<CR>
nnoremap sP :<C-u>bp<CR>
nnoremap st :<C-u>tabnew<CR>
nnoremap sT :<C-u>Unite tab<CR>
nnoremap ss :<C-u>sp<CR>
nnoremap sv :<C-u>vs<CR>
nnoremap sq :<C-u>q<CR>
nnoremap sQ :<C-u>bd<CR>
nnoremap ,b :<C-u>Unite buffer -buffer-name=file<CR>
nnoremap ,f :<C-u>Unite file<CR>

call submode#enter_with('bufmove', 'n', '', 's>', '<C-w>>')
call submode#enter_with('bufmove', 'n', '', 's<', '<C-w><')
call submode#enter_with('bufmove', 'n', '', 's+', '<C-w>+')
call submode#enter_with('bufmove', 'n', '', 's-', '<C-w>-')
call submode#map('bufmove', 'n', '', '>', '<C-w>>')
call submode#map('bufmove', 'n', '', '<', '<C-w><')
call submode#map('bufmove', 'n', '', '+', '<C-w>+')
call submode#map('bufmove', 'n', '', '-', '<C-w>-')

" memo
let g:memolist_memo_suffix = "md"

" QuickRun
let g:quickrun_config = {
      \   "_" : {
      \       "outputter/buffer/split": ":botright",
      \       "outputter/buffer/close_on_empty": 1
      \   }
      \}

autocmd FileType c ClangFormatAutoEnable

let g:jsx_ext_required = 0

" Common Lisp

let g:slimv_lisp = 'ros run'
let g:silmv_impl = 'sbcl'
nnoremap <silent> ,cl :VimShellInteractive ros -s swank -e '(swank:create-server :port 4005 :dont-close t)' wait<CR>
let g:paredit_disable_lisp = 1
let g:paredit_electric_return = 0

" ESC

imap <C-e> <ESC>
vmap <C-e> <ESC>
cmap <C-e> <C-c>
imap <C-k> <ESC>
vmap <C-k> <ESC>
cmap <C-k> <C-c>

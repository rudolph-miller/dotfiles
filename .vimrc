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

" Ruby
autocmd FileType ruby setlocal expandtab tabstop=2 shiftwidth=2 softtabstop=2
" YAML
autocmd FileType yaml setlocal expandtab tabstop=2 shiftwidth=2 softtabstop=2
" ERUBY
autocmd FileType eruby setlocal expandtab tabstop=2 shiftwidth=2 softtabstop=2
" HAML
autocmd FileType haml setlocal expandtab tabstop=2 shiftwidth=2 softtabstop=2

" CoffeeScript, JavaScript
autocmd FileType coffee,javascript setlocal shiftwidth=2 softtabstop=2 tabstop=2 expandtab

"Syntax highlighting
syntax on

"Yes filetype matter
filetype plugin on

"Set a nice Omnifunc - <CTRL>X <CTRL>O
set ofu=syntaxcomplete#Complete


"Set some nice java functions - <CTRL>X <CTRL>U
set completefunc=javacomplete#Complete

"Make javac the build prog - :make
"You will need to change this per project to account for libs..
"Choose on of the following for starters
"YOU MUST start vim from the 'src/' folder. or javac wont work..

"This is the simplest possible
autocmd Filetype java set makeprg=javac\ %

"Coffee script
au BufRead,BufNewFile *.coffee set filetype=coffee
au BufNewFile,BufReadPost *.coffee setl shiftwidth=2 expandtab
autocmd FileType coffee setlocal sw=2 sts=2 ts=2 et

"node
au BufRead,BufNewFile *.js set filetype=node
au BufNewFile,BufReadPost *.js setl shiftwidth=2 expandtab
autocmd FileType node setlocal sw=2 sts=2 ts=2 et

"Cakefile
au BufRead,BufNewFile Cakefile set filetype=coffee
au BufNewFile,BufReadPost Cakefile setl shiftwidth=2 expandtab
autocmd FileType coffee setlocal sw=2 sts=2 ts=2 et

"Go
au BufRead,BufNewFile *.go set filetype=go
au BufNewFile,BufReadPost *.go setl shiftwidth=2 expandtab
autocmd FileType go setlocal sw=2 sts=2 ts=2 et

"Markdown
au BufRead,BufNewFile *.md set filetype=markdown
au BufRead,BufNewFile GHI_ISSUE set filetype=markdown

"HTML
au BufRead,BufNewFile *.html set filetype=html
au BufNewFile,BufReadPost *.html setl shiftwidth=2 expandtab
autocmd FileType html setlocal sw=2 sts=2 ts=2 et

"TEML
au BufRead,BufNewFile *.tmpl set filetype=html
au BufNewFile,BufReadPost *.tmpl setl shiftwidth=2 expandtab
autocmd FileType html setlocal sw=2 sts=2 ts=2 et

"This is a good default one - works for projects without libs
"autocmd Filetype java set makeprg=javac\ -d\ ../build/\ %

"Mapped some FUNCTION keys to be more useful..
map <F7> :make<Return>:copen<Return>
map <F8> :cprevious<Return>
map <F9> :cnext<Return>

"This is a nice buffer switcher
:nnoremap <F5> :buffers<CR>:buffer<Space>

" These are useful when using MinBufExpl
" BUT the CTRL+ARROW key mappings are still wrong on Terminal IDE soft Keyboard..
" This will only work over telnet/ssh . Fix Soon.
"let g:miniBufExplMapWindowNavVim = 1
"let g:miniBufExplMapWindowNavArrows = 1

"You can change colors easily in vim. 
"Just type <ESC>:colorscheme and then TAB complete through the options 
colorscheme desert
set background=dark

"Set the color for the popup menu
:highlight Pmenu ctermbg=blue ctermfg=white
:highlight PmenuSel ctermbg=blue ctermfg=red
:highlight PmenuSbar ctermbg=cyan ctermfg=green
:highlight PmenuThumb ctermbg=white ctermfg=red

" DICTIONARY
" The dictioanry can pop up a lot of words when you have Auto popup enabled. 
" You can disable auto popup, by removing the acp.vim from your ~/.vim/plugin/
" directory and enable the dictionary here - then use <CTRL>X <CTRL>K to bring
" up the dictionary options. Or just enable it.. :-)
"set dictionary+=~/system/etc/dict/words

" Make vim popup behave more like an IDE POPUP
set completeopt=longest,menuone

" Make enter finish the completion popup menu
inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"

"Auto start NERDTree on startup..
"autocmd VimEnter * NERDTree
"autocmd VimEnter * wincmd p

"TAGLIST setup
nnoremap <F3> :TlistToggle<CR>
let Tlist_Use_Right_Window = 1
let Tlist_WinWidth = 50

" noremap ; :
" noremap : ;
inoremap <C-k> <ESC>

set nocompatible
filetype off
filetype plugin indent off

if has('vim_starting')
  set runtimepath+=~/.vim/neobundle/neobundle.vim
endif

call neobundle#begin(expand('~/.vim/neobundle/'))

NeoBundleFetch 'Shougo/neobundle.vim'

call neobundle#end()


filetype on
filetype plugin on
filetype indent on

NeoBundle 'vcscommand.vim'
NeoBundle 'Shougo/vimshell'
NeoBundle 'Shougo/unite.vim'
NeoBundle 'Shougo/neocomplcache'
NeoBundle 'Shougo/neosnippet-snippets'
NeoBundle 'scrooloose/syntastic'
NeoBundle 'kchmck/vim-coffee-script'
NeoBundle 'jelera/vim-javascript-syntax'
NeoBundle '5t111111/neat-json.vim'
NeoBundle 'dgryski/vim-godef'
NeoBundle 'vim-jp/vim-go-extra'
NeoBundle 'plasticboy/vim-markdown'
NeoBundle 'kannokanno/previm'
NeoBundle 'tyru/open-browser.vim'
NeoBundle 'https://bitbucket.org/kovisoft/slimv'
NeoBundle 'mattn/emmet-vim'
NeoBundle 'thinca/vim-quickrun'
NeoBundle 'rust-lang/rust.vim'

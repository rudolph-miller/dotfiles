map <C-n> ;!sbcl --script %<CR>
map <C-b> ;!sbcl --load %<CR>


function! ToggleKeymap1 ()
  inoremap ( 9
  inoremap 9 (
  inoremap ) 0
  inoremap 0 )
endfunction

function! ToggleKeymap2 ()
  inoremap ( (
  inoremap 9 9
  inoremap ) )
  inoremap 0 0
endfunction

:call ToggleKeymap1()

let g:paredit_mode=0
let g:slimv_repl_split = 4
let g:slimv_lisp='/usr/local/bin/sbcl'
let g:slimv_swank_cmd = '!osascript ~/.swank/start-swank.scpt'
let g:slimv_clhs_root = 'file://$HOME/dev/HyperSpec/Body/'

map ,q ;!osascript ~/.swank/stop-swank.scpt<CR>,c
map ,w ,c<CR>

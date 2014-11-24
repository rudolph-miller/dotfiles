map <C-m> ;!content="" && clang % -o %.out.tmp && ./%.out.tmp \| while read buf; do content=$content$buf"\n"; done; rm %.out.tmp && echo $content<CR>

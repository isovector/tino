" set anti
" set lines=999 columns=84

" Open Command+T in a new tab
let g:CommandTAcceptSelectionTabMap='<CR>'

" C-TAB and C-SHIFT-TAB cycle tabs forward and backward
nmap <c-tab> :tabnext<cr>
imap <c-tab> <c-o>:tabnext<cr>
vmap <c-tab> <c-o>:tabnext<cr>
nmap <c-s-tab> :tabprevious<cr>
imap <c-s-tab> <c-o>:tabprevious<cr>
vmap <c-s-tab> <c-o>:tabprevious<cr>
nnoremap ZZ <nop>

" C-# switches to tab
nmap <d-1> 1gt
nmap <d-2> 2gt
nmap <d-3> 3gt
nmap <d-4> 4gt
nmap <d-5> 5gt
nmap <d-6> 6gt
nmap <d-7> 7gt
nmap <d-8> 8gt
nmap <d-9> 9gt

set guifont=Monospace:h9
" set guifont=PragmataPro\ 16
set guifont=DejaVu\ Sans\ Mono:14
" set guifont=Monospace\ 11
" set guifont=Monospace\ 9

" function! SetStreaming()
"   set guifont=Inconsolata:h20
" endfunction

nnoremap ZZ <nop>
vnoremap ZZ <nop>

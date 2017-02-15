nnoremap <buffer> <C-n> ggO<ESC>20i-<ESC>:r! date<CR>2o<ESC>i
nnoremap <buffer> <C-j> j/-----<CR>n:noh<CR>0
nnoremap <buffer> <C-k> k/-----<CR>N:noh<CR>0

au FocusLost *.db :wa

set cc=81 tw=80 fo=cqt
setf calendar

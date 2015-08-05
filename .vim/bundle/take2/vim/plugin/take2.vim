augroup plugin-take2
  autocmd!
  autocmd CursorMoved *  call recording#onActivity()
  autocmd CursorMovedI * call recording#onActivity()
  autocmd FocusGained * call recording#onActivity()

  autocmd FocusLost * call recording#onIdle()
  autocmd VimEnter * call recording#onEnter()
  autocmd VimLeave * call recording#onExit()
augroup END

command! -bang -nargs=* -complete=file Take2Coeff call interface#showCoeff()


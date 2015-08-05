
python << endpython
import vim
import xmlrpclib
import os

accio = xmlrpclib.ServerProxy("http://localhost:7432/")

def pullCoeffForFile(file = None):
    """
    Generate a vim list and pipe it to lgetexpr corresponding to the file's top
    coefficient matches.
    """
    if file == None:
        file = vim.current.buffer.name

    error = False
    coeffs = None

    pwd = vim.eval("getcwd()")

    try:
        coeffs = accio.coefficients(file)
        if coeffs == False or len(coeffs) == 0:
            error = True
    except:
        error = True

    if error:
        vim.command("echoe \"No coefficients found for %s\"" % file)
        return

    qfix = map(lambda x: os.path.relpath(x[0], pwd) + ":1: " + str(x[1]), coeffs)
    qfix = "\", \"".join(qfix)
    qfix = "[\"%s\"]" % qfix

    vim.command("let l:oldformat=&errorformat")
    vim.command("set errorformat=%f:%l:%m")
    vim.command("lgetexpr %s" % qfix)
    vim.command("set errorformat=l:oldformat")
endpython


function! interface#showCoeff()
python << endpython
pullCoeffForFile()
endpython
    redraw!
    lopen

    " Shamelessly copied from https://github.com/rking/ag.vim/blob/master/autoload/ag.vim
    nnoremap <silent> <buffer> h  <C-W><CR><C-w>K
    nnoremap <silent> <buffer> H  <C-W><CR><C-w>K<C-w>b
    nnoremap <silent> <buffer> o  <CR>
    nnoremap <silent> <buffer> t  <C-w><CR><C-w>T
    nnoremap <silent> <buffer> T  <C-w><CR><C-w>TgT<C-W><C-W>
    nnoremap <silent> <buffer> v  <C-w><CR><C-w>H<C-W>b<C-W>J<C-W>t
    nnoremap <silent> <buffer> e  <CR><C-w><C-w>:lclose<CR>
    nnoremap <silent> <buffer> go <CR>:lopen<CR>
    nnoremap <silent> <buffer> q  :lclose<CR>

    nnoremap <silent> <buffer> gv :let b:height=winheight(0)<CR><C-w><CR><C-w>H:lopen<CR><C-w>J:exe printf(":normal %d\<lt>c-w>_", b:height)<CR>
endfunction


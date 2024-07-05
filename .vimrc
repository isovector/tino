autocmd!
set nocompatible

set shortmess+=I

set rtp +=~/.vim

let output = system("xrandr | grep '^HDMI-1 disconnected'")
if v:shell_error != 0
  " hdmi is connected
  set guifont=Liberation\ Mono:h7
else
  set guifont=Liberation\ Mono:h7
endif

" if exists('g:GtkGuiLoaded')
"   call rpcnotify(1, 'Gui', 'Font', 'Source Code Pro 8')
"   call rpcnotify(1, 'Gui', 'Option', 'Tabline', 0)
" endif
" set guifont=Hack:h8

if !has('nvim')
  set shell=/usr/bin/zsh\ -l
endif

" ------------------------------------------------------------------------------
                               " Path Extensions
" ------------------------------------------------------------------------------
let $PATH = $PATH . ':' . expand('~/.local/bin')
let $PATH = $PATH . ':' . expand('~/.cabal/bin')
let $PATH = $PATH . ':' . expand('~/.ghcup/bin')



" ------------------------------------------------------------------------------
                              " Plugin Management
" ------------------------------------------------------------------------------
call plug#begin('~/.vim/plugged')

" Development
Plug 'isovector/ghci.vim', { 'for': 'haskell' }
Plug 'isovector/cornelis'

" Libraries
Plug 'vim-scripts/L9'
Plug 'xolox/vim-misc'
Plug 'tomtom/tlib_vim'
Plug 'MarcWeber/vim-addon-mw-utils'
Plug 'kana/vim-submode'
Plug 'vim-scripts/ingo-library'
Plug 'Shougo/vimproc'

" Silent
Plug 'rking/ag.vim'
Plug 'tpope/vim-repeat'
Plug 'majutsushi/tagbar'
Plug 'vim-scripts/vim-lamdify'
Plug 'junegunn/rainbow_parentheses.vim'
Plug 'ap/vim-buftabline'
" Plug 'yssl/QFEnter'

" Navigation
Plug 'ctrlpvim/ctrlp.vim'

" Misc
" Plug 'Shougo/neosnippet'

" system clipboard
Plug 'vim-scripts/tregisters'

" git
Plug 'tpope/vim-fugitive'

" block-mode visual
Plug 'vim-scripts/vis'

" snake case to kebab case
" Plug 'tpope/vim-abolish'

" autocomment
Plug 'tpope/vim-commentary'

" align character columns
Plug 'junegunn/vim-easy-align'
"

" Languages
Plug 'neovimhaskell/haskell-vim'
Plug 'vim-scripts/latex-support.vim'
Plug 'rstacruz/sparkup'
Plug 'scalameta/nvim-metals'
Plug 'BeneCollyridam/futhark-vim'

" Colors
Plug 'altercation/vim-colors-solarized'
Plug 'ptrr/phd-vim'
Plug 'ciaranm/inkpot'
Plug 'ajh17/Spacegray.vim'
Plug 'nanotech/jellybeans.vim'
Plug 'ronny/birds-of-paradise.vim'
Plug 'ratazzi/blackboard.vim'
Plug 'justincampbell/vim-railscasts'
Plug 'vim-scripts/leo256'
Plug 'w0ng/vim-hybrid'
Plug 'ap/vim-css-color'
Plug 'Valloric/vim-valloric-colorscheme'
Plug 'lanox/lanox-vim-theme'
Plug 'pkukulak/idle'
Plug 'mhartington/oceanic-next'
Plug 'geetarista/ego.vim'
Plug 'monkoose/boa.vim'
Plug 'euclio/vim-nocturne'
Plug 'fenetikm/falcon'
Plug 'fielding/vice'

" Textobjs
" expanded %
Plug 'tmhedberg/matchit'
Plug 'pilgrimlyieu/vim-surround'
Plug 'kana/vim-textobj-user'
Plug 'kana/vim-textobj-syntax'

Plug 'rbonvall/vim-textobj-latex'
Plug 'Julian/vim-textobj-variable-segment'
Plug 'deris/vim-shot-f'
Plug 'michaeljsmith/vim-indent-object'
Plug 'justinmk/vim-sneak'

" Plug 'neoclide/coc.nvim', {'branch': 'master', 'do': 'yarn install --frozen-lockfile'}

" cornelis
Plug 'neovimhaskell/nvim-hs.vim'
Plug 'liuchengxu/vim-which-key'

" light colos
Plug 'NLKNguyen/papercolor-theme'
Plug 'rakr/vim-one'

Plug 'smjonas/live-command.nvim'

" let g:cornelis_max_size = 19
" let g:cornelis_split_direction = 'Vertical'
call plug#end()


syntax on
let g:solarized_termcolors=256
set t_Co=256
set background=light

let g:coc_enable_locationlist=0



" ------------------------------------------------------------------------------
                                   " Leaders
" ------------------------------------------------------------------------------
let mapleader = " "
let maplocalleader = "\\"

" Automatic pane split layouts
" nnoremap <leader>3 :vsplit<CR>:bn<CR>:vsplit<CR>:bn<CR>
" nnoremap <leader>4 :vnew<CR>:bn<CR>:vnew<CR>:bn<CR><C-W><C-L><C-W><C-L>:split<CR>:bn<CR>
nnoremap <leader>sv :vert sb  <BS>

nnoremap <leader>zl :silent! s//<cr>:noh<cr>I<esc>:w<cr>02w
nnoremap <leader>zr O <c-u>-- $>  <bs>

nnoremap zC zc

" Easier access to commands
nnoremap <leader>m :! (cd `git rev-parse --show-toplevel`; make)<CR>
nnoremap <leader>g :silent! grep!  <BS>
nnoremap K :silent! grep! <cword><CR>:copen<CR>
nnoremap <leader>f ::CtrlPMRUFiles<CR>
nnoremap <leader>l :TagbarToggle<CR>

nnoremap <leader>wc :! wc %<CR>
nnoremap zb z=1<CR><CR>

" Todo management
nmap     <leader>td OTODO(sandy): <ESC>gccA
nnoremap <silent> <leader>wtd :lgrep! TODO.sandy<CR>:lw<CR>

" New functionality
noremap  <leader>mv :call RenameFile()<cr>
nnoremap <leader>sla V:s/\(\)/\1\r/<Left><Left><Left><Left><Left><Left><Left><Left>
nnoremap <leader>slb V:s/\(\)/\r\1/<Left><Left><Left><Left><Left><Left><Left><Left>
vnoremap <leader>sl :sort<Cr>gv:! awk '{ print length(), $0 \| "sort -n \| cut -d\\  -f2-" }'<CR>

nnoremap <silent> <leader>sT :set tags=<C-R>=system("git rev-parse --show-toplevel")<CR><BS>/tags<CR>
nnoremap <silent> <leader>st :! (cd `git rev-parse --show-toplevel`; hasktags **/*.hs)<CR>:set tags=<C-R>=system("git rev-parse --show-toplevel")<CR><BS>/tags<CR>
nnoremap <silent> <leader>sgt :! (cd `git rev-parse --show-toplevel`; hasktags compiler/**/*.hs)<CR>:set tags=<C-R>=system("git rev-parse --show-toplevel")<CR><BS>/ctags<CR>

function! MkHsModuleName(dir, path)
  return substitute(a:path[], '/', '.', '')
endfunction

function! HsModuleName()
  return MkHsModuleName(system("git rev-parse --show-toplevel"), expand('%:p'))
endfunction

nnoremap <silent> <leader>mk :normal! ggimodule <C-R>=HsModuleName()<CR> where<CR>





nnoremap <leader>b :Git blame<CR>

" things i stole from chris penner
nnoremap c "_c

let g:vim_markdown_folding_disabled=1
let g:vim_markdown_conceal=0
let g:tex_conceal = ""



" ------------------------------------------------------------------------------
                                " Local Leaders
" ------------------------------------------------------------------------------
" TODO(sandy): Add infrastructure to only enable these in certain filetypes

" Create banners in vimrc
nmap <localleader>wb O<ESC>78i-<ESC>gccyyjpk<CR>center<CR>gcc



" ------------------------------------------------------------------------------
                              " Quick Directories
" ------------------------------------------------------------------------------
nnoremap <leader>cd :cd %:p:h<CR>
nnoremap <leader>cp :cd ~/prj/
nnoremap <leader>ct :cd ~/.tino/

" ------------------------------------------------------------------------------
                                 " Quick Edits
" ------------------------------------------------------------------------------
nnoremap <leader>ev :e ~/.vimrc<cr>
nnoremap <leader>et :e ~/.tino/bin/tino<cr>
nnoremap <leader>el :e ~/.vimrc.local<cr>
nnoremap <leader>ea :e ~/.tino/zsh/aliases.zsh<cr>
nnoremap <leader>ex :e ~/.config/xmonad/xmonad.hs<cr>
nnoremap <leader>en :e /etc/nixos/configuration.nix<cr>
nnoremap <leader>ej :e ~/.config/jj/config.toml<cr>
nnoremap <leader>ew :e ~/.w3m/keymap<cr>
nnoremap <leader>ey :e ~/.config/eww/eww.yuck<cr>
nnoremap <leader>ez :e ~/.zshrc.local<cr>
nnoremap <leader>ec :e ~/.arbtt/categorize.cfg<cr>
nnoremap <leader>ee <C-w><C-v><C-l>:e ~/.notebook.db<cr>:vertical resize 84<cr>
nnoremap <leader>ep :call EditPcfbFile()<cr>

function! EditPcfbFile()
  let file = system("date +'%Y-%m-%d'")
  let file = strpart(file, 0, len(file) - 1)
  execute ":e ~/.tino/var/" . file . ".txt"
endfunction



" ------------------------------------------------------------------------------
                           " Better Default Bindings
" ------------------------------------------------------------------------------
" Remove dumb bindings
noremap  za <nop>
inoremap <F1> <nop>
nnoremap <F1> <nop>
vnoremap <F1> <nop>
map <t_%9> <nop>
imap <t_%9> <nop>
nnoremap Q <nop>
nnoremap gh <nop>
nnoremap <C-Q> <nop>

" Better bindings
nnoremap : <nop>
nnoremap <CR> :
vnoremap <CR> :B  <BS>
nnoremap / /\v
nnoremap <C-q> <C-W><C-q>
" nnoremap K :grep! "\b<C-R><C-W>\b"<CR>:cw<CR>
vnoremap < <gv
vnoremap > >gv
nnoremap * *N
nnoremap Y y$

vnoremap J :m '>+1<CR>gv
vnoremap K :m '<-2<CR>gv

" Insert mode bindings
" inoremap <C-I> <C-O>^
" inoremap <C-A> <C-O>$

" Switch to alt file
nnoremap # :e #<CR>

function! Rename()
  let var = expand("<cword>")
  let result = input(var . " -> ")
  if result != ""
    set noignorecase
    execute "normal! :%s//" . result . "\<CR>"
    set ignorecase
  endif
endfunction

" Rename
" TODO(sandy): make a bufdo version of this
nnoremap <Leader>s *N:noh<CR>:call Rename()<CR>

" Free keys:
" K, !?, &, \, Zx, Q


" ------------------------------------------------------------------------------
                             " Convenience Bindings
" ------------------------------------------------------------------------------
nnoremap vv vip

" Rerun last ex command
nnoremap <S-CR> :<Up><CR>

" Run shell command
nnoremap <C-CR> :!  <BS>

" Close buffer without changing layout
nnoremap :: :bp\|bd #<CR>

" Delete with no yank stack
nnoremap <silent> <leader>d "_d
vnoremap <silent> <leader>d "_d

" Save file
nnoremap ;; :w<CR>

" Pop tag stack
" nnoremap <C-[> <ESC>:po<CR>


" Help align visual blocks by delimiter
vmap <leader><space> <Plug>(EasyAlign)

nnoremap zj moo <c-u><esc>k`o
nnoremap zk moO <c-u><esc>`o

" Things we can align on
let g:easy_align_delimiters = {
\ '=': { 'pattern': '[=≡]',                'left_margin': 1, 'right_margin': 1 },
\ '[': { 'pattern': '[[\]]',               'left_margin': 0, 'right_margin': 0 },
\ '(': { 'pattern': '[()]',                'left_margin': 2, 'right_margin': 0 },
\ ']': { 'pattern': '[[\]]',               'left_margin': 1, 'right_margin': 0 },
\ ')': { 'pattern': '[()]',                'left_margin': 1, 'right_margin': 0 },
\ '<': { 'pattern': '[<←]',                'left_margin': 1, 'right_margin': 0 },
\ '.': { 'pattern': '\.',                  'left_margin': 1, 'right_margin': 1 },
\ '>': { 'pattern': '->\|→',               'left_margin': 1, 'right_margin': 1 },
\ 'p': { 'pattern': '¿¿',                  'left_margin': 1, 'right_margin': 1 },
\ ':': { 'pattern': '::\|∷\|:',               'left_margin': 1, 'right_margin': 1 },
\ '@': { 'pattern': '@',                   'left_margin': 1, 'right_margin': 0 },
\ '$': { 'pattern': '\$',                  'left_margin': 1, 'right_margin': 1 },
\ '~': { 'pattern': '\.\~',                'left_margin': 1, 'right_margin': 1 },
\ '#': { 'pattern': '#',                   'left_margin': 1, 'right_margin': 0 },
\ 't': { 'pattern': '⁀',                   'left_margin': 1, 'right_margin': 1 },
\ 'q': { 'pattern': '\(qualified\)\?\ze ', 'left_margin': 1, 'right_margin': 1 },
\ 'c': { 'pattern': '.\zs--',              'left_margin': 2, 'right_margin': 1, 'ignore_groups': [] },
\ 'r': { 'pattern': '[∼≤≡≈∎]',             'left_margin': 2, 'right_margin': 0 },
\ }

cmap <expr> %% expand('%:p:h') . '/'


" ------------------------------------------------------------------------------
                                 " Gnarly Shit
" ------------------------------------------------------------------------------
" Sudo save file.
cmap w!! %!sudo tee > /dev/null %



" ------------------------------------------------------------------------------
                              " Movement Bindings
" ------------------------------------------------------------------------------
" Cursor Movement
nnoremap <up> <nop>
nnoremap <down> <nop>
nnoremap <left> <nop>
nnoremap <right> <nop>
nnoremap j gj
nnoremap k gk
vnoremap j gj
vnoremap k gk

" Window movement
nnoremap L <C-W><C-L>
nnoremap H <C-W><C-H>

" Window resizing
noremap <silent> <C-F9>  :vertical resize -10<CR>
noremap <silent> <C-F10> :resize +10<CR>
noremap <silent> <C-F11> :resize -10<CR>
noremap <silent> <C-F12> :vertical resize +10<CR>

nnoremap <silent> <C-c> :copen<CR>

" Buffer list
nnoremap gb :ls<CR>:b<Space>
" nnoremap gb :Telescope buffers<CR>



" ------------------------------------------------------------------------------
                                 " Experimental
" ------------------------------------------------------------------------------
function! SmartWordSearch(dir)
  let hlsearch = &hlsearch
  let search = @/
  let @/ = "\\v<|\\u|[_.-]\\zs|\\s+\\zs\\S"
  execute "normal! " . a:dir
  let @/ = search
  let &hlsearch = hlsearch
endfunction

noremap <silent> W :silent call SmartWordSearch("n")<CR>
noremap <silent> B :silent call SmartWordSearch("N")<CR>

function! Reg()
    reg
    echo "Register: "
    let char = nr2char(getchar())
    if char != "\<Esc>"
        execute "normal! \"".char."p"
    endif
    redraw
endfunction

" noremap <up>    <C-W>-
" noremap <down>  <C-W>+
noremap <left>  3<C-W><
noremap <right> 3<C-W>>

nnoremap " '
nnoremap ' "
vnoremap " '
vnoremap ' "
cnoremap '' <C-R>
nnoremap '' :call Reg()<CR>

" Experimental motions

let g:sneak#s_next = 1

" paste over
function! PasteOver(type, ...)
    let saveSel = &selection
    let &selection = "inclusive"
    let saveReg = @@
    let reg = v:register
    let regContents = getreg(reg)

    if a:0  " Invoked from Visual mode, use '< and '> marks.
        silent exe "normal! `<" . a:type . "`>"
    elseif a:type == 'line'
        silent exe "normal! '[V']"
    elseif a:type == 'block'
        silent exe "normal! `[\<C-V>`]"
    else
        silent exe "normal! `[v`]"
    endif

    execute "normal! \"" . reg . "p"

    let &selection = saveSel
    let @@ = saveReg

    call setreg(reg, regContents)
endfunction

function! SetPasteOver()
    set opfunc=PasteOver
    return "g@"
endfunction

nnoremap <expr> PP SetPasteOver()


" ------------------------------------------------------------------------------
                              " System Integration
" ------------------------------------------------------------------------------
" Regular copy and paste
set iminsert=1
set imsearch=1
set imcmdline
lnoremap <C-v> <C-r><C-o>+
set clipboard+=unnamedplus


" Jump back to same line when reopening
augroup line_return
    au!
    au BufReadPost *
        \ if line("'\"") > 0 && line("'\"") <= line("$") |
        \ execute 'normal! g`"zvzz' |
        \ endif
augroup END

" Get rid of stupid system files from ctrl-p search
let g:ctrlp_custom_ignore = 'target/\|dist/\|lua$\|ogg$\|ghcide\|testdata\|docs\|\.jj'
" let g:ctrlp_by_filename = 1


" ------------------------------------------------------------------------------
                                " GUI and Colors
" ------------------------------------------------------------------------------
if has("gui_running")
    set guitablabel=%-0.12t%M
    set showtabline=2
    " au BufAdd * :RainbowParentheses

    try
    if g:colors_name ==# "railscasts"
        hi WildMenu guifg=Black guibg=#777700 gui=bold cterm=bold
        hi Tabline guibg=#000000 guifg=#999999
        hi TablineFill guibg=#777700 guifg=#000000
        hi StatusLine guibg=#000000 guifg=#FFFFFF
    endif
    catch
    endtry
else
    set t_Co=256
endif

set guioptions=ac
set background=dark
colo ego
" set background=light

" Rainbow colored parentheses
let g:rainbow#max_level = 16
let g:rainbow#pairs = [['(', ')'], ['[', ']']]
let g:rainbow#colors = {
\   'dark': [
\     ['magenta', 'purple1'     ],
\     ['cyan',    'magenta1'    ],
\     ['green',   'slateblue1'  ],
\     ['yellow',  'cyan1'       ],
\     ['red',     'springgreen1'],
\     ['magenta', 'green1'      ],
\     ['cyan',    'greenyellow' ],
\     ['green',   'yellow1'     ],
\     ['yellow',  'orange1'     ]
\   ] }

" 2mat ErrorMsg '\%81v.'

set statusline=
set statusline +=%3*%y%*                "file type
set statusline +=%1*\ %n\ %*            "buffer number
set statusline +=%4*\ %<%f%*            "full path
set statusline +=%2*%m%*                "modified flag
set statusline +=%1*%=%2*\ =%{v:register}
set statusline +=%5l%*                  "current line
set statusline +=%2*/%L%*               "total lines
set statusline +=%1*%4v\ %*             "virtual column number

hi User1 guifg=#eea040 guibg=#222222
hi User2 guifg=#dd3333 guibg=#222222
hi User3 guifg=#ff66ff guibg=#222222
hi User4 guifg=#a0ee40 guibg=#222222
hi User5 guifg=#eeee40 guibg=#222222


" ------------------------------------------------------------------------------
                                 " Autocommands
" ------------------------------------------------------------------------------
" Redraw when the window is resized
au VimResized * :wincmd =

" On save, remove all trailing spaces
au BufWritePre * execute "normal! ms:%s/\\v\\s\+$//e\<CR>`s"

" Re-source ~/.vimrc whenever it is saved
augroup automaticallySourceVimrc
  au!
  au bufwritepost .vimrc source ~/.vimrc
augroup END

" <CR> maps to :, but this is shitty for quickfix windows
augroup unmapCRInQuickfix
  au!
  autocmd BufReadPost,BufEnter quickfix nnoremap <buffer> <CR> <CR>
  autocmd BufReadPost,BufEnter quickfix nnoremap <buffer> : :
  autocmd BufReadPost,BufEnter quickfix nnoremap <buffer> q <C-W><C-Q>
  autocmd CmdwinEnter * nnoremap <buffer> <CR> <CR>
  autocmd CmdwinEnter * nnoremap <buffer> : :
  autocmd CmdwinEnter * nnoremap <buffer> q <C-W><C-Q>
  autocmd WinLeave * cclose
augroup END


" ------------------------------------------------------------------------------
                              " Filetype Settings
" ------------------------------------------------------------------------------

function! MarkdownFiletype()
    setf ghmarkdown
    " Markdown link
    inoremap <buffer> <C-B><C-B> ****<Left><Left>
    nnoremap <buffer> zb z=1<CR><CR>
    imap <buffer> \ann <ESC>maysiv]%a(Ann)<space><esc>a
    set cc=81
endfunction

function! AddHsPragma(kind, more)
    " Add a new HS pragma, and sort the list so it's pretty
    let pragma = input(a:kind . " ")
    normal! ms
    if match(getline(1), "module") == 0
      execute "normal! ggO\<ESC>"
    endif
    if pragma != ""
        execute "normal! ggO{-# " . a:kind . " " . a:more . pragma . " #-}\<ESC>"
    endif
    execute "normal! ggvip:sort\<CR>gv:EasyAlign -#\<CR>"
    normal `s
endfunction

au BufReadPre *.agda call AgdaFiletype()
au BufReadPre *.lagda.md call AgdaFiletype()
" au BufWritePost *.agda execute "normal! :CornelisLoad\<CR>"

function! ScalaFiletype()
  inoremap <buffer> -= <space>=><space>
  inoremap <buffer> 0- <space>=><space>
endfunction

function! AgdaFiletype()
    setlocal nospell
    inoremap <buffer> <localleader> <C-O>:call cornelis#prompt_input()<CR>
    set cc=64,81
    let b:surround_104="{! \r !}"
    let b:surround_112="{-# \r #-}"
    let b:surround_105="⦃ \r ⦄"
    call cornelis#bind_input("rep", "𝄆 𝄇")
    call cornelis#bind_input("ru", " ⇀ ")
    call cornelis#bind_input("dw", "𝅝")
    call cornelis#bind_input("dh", "𝅗𝅥")
    call cornelis#bind_input("dq", "𝅘𝅥")
    call cornelis#bind_input("d8", "𝅘𝅥𝅮")
    call cornelis#bind_input("d1", "𝅘𝅥𝅯")
    call cornelis#bind_input("d3", "𝅘𝅥𝅰")
    call cornelis#bind_input("d.", "．")
    call cornelis#bind_input("tie", "⁀")
    call cornelis#bind_input("div", "÷")
    call cornelis#bind_input("half", "½")
    call cornelis#bind_input("sp", "𝄩")

    call cornelis#bind_input("drw", "𝄻")
    call cornelis#bind_input("drh", "𝄼")
    call cornelis#bind_input("drq", "𝄽")
    call cornelis#bind_input("dr8", "𝄾")
    call cornelis#bind_input("dr6", "𝄿")
    call cornelis#bind_input("dr2", "𝅀")
    call cornelis#bind_input("alg", "…algebra…")
    call cornelis#bind_input("idl", "identityˡ")
    call cornelis#bind_input("idr", "identityʳ")
    call cornelis#bind_input("via", "…via…")
    call cornelis#bind_input("...", "…")
    call cornelis#bind_input("mto", "↦")
    call cornelis#bind_input("two", "⇉")
    call cornelis#bind_input("sem", "⊨")
    call cornelis#bind_input("nsem", "⊭")
    call cornelis#bind_input("sqv", "▯")
    call cornelis#bind_input("^+", "⁺")
    call cornelis#bind_input("imp", "⊃")
    call cornelis#bind_input("inj", "↪")
    call cornelis#bind_input("ne", "≠")
    call cornelis#bind_input("iso", "≅")
    call cornelis#bind_input("iff", "↔")
    call cornelis#bind_input("neq", "≢")
    call cornelis#bind_input("num", "↥")
    call cornelis#bind_input("den", "↧ₙ")
    call cornelis#bind_input("-p", "⟶")
    call cornelis#bind_input(".0", "□")
    call cornelis#bind_input(".1", "∙")
    call cornelis#bind_input(".2", "⠢")
    call cornelis#bind_input(".3", "⋱")
    call cornelis#bind_input(".4", "⠭")
    call cornelis#bind_input(".5", "⁙")
    call cornelis#bind_input(".6", "⠿")


    " imap <buffer> == <space>\==<space>
    imap <buffer> -= <space>\to<space>
    imap <buffer> _+ <space>\=><space>
    imap <buffer> )_ <space>\=><space>
    imap <buffer> \cong cong (λ \Gf \to ?) ?
    imap <buffer> \begin begin<CR><tab>?<CR><BS>\step<CR><BS>\qed
    imap <buffer> 0- <space>\to<space>
    inoremap <buffer> =- <space><-<space>
    inoremap <buffer> -0 <space><-<space>
    " inoremap <buffer> ? {!  !}<left><left><left>
    imap <buffer> \step \==\< ? \><CR><space><space>?
    imap <buffer> \sstep \~~\< ? \><CR><space><space>?
    nnoremap <buffer> <leader>l :CornelisLoad<CR>
    nnoremap <buffer> zl :CornelisLoad<CR>
    nnoremap <buffer> <leader>o :call AddHsPragma("OPTIONS", "--")<CR>
    nnoremap <buffer> <leader>r :CornelisRefine<CR>
    nnoremap <buffer> <leader>d :CornelisMakeCase<CR>
    nnoremap <buffer> <leader>h :CornelisQuestionToMeta<CR>
    nnoremap <buffer> <C-A>     :CornelisInc<CR>
    nnoremap <buffer> <C-X>     :CornelisDec<CR>
    " nnoremap <buffer> <leader>e :AgdaContext<CR>
    " nnoremap <buffer> <leader>h :AgdaHelperFun<CR>
    nnoremap <buffer> <leader>, :CornelisTypeContext Simplified<CR>
    nnoremap <buffer> <leader><leader>, :CornelisTypeContext AsIs<CR>
    nnoremap <buffer> <leader><leader><leader>, :CornelisTypeContext Normalised<CR>
    nnoremap <buffer> <leader>. :CornelisTypeContextInfer Simplified<CR>
    nnoremap <buffer> <leader><leader>. :CornelisTypeContextInfer AsIs<CR>
    nnoremap <buffer> <leader><leader><leader>. :CornelisTypeContextInfer Normalised<CR>
    " nnoremap <buffer> <leader>x :AgdaCompute<CR>
    nnoremap <buffer> <leader>n :CornelisSolve Simplified<CR>
    nnoremap <buffer> <leader><leader>n :CornelisSolve AsIs<CR>
    nnoremap <buffer> <leader><leader><leader>n :CornelisSolve Normalised<CR>
    nnoremap <buffer> <leader>N :CornelisAuto<CR>
    nnoremap <buffer> gd :CornelisGoToDefinition<CR>
    nnoremap <buffer> [/ :CornelisPrevGoal<CR>
    nnoremap <buffer> ]/ :CornelisNextGoal<CR>
    nnoremap <buffer> ch cl{!  !}<ESC>:w<CR>hh
    nnoremap <buffer> <leader>m yyp0f:C= ?<esc>:w<CR>$

endfunction

function! AgdaMarkdownFiletype()
  call AgdaFiletype()
  set tw=80
  set fo+=t
  set spell
endfunction


function! HaskellFiletype()
    nnoremap <buffer> <leader>eh :e %-boot<cr>
    let b:surround_112="{-# \r #-}"

    nnoremap <buffer> <leader>m yyp0f:C= _<esc>
    " set formatprg=stylish-haskell
    nnoremap <buffer> <F1> :Hoogle<space>
    nnoremap <buffer> <leader>h :Hoogle<space>
    nnoremap <buffer> <leader>l :call AddHsPragma("LANGUAGE", "")<CR>
    nnoremap <buffer> <leader>o :call AddHsPragma("OPTIONS_GHC", "-")<CR>
    " setlocal formatprg=floskell

    inoremap <buffer> <localleader>[[ ⟦
    inoremap <buffer> <localleader>]] ⟧

    nmap <buffer> <silent> [g <Plug>(coc-diagnostic-prev)
    nmap <buffer> <silent> ]g <Plug>(coc-diagnostic-next)
    nmap <buffer> <silent> gd <Plug>(coc-definition)

    nnoremap <buffer> <buffer> [[ ?\v^[^ ]* +::<CR>:noh<CR>
    nnoremap <buffer> <buffer> ]] /\v^[^ ]* +::<CR>:noh<CR>

    syntax match haskellKeyword "/\\/ conceal cchar=Î»"
    syntax match haskellKeyword "/ \zs\.\ze / conceal cchar=â"
    syntax match haskellKeyword "/\zs-\ze>/ conceal cchar=â"
    syntax match haskellKeyword "/>/ conceal cchar=âº"
    syntax match haskellKeyword "/\// conceal cchar=Ã·"

    " syntax match haskellKeyword /\zs=\ze>/ conceal cchar=â
    hi clear Conceal
    setlocal conceallevel=2
    setlocal concealcursor=nvic

    " Transform haskell line into a pointfree version
    nnoremap <buffer> <leader>pf V:! pointfree "`cat`"<CR>==

    " Easy-to-type haskell digraphs
    inoremap <buffer> ,. <space>~><space>
    inoremap <buffer> -= <space>-><space>
    inoremap <buffer> 0- <space>-><space>
    inoremap <buffer> =- <space><-<space>
    inoremap <buffer> -0 <space><-<space>
    inoremap <buffer> _+ <space>=><space>
    inoremap <buffer> )_ <space>=><space>

    nnoremap <buffer> <leader><leader>m ggI<C-R>%<ESC>V:s/\//./g<CR>:noh<CR>Imodule <ESC>A<BS><BS><BS> where<ESC>

    nnoremap <buffer> -- O-- \|<space>
    nnoremap <buffer> <leader><leader>gg magg/^import<CR>Oimport GHC.Generics<ESC>`a:noh<CR>
    nnoremap <buffer> <leader><leader>db magg/^import<CR>Oimport Data.Bool<ESC>`a:noh<CR>
    nnoremap <buffer> <leader><leader>ca magg/^import<CR>Oimport Control.Arrow<ESC>`a:noh<CR>
    nnoremap <buffer> <leader><leader>cm magg/^import<CR>Oimport Control.Monad<ESC>`a:noh<CR>
    nnoremap <buffer> <leader><leader>dm magg/^import<CR>Oimport qualified Data.Map as M<CR>import Data.Map (Map)<ESC>`a:noh<CR>
    nnoremap <buffer> <leader><leader>ds magg/^import<CR>Oimport qualified Data.Set as S<CR>import Data.Set (Set)<ESC>`a:noh<CR>
    nnoremap <buffer> <leader><leader>dim magg/^import<CR>Oimport qualified Data.IntMap as IM<CR>import Data.IntMap (IntMap)<ESC>`a:noh<CR>
    nnoremap <buffer> <leader><leader>dt magg/^import<CR>Oimport Data.Traversable<ESC>`a:noh<CR>
    nnoremap <buffer> <leader><leader>df magg/^import<CR>Oimport Data.Foldable<ESC>`a:noh<CR>

    " Quick alignment of imports
    nnoremap <silent><buffer> <leader>si magg/^import<CR>vip:EasyAlign q<CR>gv:sort /.*\%18v/<CR>:noh<CR>`a

    " Better syntax highlighting
    syntax keyword error undefined unsafePerformIO fromJust unsafeCoerce read

    syntax keyword haskellKeyword when unless flip const id maybe fmap pure fst snd curry uncurry show first second either for_ join mempty mappend mconcat asum empty mzero traverse traverse_
    syntax keyword haskellPragma ap ask filter foldl foldr not negate fromInteger div mod toInteger round truncate ceiling floor null length elem head tail any all concat and or take drop takeWhile dropWhile zip zipWith lines words unlines unwords putStrLn print getChar getLine readFile writeFile isJust get put local liftIO runReader runState runReaderT runStateT runWriter runWriterT subtract fromIntegral lift runIdentity extract Just Nothing Left Right
    syntax match haskellIdentifier /\v(S|M)\.(singleton|empty|insert|contains)/
    syntax match haskellIdentifier /\vT\.(pack|unpack)/

    syntax match haskellDecl /\v<(Has|To|From|Known|Monad|Sing)[A-Z][A-Za-z0-9'_]*>/
    syntax keyword haskellDecl Show Read Dict1 Dict2 Monad Num Fractional Real Floating Integral Eq Ord Applicative Functor
    syntax keyword haskellBottom DemoteRep Proxy Type Typeable
    syntax keyword haskellKeyword m

    nnoremap <buffer> <silent> <space>r  :<C-u>set operatorfunc=<SID>WingmanRefine<CR>g@l
    nnoremap <buffer> <silent> <space>d  :<C-u>set operatorfunc=<SID>WingmanDestruct<CR>g@l
    nnoremap <buffer> <silent> <space>n  :<C-u>set operatorfunc=<SID>WingmanFillHole<CR>g@l
    nnoremap <buffer> <silent> <space>c  :<C-u>set operatorfunc=<SID>WingmanUseCtor<CR>g@l
    nnoremap <buffer> <silent> <space>a  :<C-u>set operatorfunc=<SID>WingmanDestructAll<CR>g@l
endfunction

" Set filetypes
" autocmd BufReadPre,BufNewFile * let b:did_ftplugin = 1
au BufRead,BufNewFile *.db setfiletype db
" au BufRead,BufNewFile *.md call MarkdownFiletype()
au BufRead,BufNewFile *.markdown call MarkdownFiletype()
au BufRead,BufNewFile *.hs call HaskellFiletype()
autocmd FileType cpp setlocal commentstring=//\ %s


syntax on

let g:did_load_filetypes = 1
filetype plugin indent on

" Markdown settings
let g:vim_markdown_initial_foldlevel=1
let g:vim_markdown_math=1
let g:vim_markdown_frontmatter=1

" Haskell settings
let g:haskell_indent_do = 3
let g:haskell_indent_if = 3
let g:haskell_indent_in = 1
let g:haskell_indent_let = 4
let g:haskell_indent_case = 2
let g:haskell_indent_where = 6
let g:haddock_browser="/usr/bin/sensible-browser"

let g:gist_detect_filetype = 1



" ------------------------------------------------------------------------------
                                   " Settings
" ------------------------------------------------------------------------------
set autoindent
set backspace=indent,eol,start
set backupskip=/tmp/*,/private/tmp/*"
set dictionary=/usr/share/dict/words
set expandtab
set formatoptions=qrn1
set gdefault
set grepprg=ag
set hidden
set hlsearch
set ignorecase
set incsearch
set laststatus=2
set nolazyredraw
set list
set listchars="tab:»·,trail:·"
set matchtime=3
set modeline
set modelines=2
set mouse=a
set nofoldenable
set number
set relativenumber
set ruler
set scrolloff=8
set shiftround
set shiftwidth=2
set showcmd
set showmatch
set showmode
set smartcase
set splitbelow
set splitright
set softtabstop=2
set tabstop=2
set timeoutlen=300
set title
set ttyfast
set visualbell
set wildignore+=*.aux,*.out,*.toc
set wildignore+=*.hi
set wildignore+=*.ogg
set wildignore+=*.jpg,*.bmp,*.gif,*.png,*.jpeg
set wildignore+=*.luac
set wildignore+=*.o,*.obj,*.exe,*.dll,*.manifest
set wildignore+=*.orig
set wildignore+=*.pyc
set wildignore+=*.spl
set wildignore+=*.sw?
set wildignore+=docs/
set wildignore+=.hg,.git,.svn,.jj
set wildignore+=tags
set wildignore+=ghcide
set wildignore+=testdata
set wildmenu
set wildmode=list:longest
set wrap


" ------------------------------------------------------------------------------
                        " Searching and File Management
" ------------------------------------------------------------------------------
let g:grep_cmd_opts = '--line-numbers --noheading'

function! RenameFile()
    let old_name = expand('%')
    let new_name = input('New file name: ', expand('%'), 'file')
    if new_name != '' && new_name != old_name
        exec ':saveas ' . new_name
        exec ':silent !rm ' . old_name
        redraw!
    endif
endfunction

if executable('ag')
  set grepprg=ag\ --nogroup\ --nocolor
  let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
  let g:ctrlp_use_caching = 0
endif



" ------------------------------------------------------------------------------
                            " Easy Buffer Switching
" ------------------------------------------------------------------------------
" Turn on buffer numbers in the tabline
let g:buftabline_numbers = 1
set showtabline=2

" Allow switching to buffer #<n> by typing <n>e
function! s:bufSwitch(count)
    if a:count >=# 1
        return ":\<C-U>" . a:count . "b\<CR>"
    endif
    return 'e'
endfunction
nnoremap <expr> e <SID>bufSwitch(v:count)



" ------------------------------------------------------------------------------
                           " Source Local Definitions
" ------------------------------------------------------------------------------
" At work, I have company-specific bindings that would get me in trouble to post
" on github. So they live in this unmanaged file.
try
    source ~/.vimrc.local
catch
endtry



" ------------------------------------------------------------------------------
                                   " Training
" ------------------------------------------------------------------------------
" The best way to get better at things is to make it really hard to do the
" wrong things

function! IgnoreWithoutCount(command)
  if v:count == 0
      return ""
  end
  return a:command
endfunction

" nnoremap <expr> l IgnoreWithoutCount('l')
" nnoremap <expr> h IgnoreWithoutCount('h')
" nnoremap <expr> j IgnoreWithoutCount('j')
" nnoremap <expr> k IgnoreWithoutCount('k')

" ------------------------------------------------------------------------------
                                    " Neovim
" ------------------------------------------------------------------------------

if has("nvim")

tnoremap <ESC> <C-\><C-n>:set relativenumber<CR>
highlight TermCursor ctermfg=red guifg=red

func! MaybeInsertMode(direction)
    stopinsert
    if &buftype == 'terminal'
       set norelativenumber
    endif
    execute "wincmd " . a:direction
    if &buftype == 'terminal'
        startinsert!
        set norelativenumber
    endif
endfunc

tnoremap <silent> <c-h> <c-\><c-n>:call MaybeInsertMode("h")<cr>
tnoremap <silent> <c-j> <c-\><c-n>:call MaybeInsertMode("j")<cr>
tnoremap <silent> <c-k> <c-\><c-n>:call MaybeInsertMode("k")<cr>
tnoremap <silent> <c-l> <c-\><c-n>:call MaybeInsertMode("l")<cr>
nnoremap <silent> <c-h> :call MaybeInsertMode("h")<cr>
nnoremap <silent> <c-j> :call MaybeInsertMode("j")<cr>
nnoremap <silent> <c-k> :call MaybeInsertMode("k")<cr>
nnoremap <silent> <c-l> :call MaybeInsertMode("l")<cr>

endif


" COC stuff

xmap <leader>xa  <Plug>(coc-codeaction-selected)
nmap <leader>xa  <Plug>(coc-codeaction-selected)
nmap <c-space> <space>xal

nmap <silent> <space>e  <Plug>(coc-codelens-action)

function! s:GotoNextHole()
  sleep 500m
  normal 0
  call CocActionAsync('diagnosticNext', 'hint')
endfunction

function! s:WingmanRefine(type)
  call CocAction('codeAction', a:type, ['refactor.wingman.refine'])
  call <SID>GotoNextHole()
endfunction

function! s:WingmanDestruct(type)
  call CocAction('codeAction', a:type, ['refactor.wingman.caseSplit'])
  call <SID>GotoNextHole()
endfunction

function! s:WingmanDestructAll(type)
  call CocAction('codeAction', a:type, ['refactor.wingman.splitFuncArgs'])
  call <SID>GotoNextHole()
endfunction

function! s:WingmanFillHole(type)
  call CocAction('codeAction', a:type, ['refactor.wingman.fillHole'])
  call <SID>GotoNextHole()
endfunction

function! s:WingmanUseCtor(type)
  call CocAction('codeAction', a:type, ['refactor.wingman.useConstructor'])
  call <SID>GotoNextHole()
endfunction

" Remove search highlighting
nnoremap <silent> <leader><space>  :noh<cr>

if exists('*complete_info')
  inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"
else
  inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
endif

set updatetime=300

if has("patch-8.1.1564")
  " Recently vim can merge signcolumn and number column into one
  set signcolumn=number
else
  set signcolumn=yes
endif
" nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

let g:ctrlp_working_path_mode = 'r'
let g:ctrlp_mruf_relative = 1
let g:ctrlp_mruf_exclude = '.jj'

let g:neovide_cursor_vfx_mode = "ripple"

nnoremap J :set operatorfunc=Joinoperator<CR>g@
nnoremap JJ J
nnoremap gJ :set operatorfunc=GJoinoperator<CR>g@
onoremap J j
func! Joinoperator(submode)
        '[,']join
endfunc
func! GJoinoperator(submode)
        '[,']join!
endfunc



nnoremap <leader>ut yypVr=<esc>o<cr><esc>
nnoremap <leader>us o<cr>----<esc>o<cr><esc>


function! DoCombyMatch(cmd)
  let args = map(split(a:cmd, '/'), {pos,val -> shellescape(val)})
  let comby = "comby -match-only -ripgrep " . shellescape("--ignore -g *." . expand("%:e")) .  " " . args[0] . " ''"
  echom comby
  cexpr system(comby)
endfunc

function! DoCombySubst(cmd)
  let args = map(split(a:cmd, '/'), {pos,val -> shellescape(val)})
  let comby = "comby -stdin -stdout -matcher ." . expand("%:e") . " " . args[0] . " " . args[1]
  let cmd = "norm! :0,$ !" . comby . "\<CR>"
  norm! ms
  execute cmd
  norm! `s
endfunc

command! -nargs=1 CombySubst call DoCombySubst(<q-args>)
command! -nargs=1 CombyGrep call DoCombyMatch(<q-args>)


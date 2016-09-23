autocmd!
set nocompatible

if !has('nvim')
  set shell=/usr/bin/zsh\ -l
endif

" ------------------------------------------------------------------------------
                               " Path Extensions
" ------------------------------------------------------------------------------
let $PATH = $PATH . ':' . expand('~/.local/bin')
let $PATH = $PATH . ':' . expand('~/.cabal/bin')
let $PATH = $PATH . ':' . expand('~/.stack/programs/x86_64-linux/ghc-7.8.4/bin')



" ------------------------------------------------------------------------------
                              " Plugin Management
" ------------------------------------------------------------------------------
call plug#begin('~/.vim/plugged')
" Development
Plug 'isovector/ghci.vim'

" Libraries
Plug 'vim-scripts/L9'
Plug 'xolox/vim-misc'
Plug 'tomtom/tlib_vim'
Plug 'MarcWeber/vim-addon-mw-utils'
Plug 'kana/vim-submode'
Plug 'vim-scripts/ingo-library'
Plug 'mattn/webapi-vim'

" Silent
Plug 'rking/ag.vim'
Plug 'tpope/vim-repeat'
Plug 'majutsushi/tagbar'
Plug 'tpope/vim-characterize'
Plug 'vim-scripts/vim-lamdify'
Plug 'junegunn/rainbow_parentheses.vim'
Plug 'ap/vim-buftabline'
Plug 'Shougo/vimproc'

" Navigation
Plug 'kien/ctrlp.vim'
Plug 'vim-scripts/JumpToLastOccurrence'
Plug 'christoomey/vim-tmux-navigator'

" Misc
Plug 'mattn/gist-vim'
Plug 'junegunn/goyo.vim'
" Plug 'Shougo/neosnippet'
Plug 'junegunn/limelight.vim'
Plug 'vim-scripts/tregisters'
Plug 'isovector/vim-howdoi'

" Formatting
Plug 'vim-scripts/vis'
Plug 'godlygeek/tabular'
Plug 'skwp/greplace.vim'
Plug 'tpope/vim-abolish'
Plug 'tpope/vim-commentary'
Plug 'junegunn/vim-easy-align'
Plug 'michaeljsmith/vim-indent-object'
" Plug 'scrooloose/syntastic'

" Languages
Plug 'neovimhaskell/haskell-vim'
" Plug 'eagletmt/ghcmod-vim'
Plug 'Twinside/vim-hoogle'
Plug 'vim-scripts/lua.vim'
Plug 'vim-scripts/lua_indent'
Plug 'plasticboy/vim-markdown'
Plug 'rhysd/conflict-marker.vim'
Plug 'vim-scripts/latex-support.vim'
Plug 'jtratner/vim-flavored-markdown'
Plug 'derekelkins/agda-vim'

" Colors
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

" Textobjs
Plug 'tmhedberg/matchit'
Plug 'tpope/vim-surround'
Plug 'kana/vim-textobj-user'
Plug 'kana/vim-textobj-syntax'
" Plug 'Lokaltog/vim-easymotion'
Plug 'junegunn/vim-after-object'
Plug 'terryma/vim-expand-region'
Plug 'rbonvall/vim-textobj-latex'
Plug 'Julian/vim-textobj-variable-segment'
Plug 'deris/vim-shot-f'
Plug 'wellle/targets.vim'
Plug 'michaeljsmith/vim-indent-object'
Plug 'justinmk/vim-sneak'

call plug#end()



" ------------------------------------------------------------------------------
                                   " Leaders
" ------------------------------------------------------------------------------
let mapleader = " "
let maplocalleader = ","

" Automatic pane split layouts
nnoremap <leader>3 :vsplit<CR>:bn<CR>:vsplit<CR>:bn<CR>
nnoremap <leader>4 :vnew<CR>:bn<CR>:vnew<CR>:bn<CR><C-W><C-L><C-W><C-L>:split<CR>:bn<CR>
nnoremap <leader>sv :vert sb  <BS>

" Easier access to commands
nnoremap <leader>m :make<CR>
nnoremap <leader>g :grep!  <BS>
nnoremap <leader>f :CtrlP<CR>
nnoremap <leader>l :TagbarToggle<CR>

" Todo management
nmap     <leader>td OTODO(sandy): <ESC>gccA
nnoremap <silent> <leader>wtd :lgrep! TODO.sandy<CR>:lw<CR>

" New functionality
noremap  <leader>mv :call RenameFile()<cr>
vnoremap <leader><leader>c :!octave --silent \| cut -c8-<cr>
nnoremap <leader>sla V:s/\(\)/\1\r/<Left><Left><Left><Left><Left><Left><Left><Left>
nnoremap <leader>slb V:s/\(\)/\r\1/<Left><Left><Left><Left><Left><Left><Left><Left>
vnoremap <leader>sl :sort<Cr>gv:! awk '{ print length(), $0 \| "sort -n \| cut -d\\  -f2-" }'<CR>

let g:howdoi_map = '<leader>h'


" ------------------------------------------------------------------------------
                                " Local Leaders
" ------------------------------------------------------------------------------
" TODO(sandy): Add infrastructure to only enable these in certain filetypes

" No distractions for writing
nnoremap <leader>gy :Goyo<CR>

" Insert new vim plugin line from system clipboard
nnoremap <leader>pg o<ESC>"+pkddA'<ESC>0iPlug '<ESC>0

" Create banners in vimrc
nmap <localleader>wb O<ESC>78i-<ESC>gccyyjpk<CR>center<CR>gcc



" ------------------------------------------------------------------------------
                              " Quick Directories
" ------------------------------------------------------------------------------
function! Go()
  let where = input("go ")
  let dir = system("~/.tino/bin/tino where " . where)
  execute ":cd " . dir
endfunction

function! EditGo()
  let where = input("edit go ")
  let where = split(system("~/.tino/bin/tino where " . where), "\n")[0]
  return ":e " . where . "/"
endfunction
nnoremap <expr> <leader>eg EditGo()

nnoremap <leader>cd :cd %:p:h<CR>
nnoremap <leader>cp :cd ~/Projects/
nnoremap <leader>cg :call Go()<CR>
nnoremap <leader>ct :cd ~/.tino/
nnoremap <leader>cz :cd ~/.tino/zsh/



" ------------------------------------------------------------------------------
                                 " Quick Edits
" ------------------------------------------------------------------------------
nnoremap <leader>ev :e ~/.vimrc<cr>
nnoremap <leader>et :e ~/.tino/bin/tino<cr>
nnoremap <leader>el :e ~/.vimrc.local<cr>
nnoremap <leader>ea :e ~/.tino/zsh/aliases.zsh<cr>
nnoremap <leader>ex :e ~/.xmonad/src/xmonad.hs<cr>
nnoremap <leader>eo :e ~/one-liners<cr>
nnoremap <leader>ez :e ~/.zshrc.local<cr>
nnoremap <leader>ee <C-w><C-v><C-l>:e ~/.notebook.db<cr>:vertical resize 84<cr>
nnoremap <leader>ep :call EditPcfbFile()<cr>

" Switch to header or c file
nnoremap gH :e %<.h<CR>
nnoremap gC :e %<.cc<CR>

function! EditPcfbFile()
  let file = system("date +'%Y-%m-%d'")
  let file = strpart(file, 0, len(file) - 1)
  execute ":e ~/.tino/var/" . file . ".txt"
endfunction



" ------------------------------------------------------------------------------
                                " Quick Inserts
" ------------------------------------------------------------------------------
inoremap \fn <c-r>=expand('%:t:r')<cr>
inoremap \dt <esc>:r! date "+\%Y-\%m-\%d \%H:\%m"<cr>kJA  <BS>
inoremap <S-Tab> <esc>ma<<`aa



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
nnoremap K <nop>
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

function! SubstituteParameter()
  let var = expand("<cword>")
  let result = input(var . " -> ")
  execute "normal! v/{\<CR>%"
  execute "normal! :s/\\v<" . var . ">/". result . "\<CR>"
  execute "normal! \<C-O>"
endfunction

" Rename
" TODO(sandy): make a bufdo version of this
nnoremap <Leader>s *N:noh<CR>:%s//
nnoremap <Leader>sp :call SubstituteParameter()<CR>

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
inoremap <c-s> <esc>:w<CR>a
nnoremap <c-s> :w<CR>
vnoremap <c-s> :<c-u>w<CR>

" Pop tag stack
" nnoremap <C-[> <ESC>:po<CR>

" Remove search highlighting
nnoremap <silent> <leader><space> :noh<cr>

" Help align visual blocks by delimiter
vmap <s-space> <Plug>(EasyAlign)

nnoremap zj moo<esc>k`o
nnoremap zk moO<esc>`o

" Things we can align on
let g:easy_align_delimiters = {
\ '[': { 'pattern': '[[\]]', 'left_margin': 0, 'right_margin': 0, 'stick_to_left': 0 },
\ '(': { 'pattern': '[()]', 'left_margin': 0, 'right_margin': 0, 'stick_to_left': 0 },
\ ']': { 'pattern': '[[\]]', 'left_margin': 1, 'right_margin': 0, 'stick_to_left': 0 },
\ ')': { 'pattern': '[()]', 'left_margin': 1, 'right_margin': 0, 'stick_to_left': 0 },
\ '<': { 'pattern': '[<]', 'left_margin': 1, 'right_margin': 0, 'stick_to_left': 0 },
\ '>': { 'pattern': '[->]', 'left_margin': 1, 'right_margin': 0, 'stick_to_left': 0 },
\ ':': { 'pattern': '::', 'left_margin': 1, 'right_margin': 1, 'stick_to_left': 0 },
\ '$': { 'pattern': '\$', 'left_margin': 1, 'right_margin': 1, 'stick_to_left': 0 },
\ }

cmap <expr> %% expand('%:p:h') . '/'


" ------------------------------------------------------------------------------
                                 " Gnarly Shit
" ------------------------------------------------------------------------------
" Sudo save file.
cmap w!! %!sudo tee > /dev/null %

" Change property to dict lookup (a.x -> a['x'])
nmap cod mmysiw]hxlysw'`ml

" Change dict lookup to propert (a['x'] -> a.x)
nmap cop mmds'ds]i.<ESC>`mh



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

" Cursor binding
nnoremap <C-c> :set cursorbind! scrollbind!<CR>

" Buffer list
nnoremap gb :ls<CR>:b<Space>



" ------------------------------------------------------------------------------
                                 " Experimental
" ------------------------------------------------------------------------------
function! SmartWordSearch(dir)
  let hlsearch = &hlsearch
  let search = @/
  let @/ = "\\v<|\\u|_\\zs|\\s+\\zs\\S"
  execute "normal! " . a:dir
  let @/ = search
  let &hlsearch = hlsearch
endfunction

noremap <silent> W :silent call SmartWordSearch("n")<CR>
noremap <silent> B :silent call SmartWordSearch("N")<CR>

" Delete until arguments
nmap <silent> dua dt(lds)

function! Reg()
    reg
    echo "Register: "
    let char = nr2char(getchar())
    if char != "\<Esc>"
        execute "normal! \"".char."p"
    endif
    redraw
endfunction

noremap <up>    <C-W>-
noremap <down>  <C-W>+
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

function! AlignWithAbove()
    let c = nr2char(getchar())
    exec "-t-"
    exec "normal! f" . c . "D0v$r \<ESC>"
    startinsert!
endfunction

nnoremap <F10> :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<' . synIDattr(synID(line("."),col("."),0),"name") . "> lo<" . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>


" ------------------------------------------------------------------------------
                              " System Integration
" ------------------------------------------------------------------------------
" Regular copy and paste
" TODO(sandy): Maybe get rid of this and use the system clipboard?
vnoremap <C-c> "+yi
set iminsert=1
set imsearch=1
set imcmdline
lnoremap <C-v> <C-r><C-o>+

" Jump back to same line when reopening
augroup line_return
    au!
    au BufReadPost *
        \ if line("'\"") > 0 && line("'\"") <= line("$") |
        \ execute 'normal! g`"zvzz' |
        \ endif
augroup END

autocmd VimEnter * call after_object#enable('=', ':', '-', '#', ' ')

" Get rid of stupid system files from ctrl-p search
let g:ctrlp_custom_ignore = 'target/\|dist/\|\_site/'


" ------------------------------------------------------------------------------
                                " GUI and Colors
" ------------------------------------------------------------------------------
if has("gui_running")
    set guitablabel=%-0.12t%M
    set showtabline=2
    au BufAdd * :RainbowParentheses

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
colo ego
set background=dark

" Rainbow colored parentheses
let g:rainbow#max_level = 16
let g:rainbow#pairs = [['(', ')'], ['[', ']']]
let g:rainbow#colors = {
\   'dark': [
\     ['yellow',  'orange1'     ],
\     ['green',   'yellow1'     ],
\     ['cyan',    'greenyellow' ],
\     ['magenta', 'green1'      ],
\     ['red',     'springgreen1'],
\     ['yellow',  'cyan1'       ],
\     ['green',   'slateblue1'  ],
\     ['cyan',    'magenta1'    ],
\     ['magenta', 'purple1'     ]
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
augroup END


" ------------------------------------------------------------------------------
                              " Filetype Settings
" ------------------------------------------------------------------------------

function! MarkdownFiletype()
    setfiletype ghmarkdown
    " Markdown link
    imap     <buffer> \m <ESC>maT]y$}}O<ESC>p0ys$]A:<ESC>`a$T]ys$]A
    inoremap <buffer> <C-B><C-B> ****<Left><Left>
    nnoremap <buffer> zb z=1<CR><CR>

    nnoremap <buffer> == gqip
endfunction

function! AddHsPragma()
    let pragma = input("LANGUAGE ")
    if pragma != ""
        execute "normal! msggO{-# LANGUAGE " . pragma . " #-}\<ESC>`s"
    endif
endfunction

function! HaskellFiletype()
    nnoremap <buffer> <F1> :Hoogle<space>
    nnoremap <buffer> <leader>h :Hoogle<space>
    nnoremap <buffer> <leader>l :call AddHsPragma()<CR>

    " Transform haskell line into a pointfree version
    nnoremap <buffer> <leader>pf V:! pointfree "`cat`"<CR>==
    inoremap -= <space>-><space>
    inoremap =- <space><-<space>
    inoremap +_ <space><=<space>
    inoremap _+ <space>=><space>

    syntax keyword haskellTodo showTrace error undefined traceChanges unsafePerformIO fromJust unsafeCoerce
    syntax keyword haskellNumber sample pick scanle newCollection center tags tagging findTag tag foldmp arrows keyPress onEvent poll sync async scaleRel mkRel origin move toStanza getX getY rect traced whenE run uniform uniformIn listOf uniformly filled styled mag distance posDif circle polygon runLift group go fcata acata rcata
    syntax keyword haskellIdentifier return fmap map id forM_ mapM_ join ap pure ask filter foldl foldr flip not maybe fst snd curry uncurry negate abs fromInteger div mod toInteger round truncate ceiling floor mempty mappend mconcat sequence null length elem head tail any all concat and or take drop takeWhile dropWhile lookup zip zipWith lines words unlines unwords read show putStrLn print getChar getLine readFile writeFile isJust makeLenses const view set first second get put local liftIO def when runReader runState runReaderT runStateT runWriter runWriterT fromEnum toEnum subtract fromIntegral forM lift liftM liftM2 liftM3 liftM4 liftM5 uncons minBound maxBound runIdentity coiter coiterT extract unwrap liftF fix runFree cata ana forall
    syntax match haskellIdentifier /\vS\.(singleton|empty|insert|contains)/
    syntax match haskellIdentifier /\vM\.(singleton|empty|insert|contains)/
    syntax match haskellIdentifier /\vT\.(pack|unpack)/

    " syntax region haskellDefinition start=/\v^[^ ]/ end='=' oneline contains=haskellStart,haskellArg
    " syntax match haskellStart "\v^[^ ]+" contained
    " syntax match haskellArg "\v [^ ]+"  contained
    " highlight def link haskellStart haskellNumber
    " highlight def link haskellArg haskellNumber
endfunction

" Set filetypes
" autocmd BufReadPre,BufNewFile * let b:did_ftplugin = 1
au BufRead,BufNewFile *.db setfiletype db
au BufRead,BufNewFile *.md call MarkdownFiletype()
au BufRead,BufNewFile *.markdown call MarkdownFiletype()
au BufRead,BufNewFile *.hs call HaskellFiletype()
au BufRead,BufNewFile *.htex setfiletype htex
autocmd FileType cpp setlocal commentstring=//\ %s


syntax on
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
set lazyredraw
set list listchars=tab:»·,trail:·
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
set shiftwidth=4
set showcmd
set showmatch
set showmode
set smartcase
set splitbelow
set splitright
set softtabstop=4
set tabstop=4
set tags=./tags,tags,../tags
set timeoutlen=300
set title
set ttyfast
set visualbell
set wildignore+=*.aux,*.out,*.toc
set wildignore+=*.hi
set wildignore+=*.jpg,*.bmp,*.gif,*.png,*.jpeg
set wildignore+=*.luac
set wildignore+=*.o,*.obj,*.exe,*.dll,*.manifest
set wildignore+=*.orig
set wildignore+=*.pyc
set wildignore+=*.spl
set wildignore+=*.sw?
set wildignore+=.hg,.git,.svn
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
                                     " Goyo
" ------------------------------------------------------------------------------
" Goyo is zen-mode writing. Most of my usual settings conflict with it pretty
" hard, so this fixes it
function! s:goyo_enter()
    Limelight
    augroup relnum
        au!
    augroup END
endfunction

function! s:goyo_leave()
    augroup relnum
        au!
        au FocusLost * :set norelativenumber
        au FocusGained * :set relativenumber
        au InsertEnter * :set norelativenumber
        au InsertLeave * :set relativenumber
    augroup END

    if exists(":Limelight")
        Limelight!
    endif
endfunction

autocmd User GoyoEnter nested call <SID>goyo_enter()
autocmd User GoyoLeave nested call <SID>goyo_leave()
call <SID>goyo_leave()



" ------------------------------------------------------------------------------
                            " Easy Buffer Switching
" ------------------------------------------------------------------------------
" Turn on buffer numbers in the tabline
let g:buftabline_numbers = 1

" Allow switching to buffer #<n> by typing <n>e
function! s:bufSwitch(count)
    if count >=# 1
        return ":\<C-U>" . count . "b\<CR>"
    endif
    return 'e'
endfunction
nnoremap <expr> e <SID>bufSwitch(v:count)



" ------------------------------------------------------------------------------
                            " Limelight Scrollbinding
" ------------------------------------------------------------------------------
function! s:sync_limelight(bang)
  execute 'Limelight'.a:bang
  augroup sync_limelight
    autocmd!
    if empty(a:bang)
      autocmd CursorMoved * wincmd p | doautocmd limelight CursorMoved | wincmd p
    endif
  augroup END
endfunction

command! -bang SyncLimelight call s:sync_limelight('<bang>')



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

nnoremap <silent> ga :call AlignWithAbove()<CR>

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


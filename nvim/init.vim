call plug#begin('~/.local/share/nvim/plugged')
Plug 'tpope/vim-fugitive'
nnoremap <leader>b :Gblame<CR>
Plug 'tpope/vim-repeat'
Plug 'junegunn/rainbow_parentheses.vim'
Plug 'kien/ctrlp.vim'
nnoremap <leader>f :CtrlP<CR>
Plug 'kana/vim-textobj-user'
Plug 'Julian/vim-textobj-variable-segment'
Plug 'deris/vim-shot-f'
Plug 'junegunn/vim-easy-align'
Plug 'neovimhaskell/haskell-vim'
let g:haskell_enable_quantification = 1
let g:haskell_enable_recursivedo = 1
let g:haskell_enable_arrowsyntax = 1
let g:haskell_enable_pattern_synonyms = 1
let g:haskellmode_completion_ghc = 0

Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
let g:deoplete#enable_at_startup = 1
inoremap <expr> j pumvisible() ? '<C-n>' : 'j'
inoremap <expr> k pumvisible() ? '<C-p>' : 'k'

Plug 'eagletmt/neco-ghc'
let g:necoghc_enable_detailed_browse = 1
autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc

Plug 'ap/vim-buftabline'
let g:buftabline_numbers = 1

" Allow switching to buffer #<n> by typing <n>e
function! s:bufSwitch(count)
    if count >=# 1
        return ":\<C-U>" . count . "b\<CR>"
    endif
    return 'e'
endfunction
nnoremap <expr> e <SID>bufSwitch(v:count)

Plug 'geetarista/ego.vim'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'justinmk/vim-sneak'
let g:sneak#s_next = 1
Plug '~/Projects/ghci.vim'

" Plug 'takac/vim-hardtime'
" let g:hardtime_default_on = 1
" let g:list_of_normal_keys = ["h", "j", "k", "l", "{", "}"]

call plug#end()

let mapleader = " "

nnoremap [[ :call searchpair('\[', '', '\]', 'bW')<CR>
nnoremap ]] :call searchpair('\[', '', '\]', 'W')<CR>
nnoremap ;; :w<CR>
nnoremap <cr> :
nnoremap # :e #<CR>
inoremap <C-X> <C-X><C-O>

nnoremap <Leader>evv :e ~/.vimrc<CR>
nnoremap <Leader>ev :e ~/.config/nvim/init.vim<CR>

colo ego

augroup automaticallySourceVimrc
  au!
  au bufwritepost init.vim source ~/.config/nvim/init.vim
augroup END

set hidden
set wildmenu
set shiftwidth=2
set wildmode=list:longest

au BufWritePre * execute "normal! ms:%s/\\v\\s\+$//e\<CR>`s"

set inccommand=nosplit


nnoremap <leader>sv :buffers<CR>:vert sb<SPACE>
nnoremap L <C-W><C-L>
nnoremap H <C-W><C-H>
nmap     <leader>td OTODO(sandy): <ESC>gccA

nnoremap : <nop>
nnoremap K <nop>
nnoremap <leader>cd :cd %:p:h<CR>
noremap  za <nop>
inoremap <F1> <nop>
nnoremap <F1> <nop>
vnoremap <F1> <nop>
map <t_%9> <nop>
imap <t_%9> <nop>
nnoremap Q <nop>
nnoremap gh <nop>
nnoremap <C-Q> <nop>

nnoremap / /\v
nnoremap <C-q> <C-W><C-q>
vnoremap < <gv
vnoremap > >gv
nnoremap * *N
nnoremap Y y$
nnoremap j gj
nnoremap k gk

nnoremap <Leader>s *N:noh<CR>:%s//
nnoremap :: :bp\|bd #<CR>
nnoremap <silent> <leader><space> :noh<cr>

" Help align visual blocks by delimiter
vmap <leader><space> <Plug>(EasyAlign)

" Things we can align on
let g:easy_align_delimiters = {
\ '[': { 'pattern': '[[\]]', 'left_margin': 0, 'right_margin': 0, 'stick_to_left': 0 },
\ '(': { 'pattern': '[()]', 'left_margin': 0, 'right_margin': 0, 'stick_to_left': 0 },
\ ']': { 'pattern': '[[\]]', 'left_margin': 1, 'right_margin': 0, 'stick_to_left': 0 },
\ ')': { 'pattern': '[()]', 'left_margin': 1, 'right_margin': 0, 'stick_to_left': 0 },
\ '<': { 'pattern': '[<]', 'left_margin': 1, 'right_margin': 0, 'stick_to_left': 0 },
\ '.': { 'pattern': '\.', 'left_margin': 1, 'right_margin': 1, 'stick_to_left': 0 },
\ '>': { 'pattern': '[->]', 'left_margin': 1, 'right_margin': 0, 'stick_to_left': 0 },
\ ':': { 'pattern': '::', 'left_margin': 1, 'right_margin': 1, 'stick_to_left': 0 },
\ '$': { 'pattern': '\$', 'left_margin': 1, 'right_margin': 1, 'stick_to_left': 0 },
\ '~': { 'pattern': '\.\~', 'left_margin': 1, 'right_margin': 1, 'stick_to_left': 0 },
\ '#': { 'pattern': '#', 'left_margin': 1, 'right_margin': 0, 'stick_to_left': 0 },
\ 'q': { 'pattern': '\(qualified\)\?\ze ', 'left_margin': 1, 'right_margin': 1, 'stick_to_left': 0 },
\ 'c': { 'pattern': '.\zs--', 'left_margin': 2, 'right_margin': 1, 'stick_to_left': 0, 'ignore_groups': [] },
\ }

" Buffer list
nnoremap gb :ls<CR>:b<Space>

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

function! ClipboardYank()
  call system('xclip -i -selection clipboard', @@)
endfunction
function! ClipboardPaste()
  let @@ = system('xclip -o -selection clipboard')
endfunction

vnoremap <silent> y y:call ClipboardYank()<cr>
vnoremap <silent> d d:call ClipboardYank()<cr>

" Jump back to same line when reopening
augroup line_return
    au!
    au BufReadPost *
        \ if line("'\"") > 0 && line("'\"") <= line("$") |
        \ execute 'normal! g`"zvzz' |
        \ endif
augroup END

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

set statusline=
set statusline +=%3*%y%*                "file type
set statusline +=%1*\ %n\ %*            "buffer number
set statusline +=%4*\ %<%f%*            "full path
set statusline +=%2*%m%*                "modified flag
set statusline +=%1*%=%2*\ =%{v:register}
set statusline +=%5l%*                  "current line
set statusline +=%2*/%L%*               "total lines
set statusline +=%1*%4v\ %*             "virtual column number

augroup removeftplugin
  au!
augroup END

augroup unmapCRInQuickfix
  au!
  autocmd BufReadPost,BufEnter quickfix nnoremap <buffer> <CR> <CR>
  autocmd BufReadPost,BufEnter quickfix nnoremap <buffer> : :
  autocmd BufReadPost,BufEnter quickfix nnoremap <buffer> q <C-W><C-Q>
  autocmd CmdwinEnter * nnoremap <buffer> <CR> <CR>
  autocmd CmdwinEnter * nnoremap <buffer> : :
  autocmd CmdwinEnter * nnoremap <buffer> q <C-W><C-Q>
augroup END

filetype plugin indent on
set autoindent
set backspace=indent,eol,start
set dictionary=/usr/share/dict/words
set expandtab
set formatoptions=qrn1
set gdefault
set grepprg=ag
set hlsearch
set ignorecase
set incsearch
set lazyredraw
set list listchars=tab:»·,trail:·
set nofoldenable
set number
set relativenumber
set ruler
set smartcase
set splitbelow
set splitright
set softtabstop=2
set tabstop=2
set timeoutlen=300
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

let g:grep_cmd_opts = '--line-numbers --noheading'

if executable('ag')
  set grepprg=ag\ --nogroup\ --nocolor
  let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
  let g:ctrlp_use_caching = 0
endif


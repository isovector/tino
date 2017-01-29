function! AddHsPragma()
    " Add a new HS pragma, and sort the list so it's pretty
    let pragma = input("LANGUAGE ")
    normal! ms
    if match(getline(1), "module") == 0
      execute "normal! ggO\<ESC>"
    endif
    if pragma != ""
        execute "normal! ggO{-# LANGUAGE " . pragma . " #-}\<ESC>"
    endif
    execute "normal! ggvip:sort\<CR>gv:EasyAlign -#\<CR>"
    normal `s
endfunction

nnoremap <buffer> <leader>l :call AddHsPragma()<CR>

" Transform haskell line into a pointfree version
nnoremap <buffer> <leader>pf V:! pointfree "`cat`"<CR>==

" Easy-to-type haskell digraphs
inoremap <buffer> ,. <space>~><space>
inoremap <buffer> -= <space>-><space>
inoremap <buffer> =- <space><-<space>
inoremap <buffer> +_ <space><=<space>
inoremap <buffer> _+ <space>=><space>

nnoremap <buffer> -- O<esc>78i-<esc>o<esc>i-- \|<space>

" Quick alignment of imports
nnoremap <silent><buffer> <leader>si magg/^import<CR>vip:EasyAlign q<CR>gv:sort /.*\%18v/<CR>:noh<CR>`a

" Better syntax highlighting
syntax keyword haskellTodo showTrace error undefined traceChanges unsafePerformIO fromJust unsafeCoerce trace

syntax keyword haskellNumber sample pick scanle newCollection center tags tagging findTag tag foldmp arrows keyPress onEvent poll sync async scaleRel mkRel origin move toStanza getX getY rect traced whenE run uniform uniformIn listOf uniformly filled styled mag distance posDif circle polygon runLift group go fcata acata rcata

syntax keyword haskellPragma load require fromConfig enter serve yield sinkList concatMap runConduit yieldMany iterM

syntax keyword haskellKeyword when unless flip const id maybe fmap map pure return sequence fst snd curry uncurry show read view set first second toS either forM_ mapM_ forM mapM join mempty mappend mconcat mzero fix traverse traverse_
syntax keyword haskellPragma ap ask filter foldl foldr not negate abs fromInteger div mod toInteger round truncate ceiling floor null length elem head tail any all concat and or take drop takeWhile dropWhile lookup zip zipWith lines words unlines unwords putStrLn print getChar getLine readFile writeFile isJust makePrisms makeLenses get put local liftIO def runReader runState runReaderT runStateT runWriter runWriterT fromEnum toEnum subtract fromIntegral forM lift liftM liftM2 liftM3 liftM4 liftM5 uncons minBound maxBound runIdentity coiter coiterT extract unwrap liftF runFree cata ana forall evalStateT execStateT evalState runState Just Nothing Left Right
syntax match haskellIdentifier /\v(S|M)\.(singleton|empty|insert|contains)/
syntax match haskellIdentifier /\vT\.(pack|unpack)/

syntax match haskellDecl /\v<(Has|To|From|Known|Monad|Sing)[A-Z][A-Za-z0-9'_]*>/
syntax keyword haskellDecl Show Read Dict1 Dict2 Monad Num Fractional Real Floating Integral Eq Ord Applicative Functor
syntax keyword haskellBottom DemoteRep Proxy Type Typeable
syntax keyword haskellKeyword m

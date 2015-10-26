syn keyword pythonThis cls self

syn match pythonChain '\v(^|\W)(cls|self)\zs(\s*\.\s*\h\w*)+' contains=pythonMember
syn match pythonMember '\h\w*' contained

hi link pythonThis Identifier
hi link pythonMember Function

syn keyword Identifier args
syn match LineNr /\vutils\ze\./
syn match Constant /\w\+=\@=/
syn match Function /\v\w+\ze\(\@=/
syn match Special /\v(messages|msgs)\.\zs\w+/

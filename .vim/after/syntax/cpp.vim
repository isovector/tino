syn match    cCustomParen    "?=(" contains=cParen,cCppParen
syn match    cCustomFunc     "\w\+\s*(\@=" contains=cCustomParen
syn match    cCustomScope    "::"
syn match    cCustomClass    "\w\+\s*::" contains=cCustomScope

hi def link cCustomFunc  Function
hi def link cCustomClass Constant

syn match   cppNameSpace  "\w\+::"
hi def link cppNameSpace  LineNr

syn match   cppUtilStatus  "util::Status"
hi def link cppUtilStatus  Type

syntax keyword Type string

syn match   cppExpect  "\v[A-Z_]+\(@="
hi def link cppExpect  Statement

syn match   cppFlag  "\vFLAGS_\w+"
hi def link cppFlag  Character

highlight Search guibg='Purple' guifg='NONE'


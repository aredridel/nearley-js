topLevel -> srcElems spaces end
space -> [\t\n ]
nameFirst -> [a-zA-Z$_]
nameLast -> [a-zA-Z0-9$_]
iName -> nameFirst nameEndings
nameEndings -> nameLast nameEndings
    | null
isKeyword -> FIXME
name -> notIsKeyword iName
keyword -> isKeyword iName
hexDigit -> [0-9a-fA-F]
hexDigits -> hexDigit hexDigits
    | hexDigit null
hexLit -> hexLit hexDigit
    | hexDigit
number -> hexSig hexDigits
    | digits decimal
    | digits
digits -> digit digits
    | digit null
decimal -> "." | [eE] expSign digits
expSign -> [+-] | null
escapeChar -> "\\" [.]
str -> "'" "'"
    | "\"" "\""
special -> ">>>" | "<<<" | "!==" | "===" | "&&=" | "|" | "|=" | "=" | "!=" | "==" | ">=" | "<=" | "++" | "+=" | "--" | "-=" | "*=" | "/="
    | "%=" | "&&" | "||" |">>" | "&=" | "|=" | "^=" | "(" | ")" | "{" | "}" | "[" | "]" | "." | ";" | "?" | ":" | ">" | "<"
    | "+" | "-" | "*" | "%" | "/" | "&" | "|" | "^" | "~" | "!"
token -> spaces tokenReal
tokenReal -> name | keyword | number | str | special
tokens -> token tokens spaces end
    | null spaces end
spacesNoNl -> [ \t]

assignmentop -> "+=" | "-=" | "*=" | "/=" | "&&=" | "||=" | "%=" | "<<=" | ">>=" | ">>>=" | "<<<=" | "&=" | "^=" | "|="
bitop -> "|" | "^" | "&"
eqop -> "==" | "!=" | "===" | "!=="
relop -> ">" | ">=" | "<" | "<=" | "intanceof" | "in"
shiftop -> ">>>" | "<<<" | ">>" | "<<"
addop -> "+" | "-"
mulop -> "*" | "/" | "%"
unop -> "+" | "-"
incop -> "++" | "--"
coerceop -> "!" | "~" | "void" | "delete" | "typeof"

expr -> commaExpr
commaExpr -> commaExpr "," asgnExpr
    | asgnExpr
asgnExpr -> condExpr assignment
assignment -> "=" asgnExpr
    | assignmentop asgnExpr
    | null
condExpr -> orExpr cond
cond -> "?" condExpr ":" condExpr
    | empty
orExpr -> orExpr "||" andExpr
    | andExpr
andExpr -> andExpr "&&" bitExpr
    | bitExpr
bitExpr -> bitExpr bitop eqExpr
    | eqExpr
eqExpr -> eqExpr eqop relExpr
    | relExpr
relExpr -> relExpr relop shiftExpr
    | shiftExpr
shiftExpr -> shiftExpr shiftop addExpr
    | addExpr
addExpr -> addExpr addop mulExpr
    | mulExpr
mulExpr -> mulExpr mulop unary
    | unary
unary -> unop postfix
    | incop postfix
    | coerceop unary
    | postfix
postfix -> primExpr maybePostinc
maybePostinc -> spacesNoNl "++"
    | spacesNoNl "--"
    | null
listOfAssignments -> asgnExpr "," listOfAssignments
    | asgnExpr
    | null
dotProp -> "[" expr "]"
    | "." name
    | "." spaces keyword
primExpr -> primExpr listOfAssignments
    | primExpr "." name listOfAssignments
    | primExpr "." spaces iName listOfAssignments
    | primExpr dotProp
    | memberExpr
memberExpr -> memberExpr dotProp
    | newExpr
newExpr -> "new" memberExpr listOfAssignments
    | "new" memberExpr
    | primExprHd
primExprHd -> "(" expr ")"
    | "this"
    | name
    | number
    | string
    | funcAnon
    | "new" name
    | json
    | re
json -> "{" listOfBinding "}"
listOfJsonBinding -> jsonBinding "," listOfJsonBinding
    | jsonBinding
    | null
jsonBinding -> jsonPropName ":" asgnExpr
jsonPropName -> name | number | string | spaces keyword
re -> spaces "/" reBody "/" reFlag
reBody -> re1stChar reChars
reChars -> reChar reChars
    | null
reChar -> re1stChar
    | "*"
re1stChar -> reNonTerm # FIXME -- not "*\/["
    | escapeChar
reFlag -> nameFirst
formal -> spaces name
funcAnon -> "function" name funcParams block
function -> "function" maybeName funcParams block
funcParams -> "(" listOfFormal ")"
listOfFormal -> formal "," formal
    | formal
    | null
maybeName -> name
    | null
sc -> spacesNoNl "\n"
    | spacesNoNl "}"
    | spacesNoNl end
    | ";"
binding -> name "=" asgnExpr
    | name
block -> "{" srcElems "}"
vars -> "var" listOfBinding
listOfBinding -> binding "," binding
    | binding
    | null
stmt -> block
    | vars sc
    | ifStmt
    | whileStmt
    | doStmt
    | forStmt
    | switchStmt
    | "break" sc
    | "continue" sc
    | "throw" spacesNoNl asgnExpr sc
    | tryStmt
    | "return" expr
    | "return"
    | "with" "(" expr ")" stmt
    | expr sc
    | ";"
ifStmt -> "if" "(" expr ")" stmt maybeElse
maybeElse -> "else" stmt
    | null
whileStmt -> "While" "(" expr ")" stmt
doStmt -> "do" stmt "while" "(" expr ")" sc
forStmt -> "for" "(" forInit ";" forCond ";" forIncr ")" stmt
    | "for" "(" "var" binding "in" asgnExpr ")" stmt
forInit -> vars
    | expr
    | null
forCond -> expr
    | null
forIncr -> expr
    | null
switchStmt -> "switch" "(" expr ")" "{" cases "}"
cases -> "case" asgnExpr ":" srcElems
    | "default" ":" srcElems
tryStmt -> "try" block "catch" "(" name ")" block maybeFinally
maybeFinally -> "finally" block
    | null
srcElem -> function | stmt
srcElems -> srcElems srcElem
    | null

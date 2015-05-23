# haskellnotes
## Classy Prelude
```haskell
{-# LANGUAGE OverloadedStrings #-}

import Prelude()
import ClassyPrelude
```

Classy prelude replaces classic prelude and,

1. Removed partial functions, such as *head*, *tail*, *init*, etc. Use *headEx*, *tailEx*, instead.
2. Using typeclasses to generalize common functions (for list) to other data structures.
3. Replacing String with Text.
4. Included more packages.
5. For type ambiguities, use asList, asMap, asXXX to coerce.

## Notes about parsec using Text as base streaming type

Import modules:

```haskell
import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Expr
import Text.Parsec.Text
```

Template used to declare a parser:

```haskell
myparser :: Parsec Text State Return
myparser :: Parser Return -- If state is not required, default to Text type
-- Often used Combinators
-- (<|>) (<?>) try unexpected choice many many1 between option sepBy endBy eof ...
-- Use getState, setState and updateState to access States
```

Template to define a token parser: don't use Text.Parsec.Language module as it is specialized to String type

```haskell
langdef :: GenLanguageDef Text st Identity
langdef = LanguageDef
  { commentStart   = "{-"
  , commentEnd     = "-}"
  , commentLine    = "--"
  , nestedComments = True
  , identStart     = letter
  , identLetter    = alphaNum <|> oneOf "_'"
  , opStart        = oneOf "+-*/"
  , opLetter       = oneOf "+-*/:"
  , reservedOpNames= []
  , reservedNames  = []
  , caseSensitive  = True
  }
lexer = makeTokenParser langdef
ident = map pack $ identifier lexer -- identifier returns String, need to pack it
ws    = whiteSpace lexer
-- ...
```

Expression parser template:
```haskell
expr :: Parser Return
expr    = buildExpressionParser table term
      <?> "expression"
talbe   = [ [prefix "-" Neg, prefix "+" Pos, postfix "++" Pl1]
          , [binary "*" Mul AssocLeft, binary "/" Div AssocLeft ]
          , [binary "+" Add AssocLeft, binary "-" Min AssocLeft ] ]
binary  name fun assoc = Infix (do{ reservedOp lexer name; return (\x y -> Node fun [x,y])}) assoc
prefix  name fun       = Prefix (do{ reservedOp lexer name; return $ Node fun . (:[])})
postfix name fun       = Postfix (do{ reservedOp lexer name; return $ Node fun . (:[])})
```

Runs a parser:

```haskell
case runP myparser initialState filename sourceStream of
  Left  e -> do error "parser error"
  Right r -> processReturn r
case parse myparser filename sourceStream of
  Left  e -> ...
  Right r -> ...
```

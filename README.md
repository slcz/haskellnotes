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

## Safely handle files

Notice the following code, need to provide type signature for handle's exception handling,
```haskell
handle ((\_ -> return ()) :: IOException -> IO ()) $
    braket (openFile path ReadMode)
            hClose
            $ \handle -> do
              -- use handle
              return True
```

## Commandline parsing

First, declare flags/switches as a record:
```haskell
import System.Console.GetOpt

data Options = Options
     { optVerbose     :: Bool
     , optShowVersion :: Bool
     , optOutput      :: Maybe FilePath
     , optInput       :: Maybe FilePath
     , optLibDirs     :: [FilePath]
     } deriving Show
```

Define default options
```haskell
defaultOptions    = Options
     { optVerbose     = False
     , optShowVersion = False
     , optOutput      = Nothing
     , optInput       = Nothing
     , optLibDirs     = []
     }
```

Define a list of *functions* that aggregate on the default value:

```haskell
options :: [OptDescr (Options -> Options)]
options =
 [ Option "v" ["verbose"]
     (NoArg (\ opts -> opts { optVerbose = True }))
     "chatty output on stderr"
 , Option "V" ["version"]
     (NoArg (\ opts -> opts { optShowVersion = True }))
     "show version number"
 , Option "o" ["output"]
     (OptArg ((\ f opts -> opts { optOutput = Just f }) . fromMaybe "output")
             "FILE")
     "output FILE"
 , Option "c" ["input"]
     (OptArg ((\ f opts -> opts { optInput = Just f }) . fromMaybe "input")
             "FILE")
     "input FILE"
 , Option "L" ["lib"]
     (ReqArg (\ d opts -> opts { optLibDirs = optLibDirs opts ++ [d] }) "DIR")
     "library directory"
 ]
 ```

Using *foldl'* to apply outstanding options on top of defaults. For errors, print out error message using *usageInfo* helper.

```haskell 
compilerOpts :: [String] -> IO (Options, [String])
compilerOpts argv =
   case getOpt Permute options argv of
      (o,n,[]  ) -> return (foldl' (flip id) defaultOptions o, n)
      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
   where header = "Usage: ic [OPTION...] files..."
   ```

Parsing options with effect:

```haskell
options :: [OptDescr (Options -> IO Options)]
options =
 [ Option "v" ["verbose"]
     (NoArg (\ opts -> return $ opts { optVerbose = True }))
     "chatty output on stderr"
 , Option "o" ["output"]
     (OptArg (\ f opts -> return $ opts { optOutput = f }) "FILE")
     "output FILE"
...
   case getOpt Permute options argv of
      (o,n,[]  ) -> do o' <- foldM (flip id) defaultOptions o
                       return (o', n)
```
https://wiki.haskell.org/High-level_option_handling_with_GetOpt

## Haksell Containers
----------------------------------------------------------------------------------------------------------
Map k a | HashMap k v | IntMap a | Set a | HashSet a | IntSet | Seq a | Vector a | UVector a | SVector a
----------------------------------------------------------------------------------------------------------

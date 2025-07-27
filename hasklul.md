```hs
module Main where

import Data.Kind (Type)

data Expr :: Type -> Type
  where
    EInt :: Int -> Expr Int
    EBinOp :: (Int -> Int -> Int) -> Expr Int -> Expr Int -> Expr Int

eval :: Expr Int -> Int
eval (EInt i)          = i
eval (EBinOp op ea eb) = eval ea `op` eval eb

add :: Int -> Int -> Int
add a b = a + b

mul :: Int -> Int -> Int
mul = (*)

sample :: Expr Int
sample = EBinOp add (EInt 34) (EBinOp mul (EInt 5) (EInt 7))
-- sample = EInt 34 `EBinOp add` (EInt 5 `EBinOp mul` EInt 7)

main :: IO ()
main = do
    putStrLn "Hello"
    print $ eval sample
```
----------------------------------------

    module Main

    export : main, Expr(..)

    open import Data.Kind : Type  -- == import Data.Kind (Type)
    import Data.Kind as K : Type  -- == import Data.Kind qualified as K (Type)
    import Data.Kind as K		  -- == import Data.Kind qualified as K

    K.Type

	-- | this two lines equal to
	-- @import Data.Text qualified as T (pack, unpack)@
	-- @import Data.Text (Text)@
    import Data.Text as T : pack, unpack
    open T : Text

    -- or should I use @import : ...@ like g*lang
    import :
        Data.Text as T : unlines lines

    open :
        Data.Text : Text pack unpack

    -- ^ that is just yaml lol

    data Expr : Type -> Type
        int   : Int -> Expr Int
        binop : (Int -> Int -> Int) -> Expr Int -> Expr Int -> Expr Int
        deriving Show, Eq, Generic

    -- open Expr in
    eval : Expr Int -> Int
    ... Expr.int i      = i                     -- you have to use qualified name for constructor
    ... .binop op ea eb = eval ea `op` eval eb  -- you can ommit the Type name if it is known

    add : Int -> Int -> Int -- uses @:@ instead of @::@
    ... a, b = a + b        -- uses @...@ instead of function name

    mul : Int -> Int -> Int
    ... = _*_ -- uses @_@ instead of @()@

    sub of Int -> Int -> Int -- you can use @of@ as @:@
    ... is λa, b -> a - b    -- using @is@ binding. lambda uses @λ@ instead of @\@

    -- | @is@ binding can be used in place of @=@ but not for function that is
    -- not using lambda notation

    -- * @is@ is used for single, non-pattern-matching bindings.
    -- * @=@ is used within pattern-matching clauses.
    -- * @as@ is a distinct binding operator.
    -- * Using @,@ to separate patterns (... a, b = ...) is actually less ambiguous
    --   for a parser than Haskell's space-separated patterns.

    open Expr in -- you can open the namespace of a Type temporarely
    sample : Expr Int
    ... = binop add (int 34) (binop mul (int 5) (int 7)) -- so you don't have to qualify @binop@ and @int@
    -- or
    ... is binop add (int 34) (binop mul (int 5) (int 7)) -- using @is@ binding
    -- or
    ... = int 34 `binop add` $ int 5 `binop mul` int 7 -- you can use infixing @``@ with arbitrary complex expression
    -- or
    ... = i34 `binop add` i7 * 5
      where
        i34 is int 34  -- you can use @is@ binding inside @where@ clause
        17 * 5 is int 5 `binop mul` int 7

    -- | you can also make @of@ type annotation and @is@ binding in one line
    -- this is equivalent to
    -- @sample :: Expr Int@
    -- @sample = binop add (int 34) (binop mul (int 5) (int 7))@
    sample of Expr Int is binop add (int 34) (binop mul (int 5) (int 7))

    main : IO ()
    ... = do
        "Hello".writeln      -- dot notation from Lean4
        print $ eval sample
        print sample.eval    -- simpler using dot notation
        -- | roughly euqal to @[1..10] `map` \i -> show $ succ i@
        print $ [1..10].map λi -> show i.succ

    -- or
    main does  -- using @does@ instead of @= do@
        "Hello".writeln
        print $ eval sample
        print sample.eval
        print $ [1..10].map λi -> show i.succ

    -- other main functions
    main : IO ExitCode       -- a program that uses @IO@ and return an exit code
    main : Show a => IO a    -- a program that uses @IO@ and print a showable result
    main : [String] -> IO a  -- a program that recieve cli args as list of string. here @a@ can be @()@ or @ExitCode@ or @Show a => a@
    main : String -> IO a    -- a program that recieve cli args but as a single string.

    -- so you can do
    main : [String] -> IO ()
    ... "version" :: _ = writeln "0.1.12"  -- list cons uses @::@ instead of @:@
    ... "-h" :: a :: _ = printHelp a
    ... "-h" :: _      = printUsage

    -- or
    main : IO ExitCode
    ... = do
        some_func
        return 0  -- where 0 is ExitCode or @0 : ExitCode@

    -- or
    main : IO String  -- this is the form of @main : Show a => IO a@ where a is String
    ... does
        return "Hello, World!"
    -- ^ will print the returned value

    -- the above could be written as
    main of IO String does return "Hello, World!"

    -- Note: I don't like this idea
    -- do we have did?
    -- if do is for monad, did is for comonad

    data Zipper a
        zipper of [a] -> a -> [a]

    fun : Zipper Int -> Zipper Int
    ... z = did
        x <- z
        extract x

    map : (a -> b) -> [a] -> [b]
    ... _, []      = []               -- case analysis uses comma @,@ for separating arguments
    ... f, x :: xs = f x :: map f xs

    -- [1..10].map λi -> show i.succ
    -- ([1..10].map) `apply` λi -> show i.succ
    -- :
    -- ((Int -> b) -> [b]) `apply` (Int -> String)
    -- [String]

    -- | similar concept in Agda. but i think this is like C's @#define@ macro
    variable
        a is 1
        b is 4

    class Functor f  -- notice we don't need @where@ here
        map of (a -> b) -> f a -> f b  -- uses @of@ instead of @:@

    class Functor f => Applicative f
        pure of a -> f a
        apl  of f (a -> b) -> f a -> f b

    class Applicative m => Monad m
        return of a -> m a
        bind   of m a -> (a -> m b) -> m b

    data Option a
        none of Option a       -- constructor is in lower case
        some of a -> Option a

    instance Functor Option
        map _, none = none          -- this is a definition with out a type annotation
        ... f, some a = some $ f a

    instance Applicative Option
        pure is some  -- using eta-reduction so I can use @is@ binding
        apl none  , _      = none
        ... _     , none   = none
        ... some f, some a = some $ f a

    instance Monad Option
        return is pure
        bind none  , _ = none
        ...  some a, f = f a

        -- or
        bind is fun where   -- here @fun@ is a binding
            fun none  , _ = none
            ... some a, f = f a
        -- ^ haskell equivalent is
        -- @bind = fun@
        -- @  where@
        -- @    fun None     _ = None@
        -- @    fun (Some a) f = f a@

        -- or
        bind is λ  -- @is λ@ binding
        ... none  , _ = none
        ... some a, f = f a

    infixl 7 _>>=_
    infixl 5 _<*>_ _<$>_

    _>>=_ is bind
    _<*>_ is apl
    _<$>_ is map

    -- view pattern
    length_is_three : [a] -> Bool  -- name can use snake case
    ... length -> 3 = true         -- like Haskell ViewPattern but with no paren
    ... _ = false

    length_plus_100 : [a] -> Nat
    ... length -> l = l + 100

    -- ^ is the same as
    length_plus_100 : [a] -> Nat
    ... xs = length xs + 100

    -- or more precisely
    ... xs = length xs as l; l + 100  -- the @as@ binding. it is like @let in@ binding in Haskell

    -- these two are equivalent. @ident@ is identifier
    -- ident is expr
    -- expr as ident

    -- the syntax is
    -- fun ident where ident is expr  -- using @is@
    -- expr as ident; fun ident       -- using @as@, the semicolon @;@ is mandatory

    six_nine of Int
    ... is a + b
      where a is 34; b is 7 * 5  -- @where@ clause's layout rule is like in Haskell

    -- or
    six_nine of Int
    ... is a + b
      where
        a is 34
        b is 7 * 5

    -- or
    six_nine of Int
    ... = 34 as a; 7 * 5 as b; a + b  -- using @as@ binding

    -- or
    -- Note: should I use ';'? I think so.
    six_nine of Int
    ... is
        34 as a
        7 * 5 as b
        a + b

    -- but maybe not this
    six_nine of Int
    ... =
        a is 34
        b is 7 * 5
        a + b

    -- | other examples
    splice : Nat -> [a] -> [[a]]
    ... _, [] = []
    ... n, l  = splitAt n l |> λ(x, xs) -> x :: splice n xs

    -- splice 2 [1..10]
    -- splice 2 [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    -- splitAt 2 [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] |> λ(x, xs) -> x :: splice n xs
    -- ([1, 2], [3, 4, 5, 6, 7, 8, 9, 10]) |> λ(x, xs) -> x :: splice n xs
    -- [1, 2] :: splice 2 [3, 4, 5, 6, 7, 8, 9, 10]
    -- [1, 2] :: splitAt 2 [3, 4, 5, 6, 7, 8, 9, 10] |> λ(x, xs) -> x :: splice n xs
    -- ...
    -- [1, 2] :: [3, 4] :: .. :: [9, 10] :: []

    -- or
    splice : Nat -> [a] -> [[a]]
    ... _, []                   = []
    ... n, splitAt n -> (x, xs) = x :: splice n xs  -- using view pattern

    -- splice 2 [1..10]
    -- splice 2 (12, 3-10) => 12 :: splice 2 3-10

    fun of Maybe Int -> Int
    ... mi = case mi of  -- normal @case of@
        nothing -> 0
        just i  -> i


----------------------------------------

More Example:

    module FailingStatefulParser

    export : main Parser(..) parse word take_until
    -- export : main, Parser(..), parse, word, take_until

    import :
        Data.Text as Text

    open :
        Control.Monad
        Data.Char : is_space
        Data.Text : Text

    -- open import Control.Monad

    -- open import Data.Char : is_space

    -- import Data.Text as Text
    -- open Text : Text

    newtype State s a =
        State { run_state of s -> (a, s) }

    instance Functor (State s)
        map f st = State λold ->
            run_state st old as (val, new);
            (f val, new)

    instance Applicative (State s)
        pure val = State (val ,_)

        f <*> g = State λold ->
            runState f old as (fa, fs);
            runState g fs  as (ga, gs);
            (fa ga, gs)

    instance Monad (State s)
        return is pure

        f >>= g = State λold ->
            runState f old as (fa, fs);
            runState (g fa, fs)

    eval_state of State s a -> s -> a
    ... State f, s = fst $ f s

    exec_state of State s a -> s -> s
    ... State f, s = snd $ f s

    put of s -> State s ()
    ... s = State (const ((), s))

    get of State s s
    ... is State λs -> (s, s)

    read_state of String -> State String Int
    ... str = do
        strs <- get
        ss is strs ++ str
        put ss
        return $ read ss

    -- | using @(<- action)@ like Lean4
    read_state' of String -> State String Int
    ... str = do
        ss is (<- get) ++ str
        put ss
        return $ read ss

    type Input      is Text
    type ParseError is String

    newtype Parser a = Parser
        { run_parser of State Input (Either ParseError a) }

    eval_parser of Parser a -> Input -> Either ParseError a
    ... is eval_state . run_parser

    parse of Parser a -> Input -> (Either ParseError a, Input)
    ... is run_state . run_parser

    instance Functor Parser
        fmap f p =
            Parser $ fmap f <$> run_parser p

    instance Applicative Parser
        pure is Parser . pure . pure

        f <*> a = Parser do
            f' <- run_parser f
            a' <- run_parser a
            pure $ f' <*> a'

        -- | using @(<- action)@ like Lean4
        -- f <*> a = Parser do
        --     pure $ (<- run_parser f) <*> (<- run_parser a)

    instance Monad Parser
        return is pure

        a >>= f = Parser $
            run_parser a >>= either (pure . Left) (run_parser . f)

    parse_error of ParseError -> Parser a
    ... is Parser . pure . Left

    parse_get of Parser Input
    ... is Parser $ Right <$> get

    parse_put of Input -> Parser ()
    ... input = Parser $ Right <$> put input

    take_until of (Char -> Bool) -> Parser Text
    ... p = do
        oldst <- parse_get
        Text.break p oldst as (nextval, rest);
        parse_put rest
        pure nextval

    -- | using @(<- action)@ like Lean4
    take_until'' of (Char -> Bool) -> Parser Text
    ... p = do
        Text.break p (<- parse_get) as (nextval, rest);
        parse_put rest
        pure nextval

    take_until' of (Char -> Bool) -> Parser Text
    ... p = do
        (nextval, rest) <- Text.break p <$> parseGet
        parsePut rest
        pure nextval

    optionally of Parser a -> Parser ()
    ... parser = Parser do
        oldst  <- get
        result <- run_parser parser
        case result of
            left{} -> put  oldst
            _ok    -> pure ()
        pure (right ())

    -- | using @(<- action)@ like Lean4
    optionally' of Parser a -> Parser ()
    ... parser = Parser do
        case (<- run_parser parser) of
            left{} -> put (<- get)
            _ok    -> pure ()
        pure (right ())

    word of Parser Text
    ... does
        nextword <- take_until is_space
        when (Text.null nextword) do
            parse_error "unexpected end of input"
        optionally drop_char
        pure nextword

    drop_char of Parser ()
    ... does
        st <- parse_get
        case Text.uncons st of
            nothing        -> parse_error "unexpected end of input"
            just (_, rest) -> parse_put rest

    -- | using @(<- action)@ like Lean4
    drop_char' of Parser ()
    ... does
        case Text.uncons (<- parse_get) of
            nothing        -> parse_error "unexpected end of input"
            just (_, rest) -> parse_put rest

    data FullName = FullName
        { frst of Text
        , midl of Text
        , last of Text
        } deriving Show

    parse_full_name of Parser FullName
    ... is FullName <$> word <*> word <*> word

    -- | for @val |> func@
    _|>_ of a -> (a -> b) -> b
    ... a f = f a

    infixl 2 _|>_

    main of IO ()
    ... does
        Text.pack "Muhammad Fakhrur Rozi"
        |> parse parse_full_name
        |> print

equals to

```hs
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FunctionalDependencies #-}

module FailingStatefulParser (
    main,
    Parser(..),
    parse,
    takeUntil,
    word,
) where

import Control.Monad
import Data.Char (isSpace)
import Data.Text (Text)
import Data.Text qualified as Text

newtype State s a = State
    { runState :: s -> (a, s) }

instance Functor (State s) where
    fmap f st = State \old ->
        let (val, new) = runState st old
         in (f val, new)

instance Applicative (State s) where
    pure val = State (val,)

    f <*> g = State \old ->
        let (fa, fs) = runState f old
            (ga, gs) = runState g fs
         in (fa ga, gs)

instance Monad (State s) where
    return = pure

    f >>= g = State \old ->
        let (fa, fs) = runState f old
         in runState (g fa) fs

evalState :: State s a -> s -> a
evalState (State f) s = fst $ f s

execState :: State s a -> s -> s
execState (State f) s = snd $ f s

put :: s -> State s ()
put s = State (const ((), s))

get :: State s s
get = State \s -> (s, s)

readSt :: String -> State String Int
readSt str = do
    strs <- get
    let ss = strs ++ str
    put ss
    return $ read ss

type Input      = Text
type ParseError = String

newtype Parser a = Parser
    { runParser :: State Input (Either ParseError a) }

evalParser :: Parser a -> Input -> Either ParseError a
evalParser = evalState . runParser

parse :: Parser a -> Input -> (Either ParseError a, Input)
parse = runState . runParser

instance Functor Parser where
    fmap f p =
        Parser $ f <$$> runParser p
        Parser $ fmap f <$> runParser p

instance Applicative Parser where
    pure = Parser . pure . pure
    f <*> a = Parser do
        f' <- runParser f
        a' <- runParser a
        pure $ f' <*> a'

instance Monad Parser where
    return = pure
    a >>= f = Parser $
        runParser a >>= either (pure . Left) (runParser . f)

parseError :: ParseError -> Parser a
parseError = Parser . pure . Left

parseGet :: Parser Input
parseGet = Parser $ Right <$> get

parsePut :: Input -> Parser ()
parsePut input = Parser $ Right <$> put input

takeUntil :: (Char -> Bool) -> Parser Text
takeUntil p = do
    oldst <- parseGet
    let (nextval, rest) = Text.break p oldst
    parsePut rest
    pure nextval


takeUntil' :: (Char -> Bool) -> Parser Text
takeUntil' p = do
    (nextval, rest) <- Text.break p <$> parseGet
    parsePut rest
    pure nextval

optionally :: Parser a -> Parser ()
optionally parser = Parser do
    oldst  <- get
    result <- runParser parser
    case result of
        Left{} -> put  oldst
        _ok    -> pure ()
    pure (Right ())

word :: Parser Text
word = do
    nextword <- takeUntil isSpace
    when (Text.null nextword) do
        parseError "unexpected end of input"
    optionally dropChar
    pure nextword

dropChar :: Parser ()
dropChar = do
    st <- parseGet
    case Text.uncons st of
        Nothing -> parseError "unexpected end of input"
        Just (_, rest) -> parsePut rest

data FullName = FullName
    { frst :: Text
    , midl :: Text
    , last :: Text
    } deriving Show

parseFullName :: Parser FullName
parseFullName = FullName <$> word <*> word <*> word

infixl 2 |>
(|>) = flip id

main :: IO ()
main =
    Text.pack "Muhammad Fakhrur Rozi"
    |> parse parseFullName
    |> print
```



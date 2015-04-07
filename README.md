# Haskell Style Guide

These is the style I practice writing Haskell code.

My principles are:

* If performance is not important, prefer readability over efficiency

* Styles should be easy to adapt even without the help of any text editor

## Formatting

### Line Length

Line length should be kept within **80 characters**.
But maximum line length is **100 characters**.

### Indentation

* Tabs are illegal. Use spaces for indenting.

* Indent your code blocks with **4 spaces**.

* Indent the definitions in a `where` clause 4 spaces,
but indent `where` clause with 2 spaces.

* Never align `where` the same as its previous or next line.

Some examples:

```haskell
sayHello :: IO ()
sayHello = do
    name <- getLine
    putStrLn $ greeting name
  where
    greeting name = "Hello, " ++ name ++ "!"

filter :: (a -> Bool) -> [a] -> [a]
filter _ []   = []
filter p (x:xs)
  | p x       = x : filter p xs
  | otherwise = filter p xs
```

### Blank Lines

* One blank line between top-level definitions.

* No blank lines between type signatures and function definitions.

* Add one blank line between functions in a type class instance declaration
if the functions bodies are large. Use your judgment.

### Whitespace

* Surround binary operators with a single space on either side.

* For arithmetic operators, tighter operators should have fewer surrounding spaces.

```haskell
1 + 1          -- ok
1+1            -- ok
1+8*9+9        -- ok
1 + 8*9 + 9    -- ok
1+8 * 9+9      -- no for the obvious reason
```

* Don't insert a space after a lambda.

### Data Declarations

Align the constructors in a data type definition.  Example:

```haskell
data Tree a
  = Branch !a !(Tree a) !(Tree a)
  | Leaf
```

Format records as follows:

```haskell
data Person = Person
  { firstName :: !String  -- ^ First name
  , lastName  :: !String  -- ^ Last name
  , age       :: !Int     -- ^ Age
  } deriving (Eq, Show)
```

Double colons alignings are optional.

### List Declarations

Align the elements in the list.  Example:

```haskell
exceptions =
  [ InvalidStatusCode
  , MissingContentHeader
  , InternalServerError
  ]
```

Don't do this:

```haskell
directions = [ North
             , East
             , South
             , West
             ]
```

As when you change the name of `directions`,
the alignment will be out-of-sync.

### Pragmas

For this part I don't have any preferences, keep the old content
for completeness.

Put pragmas immediately following the function they apply to.
Example:

```haskell
id :: a -> a
id x = x
{-# INLINE id #-}
```

In the case of data type definitions you must put the pragma before
the type it applies to.  Example:

```haskell
data Array e = Array
    {-# UNPACK #-} !Int
    !ByteArray
```

### Hanging Lambdas

You may or may not indent the code following a "hanging" lambda.  Use
your judgement. Some examples:

```haskell
bar :: IO ()
bar = forM_ [1, 2, 3] $ \n -> do
          putStrLn "Here comes a number!"
          print n
```

If you find yourself writing many hanging lambdas, don't hesitate to use
do notation.

Instead of:

```haskell
foo :: IO ()
foo = alloca 10 $ \a ->
      alloca 20 $ \b ->
      cFunction a b
```

You can do:

```haskell
foo :: IO ()
foo = do
  a <- alloca 10
  b <- alloca 20
  cFunction a b
```

Or even better:

```haskell
foo :: IO ()
foo = cFunction <$> alloca 10
                <*> alloca 20
```

Prefer `<$>` and `<*>` combinations over `liftAN` and `liftMN` families.

Whenever the lambda is used in something that implies a context (such as loops
or resource management), make sure to indent the code in the lambda.

### Export Lists

Format export lists as follows:

```haskell
module Data.Set (
    -- * The @Set@ type
    Set
  , empty
  , singleton

    -- * Querying
  , member
) where
```

If you don't have Haddock comments, the following is also acceptable:

```haskell
module Data.Set (
  Set,
  empty,
  singleton,
  member,
) where
```

### If-then-else clauses

Generally, guards and pattern matches should be preferred over if-then-else
clauses, where possible.  Short cases should usually be put on a single line
(when line length allows it).

* Never align `if` the same as `then` clause and `else` clause.

* Always align `then` and `else`.

* `DoAndIfThenElse` pragma **is illegal**.

When writing non-monadic code (i.e. when not using `do`) and guards
and pattern matches can't be used, you can align if-then-else clauses
you like you would normal expressions:

```haskell
foo = if ...
        then ...
        else ...
```

Or


```haskell
foo =
  if ...
    then ...
    else ...
```

### Case expressions

The alternatives in a case expression can be indented using either of
the two following styles:

```haskell
foobar = case something of
  Just j  -> foo
  Nothing -> bar
```

or as

```haskell
foobar = case something of
             Just j  -> foo
             Nothing -> bar
```

But prefer the first one, save your spaces.

Align the `->` arrows when it helps readability.

## Extensions


Languages are grouped into three categories:

1. These should always be enabled.
In order to work well with tools such as `hdevtools` and `hlint`,
these should still be put in top-of-file pragmas in addition to the `*.cabal` file.
2. Acceptable for common use.
3. Should not be used.

These may change over time,
but all code should be written with these classifications in mind.
When in doubt, ask for clarification.

### Encouraged Extensions

`OverloadedStrings`: Enable whenever you are working
with `ByteString`s, `Text`, or `FilePath`s, and definitely whenever working with `ClassyPrelude`.

`NoImplicitPrelude`: In a project that uses `ClassyPrelude`,
*all* files should use `ClassyPrelude`.
You may import functions you need from `Prelude` and
hide things from `ClassyPrelude`, but should keep this to a minimum.

`Derive*`: All extensions which enable deriving
instances of `Typeable`, `Functor`, `Applicative`, `Generic`, etc,
are encouraged, as they reduce boilerplate significantly.

`FlexibleInstances`, `FlexibleContexts`: Allow writing flexible typeclasses.

`BangPatterns`: Enable strictness annotations.

### Allowed Extensions

`ScopedTypeVariables`: For writing clear type signatures, especially in `where` clauses.

`TypeSynonymInstances`: Especially where the underlying type is supposed to be mostly hidden.

`NoMonomorphismRestriction`: Only to be used in code which requires it,
such as when working with Diagrams or other similar packages.

`TemplateHaskell`: To be used sparingly.

Most type system extensions,
such as `ExistentialQuantification`, `GADTs`, `TypeFamilies` are acceptable.

### Forbidden Extensions

* `DoAndIfThenElse`: not to indent `if` the same as `then` and `else`

* `UnicodeSyntax`: the only place where unicode characters can show up
is in string literals.

* `ViewPatterns`

When in doubt, ask for clarification.

## Imports

Imports should be grouped in the following order:

1. standard library imports
2. related third party imports
3. local application/library specific imports

* Group imports base on your judgment.

* Alphabetical import list is not encouraged, there are things more important

* Explicit import list is not encouraged, as adding and removing every
definition becomes a pain. Use qualified import when there are conflicts.

* When working in a project that uses ClassyPrelude, make sure to include
`NoImplicitPrelude` pragma at the top of the file and import ClassyPrelude
instead of using Prelude.

## Comments

### Punctuation

Write proper sentences; start with a capital letter and use proper
punctuation. *This is important.*

### Top-Level Definitions

* Exported functions are encouraged to have a top level comment

* a type signature should be provided and a partial type signature
is fine as long as an explanation is available in the comment.

* Use Haddock syntax in the comments.

It is encouraged to comment every data type, but it's not required
if the field name can speak for itself.

Function example:

```haskell
-- | Send a message on a socket.  The socket must be in a connected
-- state.  Returns the number of bytes sent.  Applications are
-- responsible for ensuring that all data has been sent.
send :: Socket      -- ^ Connected socket
     -> ByteString  -- ^ Data to send
     -> IO Int      -- ^ Bytes sent
```

For functions the documentation should give enough information to
apply the function without looking at the function's definition.

Record example:

```haskell
-- | Bla bla bla.
data Person = Person
    { age  :: !Int     -- ^ Age
    , name :: !String  -- ^ First name
    }
```

For fields that require longer comments format them like so:

```haskell
data Record = Record
    { -- | This is a very very very long comment that is split over
      -- multiple lines.
      field1 :: !Text

      -- | This is a second very very very long comment that is split
      -- over multiple lines.
    , field2 :: !Int
    }
```

Similar levels of commenting should be
applied to long functions declared in `let` or `where` blocks. When in doubt,
overcomment. Whoever needs to read and maintain your code will thank you.

### End-of-Line Comments

It's encouraged to align end-of-line comments.

Align comments for data type definitions.  Some examples:

```haskell
data Parser = Parser
    !Int         -- Current position
    !ByteString  -- Remaining input

foo :: Int -> Int
foo n = salt * 32 + 9
  where
    salt = 453645243  -- Magic hash salt.
```

### Links

Use in-line links economically.  You are encouraged to add links for
API names.  It is not necessary to add links for all API names in a
Haddock comment.  We therefore recommend adding a link to an API name
if:

* The user might actually want to click on it for more information (in
  your judgment), and

* Only for the first occurrence of each API name in the comment (don't
  bother repeating a link)

# Naming

Use camel case (e.g. `functionName`) when naming functions and upper
camel case (e.g. `DataType`) when naming data types.

For readability reasons, don't capitalize all letters when using an
abbreviation.  For example, write `HttpServer` instead of
`HTTPServer`.  Exception: Two letter abbreviations, e.g. `IO`.

### Modules

Use singular when naming modules e.g. use `Data.Map` and
`Data.ByteString.Internal` instead of `Data.Maps` and
`Data.ByteString.Internals`.

## Dealing with laziness

By default, use lazy functions and strict data types. If you deviate from this,
add a comment indicating the reason. If there is no commend indicating the
reason, it is valid to change the definition to match these expectations.

### Data types

Constructor fields should be strict, unless there's an explicit reason
to make them lazy. This avoids many common pitfalls caused by too much
laziness and reduces the number of brain cycles the programmer has to
spend thinking about evaluation order.

```haskell
-- Good
data Point = Point
    { pointX :: !Double  -- ^ X coordinate
    , pointY :: !Double  -- ^ Y coordinate
    }
```

```haskell
-- Bad
data Point = Point
    { pointX :: Double  -- ^ X coordinate
    , pointY :: Double  -- ^ Y coordinate
    }
```

Additionally, unpacking simple fields often improves performance and
reduces memory usage:

```haskell
data Point = Point
    { pointX :: {-# UNPACK #-} !Double  -- ^ X coordinate
    , pointY :: {-# UNPACK #-} !Double  -- ^ Y coordinate
    }
```

As an alternative to the `UNPACK` pragma, you can put

```haskell
{-# OPTIONS_GHC -funbox-strict-fields #-}
```

at the top of the file. Including this flag in the file inself instead
of e.g. in the Cabal file is preferable as the optimization will be
applied even if someone compiles the file using other means (i.e. the
optimization is attached to the source code it belongs to).

Note that `-funbox-strict-fields` applies to all strict fields, not
just small fields (e.g. `Double` or `Int`). If you're using GHC 7.4 or
later you can use `NOUNPACK` to selectively opt-out for the unpacking
enabled by `-funbox-strict-fields`.

### Functions

Have function arguments be lazy unless you explicitly need them to be
strict.

The most common case when you need strict function arguments is in
recursion with an accumulator:

```haskell
mysum :: [Int] -> Int
mysum = go 0
  where
    go !acc []    = acc
    go acc (x:xs) = go (acc + x) xs
```

# Misc

### Point-free style ###

Point-free style are acceptable only if:

* all chaining functions are unary functions

* non-unary functions are still acceptable if
for each of them in the function composition chain,
there is a no less than 2 lines of explanation written in the comment.

* If the 2 requirements above are met, but `hlint` gets in your way,
use annotation to suppress it (minimize the line affected by the annotation).

### Warnings ###

Make sure that `hlint` gives no warnings for your code, using the default hint
files shipped with hlint.

Make sure that there are no serious warnings or deprecation warnings caused by
using old APIs.

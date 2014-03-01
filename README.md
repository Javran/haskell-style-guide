Haskell Style Guide
===================

Every major open-source project has its own style guide: a set of
conventions (sometimes arbitrary) about how to write code for that
project.  It is much easier to understand a large codebase when all
the code in it is in a consistent style.

This project holds the style guidelines I use for my code.  If you are
modifying a project that was created by me, you may be pointed to this style
guide.  I've tried to cover the major areas of formatting and naming.  When
something isn't covered by this guide you should stay consistent with the code
in the other modules. However, it is generally acceptable to fix formatting if
you find places where it needs to be fixed.

Formatting
----------

### Line Length

Maximum line length is **100 characters**.

### Indentation

Tabs are illegal. Use spaces for indenting.  Indent your code blocks
with **2 spaces**.  Indent the definitions in a `where` clause 2 spaces. Some examples:

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

One blank line between top-level definitions.  No blank lines between
type signatures and function definitions.  Add one blank line between
functions in a type class instance declaration if the functions bodies
are large. Use your judgement.

### Whitespace

Surround binary operators with a single space on either side, including all
arithmetic operators. Don't insert a space after a lambda.

### Data Declarations

Align the constructors in a data type definition.  Example:

```haskell
data Tree a = Branch !a !(Tree a) !(Tree a)
            | Leaf
```

For long type names the following formatting is also acceptable:

```haskell
data HttpException
    = InvalidStatusCode Int
    | MissingContentHeader
```

Format records as follows:

```haskell
data Person = Person
    { firstName :: !String  -- ^ First name
    , lastName  :: !String  -- ^ Last name
    , age       :: !Int     -- ^ Age
    } deriving (Eq, Show)
```

### List Declarations

Align the elements in the list.  Example:

```haskell
exceptions =
    [ InvalidStatusCode
    , MissingContentHeader
    , InternalServerError
    ]
```

Optionally, you can skip the first newline.  Use your judgement.

```haskell
directions = [ North
             , East
             , South
             , West
             ]
```

### Pragmas

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

foo :: IO ()
foo = alloca 10 $ \a ->
      alloca 20 $ \b ->
      cFunction a b
```
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

When writing non-monadic code (i.e. when not using `do`) and guards
and pattern matches can't be used, you can align if-then-else clauses
you like you would normal expressions:

```haskell
foo = if ...
      then ...
      else ...
```

When writing monadic code, add the `DoAndIfThenElse` pragma at the top of the
file, and indent in the same manner (with the `then` and `else` aligned to the
`if`).

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

Align the `->` arrows when it helps readability.

Imports
-------

Imports should be grouped in the following order:

1. standard library imports
2. related third party imports
3. local application/library specific imports

Put a blank line between each group of imports.  The imports in each
group should be sorted alphabetically, by module name.

Always use explicit import lists or `qualified` imports for standard
and third party libraries.  This makes the code more robust against
changes in these libraries.  Exception: The Prelude.

When working in a project that uses ClassyPrelude, make sure to include
`NoImplicitPrelude` pragma at the top of the file and import ClassyPrelude
instead of using Prelude.

Comments
--------

### Punctuation

Write proper sentences; start with a capital letter and use proper
punctuation. *This is important.*

### Top-Level Definitions

Comment *every* top level function (not only exported functions),
and provide a type signature; use Haddock syntax in the comments.
Comment every data type, whether or not it is exported.  Function example:

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

Note that this applies to *all* top-level functions and data types, regardless
of whether or not they are exported. Similar levels of commenting should be
applied to long functions declared in `let` or `where` blocks. When in doubt,
overcomment. Whoever needs to read and maintain your code will thank you.

### End-of-Line Comments

Separate end-of-line comments from the code using 2 spaces.  Align
comments for data type definitions.  Some examples:

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

Naming
------

Use camel case (e.g. `functionName`) when naming functions and upper
camel case (e.g. `DataType`) when naming data types.

For readability reasons, don't capitalize all letters when using an
abbreviation.  For example, write `HttpServer` instead of
`HTTPServer`.  Exception: Two letter abbreviations, e.g. `IO`.

### Modules

Use singular when naming modules e.g. use `Data.Map` and
`Data.ByteString.Internal` instead of `Data.Maps` and
`Data.ByteString.Internals`.

Dealing with laziness
---------------------

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

Misc
----

### Point-free style ###

Avoid over-using point-free style. For example, this is hard to read:

```haskell
-- Bad:
f = (g .) . h
```

### Warnings ###

Make sure that `hlint` gives no warnings for your code, using the default hint
files shipped with hlint.

Make sure that there are no serious warnings or deprecation warnings caused by
using old APIs.

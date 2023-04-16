# Fino Syntax
The programming language's current syntax and grammar is informally specified here.

Arbitrary spaces are allowed between lexemes, excluding newlines. Newlines are only allowed where explicitly stated.

Indentation may be required. An indentation is any whitespace 4 characters or more longer than the previous indentation.

## Declarations

### Data Declarations
For now, must all be on a single line.
```
data Maybe a = Nothing | Just a

data Either a b = Left a | Right b

data Nat = Zero | Succ Nat
```

### Type Alias Declarations
For now, must all be on a single line.
```
type Failable a = Either Error a

type Person = {name : str, age : i32}
```

### Fn Declarations
`Fn` declarations are desugared to `let` declarations with lambdas and match expressions in the parser.

Type annotation is optional. If type annotation is given, a newline must be present after the type annotation.

If a newline is present after the `fn <id> [: <type>]`, then all cases must be indented and on a new line.

```
fn main : unit -> unit
    () = println "Hello, World!"

fn factorial
    1 = 1
    n = n * factorial (n - 1)

fn id x = x
```

If defining an operator, a fixity and precedence must be specified. The identifier must be surrounded in parenthesis, and it must be a valid operator identifier.

Possible fixities are: `infixl`, `infixr`, `infix`, `prefix`, and `postfix`.
Operator precedence may be any arbitrary positive integer.

```
fn infixl 6 (+) : i32 -> i32 -> i32
    a b = ...

fn infixl 6 (-) a b = ...

fn infixl 7 (*)
    a b = ...

fn postfix 12 (!)
    1 = 1
    n = n * (n-1)!
```

### Let Declarations
Type annotation is optional.

For now, no newlines allowed before `=`.
```
let x = y

let a: Type = expr
```

### Trait Declarations

### Impl Declarations

## Expressions

### Operators
Operators are desugared to regular function applications in the parser.
```
1 + 2
~x
92!
```

### Variables
```
abc123
Qwe_'4'
```

### Literals
```
123
123.456
true
false
'a'
'\n'
"hello\t\n"
()
```

### Function application
```
print "hello"
add 1 2
map (add 1) listOfIntegers
input ()
```

### Lambdas (anonymous functions)
Lambdas with arity greater than one are desugared to multiple lambdas with arity one in the parser.
```
\x -> y
\n m -> n + m
\a b c -> a (b c)
```

### Annotated Expressions
```
expression : Type
(3 + 4) : f32
```

### Let Bindings
Newlines allowed before and after `in`.
```
let x = y in z

let var = 1 + 2
in (+) 1 var

let a = expr in
body
```

### If Expression
Newlines allowed after `if`.

Newlines allowed before and after `then` and `else`.
```
if c then a else b

if c
then a
else b

if
c
then
a
else
b

if
    c
then
    a
else
    b
```

### Match Expressions
Indentation required after `match <expr>`.

Each branch must be indented and on a new line.
```
match expr
    Just m => m
    12 => a
    true => b
    false => c
    () => d
    x => e
    _ => f
```

### Do Notation
Newline must be present after `do`.

Each statement in the `do` block must be indented and on a new line.
```
fn main : unit -> unit
    () = do
         println "What is your name?"
         let name = input ()
         println ("Hello " + name + "!")
```

The above is equivalent to:

```
fn main : unit -> unit
    () = let _ = println "What is your name?" in
         let name = input () in
         println ("Hello " + name + "!")
```

### Mdo Notation
Similar to `do` notation, a newline must be present after `mdo`.

Each statement in the `mdo` block must be indented and on a new line.
```
fn failableOperation : a -> Either Error (Pair a b)
    a = mdo
        someFunc a
        res <- someOtherFunc a
        pure (Pair a res)
```

## Miscellaneous

### Imports
```
import a
import some::module
import control::monad::state
```

### Exports
All exported identifiers must be on the same line, separated by a `,`.

Exported modules must have their name prefixed by the `module` keyword.
```
import m
import abc::xyz

export someDef, fnAbc, module m, Maybe, module abc::xyz
```

### Extern Declarations
```
extern builtin_add_i32 : i32 -> i32 -> i32
```

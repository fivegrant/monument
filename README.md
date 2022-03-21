# monument


## Usage
To build run:
```
nix-build release.nix
```
To start the interpreter run:
```
./result/bin/monument
```
*more details coming in the future*

## Developing

When `monument.cabal` is changed in any way,
make sure to run:
```
nix-shell --pure -p cabal2nix --run "cabal2nix . --enable-profiling" > monument.nix
```

If any new files are added to `app`, make sure they are in the `Lib` subdirectory and include it in
`other-modules` in `monument.cabal`.

## Thoughts

Initial grammar ideas came from Girafe, however, the resulting grammar is a lot different
with some extensions.

*this is basically a notepad, will delete later*

```
predicate \var1 constant \var2 -> predicate2 named_thing named_thing2 | named_thing
^anything that's not on the left side like a var

0 + 0 -> 0
num(a) + 0 -> num(a)
0 + num(a) -> num(a)
num(a) + num(b) -> a + num(b)


1 -> num(0)
2 -> num(1)
3 -> num(2)
4 -> num(3)
5 -> num(4)
6 -> num(5)
7 -> num(6)
8 -> num(7)
9 -> num(8)
10 -> num(9)
Dec(a) -> num(a)
Dec(1 , 0) -> 10
Dec(0 , a) -> Dec(a)
Dec(0, b, a) -> Dec(b, a)
11 -> Dec(1 , 1)
12 -> Dec(1 , 2)
13 -> Dec(1 , 3)
.
..
...
....
.....
20 -> Dec(2 , 0)

there needs to be two extra bits of notation:

firstly, we need something to allow continues a pattern
to continue `(...)` so Dec(...) but i'm trying to figure out
what else

secondly:
`\(1,2,3,4,5,6,7,8,9) $a$b$c |> Dec($a,$b,$c)` would allow us to make the 
numbers 000 -> 100 expressible
note that `\` would have to update the parser.
also `|>` indicates the left side will be shown to the user but
the rightside will be used for term-rewriting although when returned
to the user, it will then return to the left side form.

```

## Errors
There are two general error types that make sense to
alert the user of: Definition and Query.

*exposition will come later*
- Definition
  - Ambiguity
- Query

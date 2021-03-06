# monument
**FOR JIM'S CODE REVIEW ASSIGNMENT, PLEASE LOOK AT `app/Lib/Parser.hs`**
**FEEL FREE TO COMMENT ON ANYTHING ELSE IN `app` THOUGH**

**NOTICE: CERTAIN TYPES OF REDUCTIONS END UP CRASHING.**
**NO GUARANTEES AT THIS POINT. WILL FIX ASAP**
**PARSING WORKS BUT I HAVENT GOTTEN AROUND TO WRITING A LEXER SO WATCH YOUR WHITESPACES (will also fix soon).**

## Usage
To build using [Nix](https://nixos.org/):
```
nix-build release.nix
```
Alternatively, [you can use Cabal and cabal-install](https://katychuang.com/cabal-guide/):
```
cabal update
cabal install
cabal build
dist/build/monument # might be `dist-newstyle/.../build/monument` instead
```
note: this works, but cabal-install is not actively supported. Therefore,
Nix is recommended.

To start the interpreter:
```
./result/bin/monument [optional-input-file]
```

For the optional input file, I suggest using `tests/sample.mo`.

Once you are in the interpreter, the possible commands are
- `\quit`: quit session
- `\print`: print the current rules of the reduction system
- `[term] -> [term]` adds a new rule to the reduction system
- `[term]` normalizes a term.

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

\macro_$m\, >- M($m)
```

## Errors
There are two general error types that make sense to
alert the user of: Definition and Query.

*exposition will come later*
- Definition
  - Ambiguity
- Query

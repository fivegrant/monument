# monument

## Developing

When `monument.cabal` is changed in any way,
make sure to run
```
nix-shell --pure -p cabal2nix --run "cabal2nix . --enable-profiling" > monument.nix
```

## Thoughts

```
predicate \var1 constant \var2 -> predicate2 named_thing named_thing2 | named_thing
^anything that's not on the left side like a var

0 + 0 -> 0
S(a) + 0 -> S(a)
0 + S(a) -> S(a)
S(a) + S(b) -> a + S(b)


1 -> S(0)
2 -> S(1)
3 -> S(2)
4 -> S(3)
5 -> S(4)
6 -> S(5)
7 -> S(6)
8 -> S(7)
9 -> S(8)
10 -> S(9)
Dec(a) -> S(a)
Dec(1 , 0) -> 10
Dec(0 , 1)
11 -> Dec(1 , 1)
12 -> Dec(1 , 2)
13 -> Dec(1 , 3)
.
..
...
....
.....
20 -> Dec(2 , 0)

```

## Errors

- Definition Errors: Ambiguity

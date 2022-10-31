# monument

## Usage
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

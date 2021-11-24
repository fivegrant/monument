# monument

## Grammar Thoughts
```
types:
  - duck/undecided
  - null
  - bool
  - numbers
    - float
    - int
    - rational?
  - strings
    + string distance
  - regex
    + calculus included
  - function
  - symbol
    + like lisp
  - infinity type

without concern for type
definition := value
value =: definition

enforce by value?
definition := type :: value
type :: value =: definition
enforce by definition?
type :: definition := value
value =: type :: definition
does the enforcement matter?

symbols ,  context filled in with symbols 

conditional => value-if-true ? value-if-false

func :=
(x,y,...),
case1 => value1 ? 
case2 => value2 ? 
case3 => value3 ? 
         value4 

BUILTIN
- +
- - 
- *
- ^
- /
- %
- !
- $ (derivative)
- &
- |

unused: @ #
```

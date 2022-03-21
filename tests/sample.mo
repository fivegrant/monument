~(F) -> T
~(T) -> F
~(~($boolean)) -> $boolean
^(F,F) -> F
^(T,F) -> ^(F,F)
^(F,T) -> ^(T,F)
^(T,T) -> T
v(T,F) -> v(T,T)
v(F,T) -> v(T,T)
v(F,F) -> F
v(T,T) -> T
-(0) -> 0
+(0,$x) -> $x
+($x,0) -> +(0, $x)
+(num($x),$y) -> +($x,num($y))

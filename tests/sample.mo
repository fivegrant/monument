~(F) -> T
~(T) -> F
~(~($p)) -> $p
^(F,F) -> F
^(T,F) -> ^(F,F)
^(F,T) -> ^(T,F)
^(T,T) -> T
v(T,T) -> T
v(T,F) -> v(T,T)
v(F,T) -> v(T,T)
v(F,F) -> F

-(0) -> 0
+(0,$x) -> $x
+($x,0) -> +(0, $x)
+(num($x),$y) -> +($x,num($y))

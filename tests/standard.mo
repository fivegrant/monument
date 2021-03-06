  Basic Logic
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

  Basic Arithmetic
-(0) -> 0
-(-($x)) -> $x
+(0,$x) -> $x
+($x,0) -> +(0,$x)
+(-($x),-($y)) -> -(+($x,$y))
+($x,-($y)) -> +(-($y),$x)
+(-(num($x)),num($y)) -> +(-($x),$y)
+(num($x),$y) -> +($x,num($y))
*(0,$x) -> 0
*($x,0) -> *(0,$x)
*(-($x),-($y)) -> *($x,$y)
*(-($x),$y) -> -(*($x,$y))
*($x,-($y)) -> *(-($y),$x)
*(num(0),$x) -> $x
*($x,num(0)) -> *(num(0),$x)
*(num($x),$y) -> +($y,*($x,$y))

  This is the current way to keep track of numbers.
  Implementing macros should solve this somewhat
c($x) -> convert($x)
convert(0) -> 0
convert(num(0)) -> 1
convert(num(num(0))) -> 2
convert(num(num(num(0)))) -> 3
convert(num(num(num(num(0))))) -> 4
convert(num(num(num(num(num(0)))))) -> 5
convert(num(num(num(num(num(num(0))))))) -> 6
convert(num(num(num(num(num(num(num(0)))))))) -> 7
convert(num(num(num(num(num(num(num(num(0))))))))) -> 8
convert(num(num(num(num(num(num(num(num(num(0)))))))))) -> 9
convert(num(num(num(num(num(num(num(num(num(num(0))))))))))) -> 10
convert(0) -> 0
convert(1) -> num(convert(0))
convert(2) -> num(convert(1))
convert(3) -> num(convert(2))
convert(4) -> num(convert(3))
convert(5) -> num(convert(4))
convert(6) -> num(convert(5))
convert(7) -> num(convert(6))
convert(8) -> num(convert(7))
convert(9) -> num(convert(8))
convert(10) -> num(convert(9))

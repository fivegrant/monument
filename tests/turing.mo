  THESE RULES DO NOT WORK YET, DO NOT RUN
  TODO: FIX THIS UP

  
  Transition Functions
tran($from,$to,$char)
  
  Finite State Automata
dfa($states,$start,$accept)

  Turing Machine
tape($left, head, $right)
<(tape(cons($new, $left), $old, $right)) -> <(tape($left, $new, cons($old, $right))
<(tape(end, $old, $right)) -> <(tape($left, $new, cons($old, $right))
<(tape($last, $old, $right)) -> <(tape(end, $last, cons($old, $right))
>(tape($left, $old, cons($new, $right))) -> >(tape(cons($old, $left), $new, $right))

turing($dfa, $tape)

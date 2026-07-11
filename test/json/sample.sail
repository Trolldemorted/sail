default Order dec
$include <prelude.sail>

val plus_one : bits(8) -> bits(8)
function plus_one x = x + 0b00000001

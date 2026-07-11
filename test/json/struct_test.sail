default Order dec
$include <prelude.sail>

// Test struct
struct Foo = {
  x : bits(8),
  y : bits(16),
}

// Test enum
enum Bar = {
  A,
  B,
  C,
}

// Test variant (union)
union Baz = {
  Q : bits(8),
  R : unit,
  S : bits(64),
}

// Test type alias
type MyBits = bits(32)

// Test function with multiple clauses
function classify n : bits(8) -> bits(3) =
  if n == 0b00000000 then 0b000
  else if n == 0b11111111 then 0b111
  else 0b010

// Test function with match
function unwrap b : Baz -> bits(64) =
  match b {
    Q(_) => 0xFFFFFFFFFFFFFFFE,
    R(_) => 0xFFFFFFFFFFFFFFFF,
    S(_) => 0xFFFFFFFFFFFFFFFD,
  }

// Test function with let
function add_three x : bits(8) -> bits(8) =
  let a = x + 0b00000001 in
  let b = a + 0b00000010 in
  b + 0b00000011
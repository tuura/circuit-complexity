# Sum

This function outputs the binary representation of the sum of _N_ input bits.

For example, given three input bits `x1`, `x2` and `x3`, the following circuit
computes their sum in the output bits `x5` (least significant bit) and `x8`
(most significant bit).

```
x4 = x2 ⊕ x3
x5 = x1 ⊕ x4
x6 = x2 ∧ x3
x7 = x1 ∧ x4
x8 = x6 ∨ x7
```

Current ABC synthesis results are produced using command `fraig; refactor`.

For example, here is the script used for producing `sum5.out`:

```
read_eqn sum5.in
read_library simple.genlib
fraig; refactor
map
write_eqn sum5.out
```

a Constant 45 0 0
b Constant 27 0 0
x Variable 0 0 3
y Variable 0 0 4
g Variable 0 0 5
m Variable 0 0 6
swap Procedure 0 0 0
mod Procedure 0 0 8
temp Variable 0 1 3

<swap.>
  0:  INT 0 4
  1:  LOD 1 3
  2:  STO 0 3
  3:  LOD 1 4
  4:  STO 1 3
  5:  LOD 0 3
  6:  STO 1 4
  7:  OPR 0 0
<mod.>
  8:  INT 0 3
  9:  LOD 1 3
  10:  LOD 1 3
  11:  LOD 1 4
  12:  OPR 0 5
  13:  LOD 1 4
  14:  OPR 0 4
  15:  OPR 0 3
  16:  STO 1 3
  17:  OPR 0 0
<main.>
  18:  INT 0 7
  19:  LIT 0 45
  20:  STO 0 3
  21:  LIT 0 27
  22:  STO 0 4
  23:  CAL <mod.> 0 8
  24:  LOD 0 3
  25:  LIT 0 0
  26:  OPR 0 9
  27:  JPC 0 31
  28:  CAL <swap.> 0 0
  29:  CAL <mod.> 0 8
  30:  JMP 0 24
  31:  LOD 0 4
  32:  STO 0 5
  33:  LIT 0 45
  34:  LIT 0 27
  35:  OPR 0 4
  36:  LOD 0 5
  37:  OPR 0 5
  38:  STO 0 6
  39:  LOD 0 5
  40:  WRT 0 0
  41:  LOD 0 6
  42:  WRT 0 0
  43:  OPR 0 0

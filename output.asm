true Constant 1 0 0
false Constant 0 0 0
x Variable 0 0 3
y Variable 0 0 4
m Variable 0 0 5
n Variable 0 0 6
pf Variable 0 0 7
prime Procedure 0 0 0
i Variable 0 1 3
f Variable 0 1 4
mod Procedure 0 0 0

<prime.>
<mod.>
  0:   INT 0 3
  1:   LOD 2 3
  2:   LOD 2 3
  3:   LOD 2 4
  4:   OPR 0 5
  5:   LOD 2 4
  6:   OPR 0 4
  7:   OPR 0 3
  8:   STO 2 3
  9:   OPR 0 0
  10:  INT 0 5
  11:  LIT 0 1
  12:  STO 0 4
  13:  LIT 0 3
  14:  STO 0 3
  15:  LOD 0 3
  16:  LOD 1 5
  17:  OPR 0 10
  18:  JPC 0 35
  19:  LOD 1 5
  20:  STO 1 3
  21:  LOD 0 3
  22:  STO 1 4
  23:  CAL <mod.> 0
  24:  LOD 1 3
  25:  LIT 0 0
  26:  OPR 0 8
  27:  JPC 0 30
  28:  LIT 0 0
  29:  STO 0 4
  30:  LOD 0 3
  31:  LIT 0 2
  32:  OPR 0 2
  33:  STO 0 3
  34:  JMP 0 15
  35:  LOD 0 4
  36:  LIT 0 1
  37:  OPR 0 8
  38:  JPC 0 43
  39:  LOD 1 5
  40:  WRT 0 0
  41:  LIT 0 1
  42:  STO 1 7
  43:  OPR 0 0
<main.>
  44:  INT 0 8
  45:  LIT 0 0
  46:  STO 0 7
  47:  RED 0 6
  48:  LOD 0 6
  49:  LIT 0 2
  50:  OPR 0 11
  51:  JPC 0 74
  52:  LIT 0 2
  53:  WRT 0 0
  54:  LOD 0 6
  55:  LIT 0 2
  56:  OPR 0 8
  57:  JPC 0 60
  58:  LIT 0 1
  59:  STO 0 7
  60:  LIT 0 3
  61:  STO 0 5
  62:  LOD 0 5
  63:  LOD 0 6
  64:  OPR 0 13
  65:  JPC 0 72
  66:  CAL <prime.> 0
  67:  LOD 0 5
  68:  LIT 0 2
  69:  OPR 0 2
  70:  STO 0 5
  71:  JMP 0 62
  72:  RED 0 6
  73:  JMP 0 48
  74:  LOD 0 7
  75:  LIT 0 0
  76:  OPR 0 8
  77:  JPC 0 80
  78:  LIT 0 0
  79:  WRT 0 0
  80:  OPR 0 0

nb : [0-9]+(\.[0-9]+)?

E -> T E_n
E_n -> '\+' T E_n
E_n -> '-' T E_n
E_n -> epsilon
T -> F T_n
T_n -> '\*' F T_n
T_n -> '/' F T_n
T_n -> epsilon
F -> '\(' E '\)'
F -> nb

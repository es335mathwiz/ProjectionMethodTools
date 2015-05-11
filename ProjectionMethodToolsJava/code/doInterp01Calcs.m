Needs["symb01`"]
Needs["symb01ValsRec`"]
Print["computing interpolation errors"]
experFile="doInterp01Calcs.m";
experTime=Timing[interOneVals=experOrd[]][[1]]
Splice["interpOneCalcs.mtex"]


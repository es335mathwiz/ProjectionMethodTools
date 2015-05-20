boop= FunctionInterpolation[
  Sow[Function[{qq$, ru$, eps$}, Function[{qtm1$, rutm1$, eps$}, 
       With[{initVals$ = primeFunc[qtm1$, rutm1$, eps$], 
         fixFunc$ = With[{fixVal = qrAccTry02ValsRecVals[#1[[1]], #1[[2]]][
               qtm1$, rutm1$, eps$]}, Sow[fixVal, "fixVal"]; fixVal] & }, 
        Sow[initVals$, "initVals="]; Sow[{qtm1$, rutm1$, eps$}, "for state="]; 
         With[{theVal = FixedPoint[fixFunc$, {initVals$[[1]], 
              initVals$[[2]]}]}, Sow[theVal, "theVal"]; theVal[[-1]]]]][qq$, 
      ru$, eps$]][interpqq, interpru, interpep]], {interpqq, -0.5, 0.5}, 
  {interpru, -0.5, 0.5}, {interpep, -0.02, 0.02}, InterpolationOrder -> 1, 
  InterpolationPoints -> 2]


boop01= FunctionInterpolation[
  Sow[Function[{qtm1$, rutm1$, eps$}, 
       With[{initVals$ = primeFunc[qtm1$, rutm1$, eps$], 
         fixFunc$ = With[{fixVal = qrAccTry02ValsRecVals[#1[[1]], #1[[2]]][
               qtm1$, rutm1$, eps$]}, Sow[fixVal, "fixVal"]; fixVal] & }, 
        Sow[initVals$, "initVals="]; Sow[{qtm1$, rutm1$, eps$}, "for state="]; 
         With[{theVal = FixedPoint[fixFunc$, {initVals$[[1]], 
              initVals$[[2]]}]}, Sow[theVal, "theVal"]; theVal[[-1]]]]]][interpqq, interpru, interpep], {interpqq, -0.5, 0.5}, 
  {interpru, -0.5, 0.5}, {interpep, -0.02, 0.02}, InterpolationOrder -> 1, 
  InterpolationPoints -> 2]

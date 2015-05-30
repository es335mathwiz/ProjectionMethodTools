(*
%<*Run["cat /proc/cpuinfo | grep processor | wc -l>numProcs"];numProcs=Get["numProcs"]*>
%<*Run["uptime | tr -s ' ' ' ' | cut -d' ' -f11->loadAvg"];loadAvg=Import["loadAvg"]*>
%<*Run["vmstat | tail -n 1 | tr -s ' ' ' ' | cut -d' ' -f5>freeMem"];freeMem=Get["freeMem"]*>
%<*experFile=symb02ValsRec.m*>
<*symb02ValsRecFirstSecs*>\endnote{Interpolation Time=<*interpTime02ValsRec*>(order=<*Global`theOrd*>,points=<*Global`thePts*>)}&<*symb02ValsRecSecondSecs*>\endnote{<*DateString[]*> Running <*experFile*>: on <*{$System,$ProcessorType}*>,
num procs=<*numProcs*>, loadAvg=<*loadAvg*>, freeMem=<*freeMem*>.}
*)
BeginPackage["timePFRERecur`",{"occBindRecur`"}]



theMaxNesting=25;
theMaxOrder=4;
theMaxPts=20;

preComp01=doChkLoad[]
compsPFNull=Table[forIOrdNPtsPF[ii,jj,{},theMaxNesting],{ii,0,theMaxOrder},{jj,ii,theMaxPts}];
(*
preComp02=doChkLoad[]
compsRENull=Table[forIOrdNPtsRE[ii,jj,{},theMaxNesting],{ii,0,theMaxOrder},{jj,ii,theMaxPts}];
preComp03=doChkLoad[]
compsPFExact=Table[forIOrdNPtsPF[ii,jj,z01ExactInitPF,theMaxNesting],{ii,0,theMaxOrder},{jj,ii,theMaxPts}];
preComp04=doChkLoad[]
compsREExact=Table[forIOrdNPtsRE[ii,jj,z01ExactInitRE,theMaxNesting],{ii,0,theMaxOrder},{jj,ii,theMaxPts}];
preComp05=doChkLoad[]
*)
Save["timePFRERecur"<>ToString[AbsoluteTime[]//Floor//InputForm]<>".m","timePFRERecur`"]

EndPackage[]

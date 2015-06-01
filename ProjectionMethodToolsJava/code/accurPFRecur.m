(*
%<*Run["cat /proc/cpuinfo | grep processor | wc -l>numProcs"];numProcs=Get["numProcs"]*>
%<*Run["uptime | tr -s ' ' ' ' | cut -d' ' -f11->loadAvg"];loadAvg=Import["loadAvg"]*>
%<*Run["vmstat | tail -n 1 | tr -s ' ' ' ' | cut -d' ' -f5>freeMem"];freeMem=Get["freeMem"]*>
%<*experFile=symb02ValsRec.m*>
<*symb02ValsRecFirstSecs*>\endnote{Interpolation Time=<*interpTime02ValsRec*>(order=<*Global`theOrd*>,points=<*Global`thePts*>)}&<*symb02ValsRecSecondSecs*>\endnote{<*DateString[]*> Running <*experFile*>: on <*{$System,$ProcessorType}*>,
num procs=<*numProcs*>, loadAvg=<*loadAvg*>, freeMem=<*freeMem*>.}
*)
BeginPackage["accurPFRecur`",{"ProtectedSymbols`","occBindRecur`"}]




theMaxOrder=2;
theMaxPts=6;

preComp01=doChkLoad[]
compsPFNull=Table[assessPF[ii,jj,{},2],{ii,0,theMaxOrder},{jj,ii,theMaxPts}];
Save["accurPFRecur"<>ToString[AbsoluteTime[]//Floor//InputForm]<>".m","accurPFRecur`"]

EndPackage[]

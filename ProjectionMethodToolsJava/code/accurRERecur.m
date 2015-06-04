(*
%<*Run["cat /proc/cpuinfo | grep processor | wc -l>numProcs"];numProcs=Get["numProcs"]*>
%<*Run["uptime | tr -s ' ' ' ' | cut -d' ' -f11->loadAvg"];loadAvg=Import["loadAvg"]*>
%<*Run["vmstat | tail -n 1 | tr -s ' ' ' ' | cut -d' ' -f5>freeMem"];freeMem=Get["freeMem"]*>
%<*experFile=symb02ValsRec.m*>
<*symb02ValsRecFirstSecs*>\endnote{Interpolation Time=<*interpTime02ValsRec*>(order=<*theOrd*>,points=<*thePts*>)}&<*symb02ValsRecSecondSecs*>\endnote{<*DateString[]*> Running <*experFile*>: on <*{$System,$ProcessorType}*>,
num procs=<*numProcs*>, loadAvg=<*loadAvg*>, freeMem=<*freeMem*>.}
*)
BeginPackage["accurRERecur`",{"occBindRecur`","ProtectedSymbols`"}]



r
theREMaxOrder=2;
theREMinOrder=0;
theREMinPts=4;
theREMaxPts=10;
theREPeriods=2;
preCompRE=doChkLoad[];
compsRENull=Table[assessRE[genFinalRE[ii,jj,{},theREPeriods]],{ii,theREMinOrder,theREMaxOrder},{jj,ii+1,theREMaxPts}];
postCompRE=doChkLoad[];
Save["accurRERecur"<>ToString[AbsoluteTime[]//Floor//InputForm]<>".m","accurRERecur`"]

EndPackage[]



SendMail["From"->"gary.anderson@frb.gov","To"->"asymptoticallystable@gmail.com","Server"->"mail.rsma.frb.gov", "Body"->"accurRERecur.m done"]

(*
%<*Run["cat /proc/cpuinfo | grep processor | wc -l>numProcs"];numProcs=Get["numProcs"]*>
%<*Run["uptime | tr -s ' ' ' ' | cut -d' ' -f11->loadAvg"];loadAvg=Import["loadAvg"]*>
%<*Run["vmstat | tail -n 1 | tr -s ' ' ' ' | cut -d' ' -f5>freeMem"];freeMem=Get["freeMem"]*>
%<*experFile=symb02ValsRec.m*>
<*symb02ValsRecFirstSecs*>\endnote{Interpolation Time=<*interpTime02ValsRec*>(order=<*theOrd*>,points=<*thePts*>)}&<*symb02ValsRecSecondSecs*>\endnote{<*DateString[]*> Running <*experFile*>: on <*{$System,$ProcessorType}*>,
num procs=<*numProcs*>, loadAvg=<*loadAvg*>, freeMem=<*freeMem*>.}
*)
BeginPackage["accurPFRecur`",{"occBindRecur`"}]




thePFMaxOrder=2;
thePFMinOrder=0;
thePFMinPts=3;
thePFMaxPts=10;
thePFPeriods=10;

preCompPF=doChkLoad[];
compsPFNull=Table[assessPF[genFinalPF[ii,jj,{},thePFPeriods]],{ii,thePFMinOrder,thePFMaxOrder},{jj,ii+1,thePFMaxPts}];
postCompPF=doChkLoad[];
Save["accurPFRecur"<>ToString[AbsoluteTime[]//Floor//InputForm]<>".m","accurPFRecur`"]


EndPackage[]
SendMail["From"->"gary.anderson@frb.gov","To"->"asymptoticallystable@gmail.com","Server"->"mail.rsma.frb.gov", "Body"->"accurPFRecur.m done"]
(*
 using port 25
*)

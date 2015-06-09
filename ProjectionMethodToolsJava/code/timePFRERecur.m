BeginPackage["timePFRERecur`",{"occBindRecur`"}]

(*
%<*Run["cat /proc/cpuinfo | grep processor | wc -l>numProcs"];numProcs=Get["numProcs"]*>
%<*Run["uptime | tr -s ' ' ' ' | cut -d' ' -f11->loadAvg"];loadAvg=Import["loadAvg"]*>
%<*Run["vmstat | tail -n 1 | tr -s ' ' ' ' | cut -d' ' -f5>freeMem"];freeMem=Get["freeMem"]*>
*)
(*
theMinNesting=0;
theMaxNesting=3;
theMinOrder=2;
theMaxOrder=3;
theMaxPts=4;
indices=Flatten[Table[{ii+(1-theMinOrder),jj+(1-Max[1,ii]),kk+(1-theMinNesting)},{ii,theMinOrder,theMaxOrder},{jj,Max[1,ii],theMaxPts},{kk,theMinNesting,theMaxNesting}],2];







preComp01=doChkLoad[]
compsPFNull=Table[Timing[{{ii,jj,kk},genFinalPF[ii,jj,{},kk]}],{ii,theMinOrder,theMaxOrder},{jj,Max[1,ii],theMaxPts},{kk,theMinNesting,theMaxNesting}];
preComp02=doChkLoad[]



compsRENull=Table[Timing[{{ii,jj,kk},genFinalRE[ii,jj,{},kk]}],{ii,theMinOrder,theMaxOrder},{jj,Max[1,ii],theMaxPts},{kk,theMinNesting,theMaxNesting}];
preComp03=doChkLoad[]
compsPFExact=Table[Timing[{{ii,jj,kk},genFinalPF[ii,jj,z01ExactInitPF,kk]}],{ii,theMinOrder,theMaxOrder},{jj,Max[1,Max[1,ii]],theMaxPts},{kk,theMinNesting,theMaxNesting}];
preComp04=doChkLoad[]
compsREExact=Table[Timing[{{ii,jj,kk},genFinalRE[ii,jj,z01ExactInitRE,kk]}],{ii,theMinOrder,theMaxOrder},{jj,Max[1,Max[1,ii]],theMaxPts},{kk,theMinNesting,theMaxNesting}];
preComp05=doChkLoad[]


Save["timePFRERecur"<>(timeStamp=ToString[AbsoluteTime[]//Floor//InputForm])<>".m","timePFRERecur`"]

*)

doCSVExport[theVals_List,csvName_String]:=
Export[csvName,Flatten[{#[[2,1]],#[[1]]}]&/@ Flatten[theVals,2]]

initName[{}]="Null"
doSplice[genFunc_,initFuncs_,
theMinOrder_Integer,theMaxOrder_Integer,
theMinPts_Integer,theMaxPts_Integer,
theMinNesting_Integer,theMaxNesting_Integer
]:=
Module[{timeStamp=ToString[AbsoluteTime[]//Floor//InputForm],
preComp=doChkLoad[],
theComps=Table[Timing[{{ii,jj,kk},genFunc[ii,jj,initFuncs,kk]}],
{ii,theMinOrder,theMaxOrder},
{jj,Max[1,ii],theMaxPts},
{kk,theMinNesting,theMaxNesting}],
runName=ToString[genFunc]<>"X"<>initName[initFuncs],
postComp},
experFile="timePFRERecur.m "<>timeStamp<> " on "<> $System <>$ProcessorType;
postComp=doChkLoad[];
csvName="time"<>runName<>timeStamp<>".csv";
doCSVExport[theComps,csvName];
SendMail["From"->"gary.anderson@frb.gov","To"->"asymptoticallystable@gmail.com","Server"->"mail.rsma.frb.gov", "Body"->"doSplice has finished"];
{preComp,postComp,theComps,csvName,experFile}]

doParallelSplice[genFunc_,initFuncs_,
theMinOrder_Integer,theMaxOrder_Integer,
theMinPts_Integer,theMaxPts_Integer,
theMinNesting_Integer,theMaxNesting_Integer
]:=
Module[{timeStamp=ToString[AbsoluteTime[]//Floor//InputForm],
preComp=doChkLoad[],
theComps=ParallelTable[Timing[{{ii,jj,kk},genFunc[ii,jj,initFuncs,kk]}],
{ii,theMinOrder,theMaxOrder},
{jj,Max[1,ii],theMaxPts},
{kk,theMinNesting,theMaxNesting}],
runName=ToString[genFunc]<>"X"<>initName[initFuncs],
postComp},
postComp=doChkLoad[];
csvName="time"<>runName<>timeStamp<>".csv";
doCSVExport[theComps,csvName];
SendMail["From"->"gary.anderson@frb.gov","To"->"asymptoticallystable@gmail.com","Server"->"mail.rsma.frb.gov", "Body"->"doSplice has finished"];
{preComp,postComp,theComps,csvName,experFile}]
(*
{preComp,postComp,theComps,csvName,experFile}=doSplice[genFinalPF,{},
0,2,1,2,0,2];
tabHead="Perfect Forsight: Null Initial Functions";
Splice["timeRecurTable.mtex","timePFNullRecurNow.tex",FormatType->OutputForm]
*)

(*
preComp=preComp01;postCom=preComp02;
csvName="timePFNullRecur"<>timeStamp<>".csv";
tabHead="Perfect Forsight: Null Initial Functions"
doCSVExport[compsPFNull,csvName]
Splice["timeRecurTable.mtex","timePFNullRecurNow.tex",FormatType->OutputForm]

preComp=preComp02;postCom=preComp03;
csvName="timePFExactRecur"<>timeStamp<>".csv";
tabHead="Perfect Forsight: Exact Initial Functions"
doCSVExport[compsPFExact,csvName]
Splice["timeRecurTable.mtex","timePFExactRecurNow.tex",FormatType->OutputForm]

preComp=preComp03;postCom=preComp04;
csvName="timeRENullRecur"<>timeStamp<>".csv";
tabHead="Rational Expectations: Null Initial Functions"
doCSVExport[compsRENull,csvName]
Splice["timeRecurTable.mtex","timeRENullRecurNow.tex",FormatType->OutputForm]

preComp=preComp04;postCom=preComp05;
csvName="timeREExactRecur"<>timeStamp<>".csv";
tabHead="Rational Expectations: Exact Initial Functions"
doCSVExport[compsREExact,csvName]
Splice["timeRecurTable.mtex","timeREExactRecurNow.tex",FormatType->OutputForm]


SendMail["From"->"gary.anderson@frb.gov","To"->"asymptoticallystable@gmail.com","Server"->"mail.rsma.frb.gov", "Body"->"tmePFRERecur.m done"]
*)

EndPackage[]

BeginPackage["timePFRERecur`",{"occBindRecur`"}]

(*
%<*Run["cat /proc/cpuinfo | grep processor | wc -l>numProcs"];numProcs=Get["numProcs"]*>
%<*Run["uptime | tr -s ' ' ' ' | cut -d' ' -f11->loadAvg"];loadAvg=Import["loadAvg"]*>
%<*Run["vmstat | tail -n 1 | tr -s ' ' ' ' | cut -d' ' -f5>freeMem"];freeMem=Get["freeMem"]*>
*)
theMinNesting=1;
theMaxNesting=2;
theMinOrder=1;
theMaxOrder=2;
theMaxPts=2;
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
doCSVExport[theVals_List,fName_String,tStamp_String]:=
Export[fName<>tStamp<>".csv",Flatten[{#[[2,1]],#[[1]]}]&/@ Flatten[theVals,2]]


experFile="timePFRERecur.m "<>timeStamp<> " on "<> $System <>$ProcessorType;
EndPackage[]

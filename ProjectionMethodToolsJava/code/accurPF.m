Needs["occBindRecur`"]

theMaxNesting=2;theMaxOrder=1;theMaxPts=3;
(*
compsPFNull=Table[forIOrdNPtsPF[ii,jj,{},theMaxNesting],{ii,0,theMaxOrder},{jj,ii,theMaxPts}];
*)


huh01$01=forIOrdNPtsPF[0,1,{},1]
fpForInitStateFunc[-.4,0,0,huh01$01[[-1,-1]]]

huh10$01=forIOrdNPtsPF[0,10,{},1]
fpForInitStateFunc[-.4,0,0,huh10$01[[-1,-1]]]
huh20$01=forIOrdNPtsPF[0,20,{},1]
fpForInitStateFunc[-.4,0,0,huh20$01[[-1,-1]]]


huh01$02=forIOrdNPtsPF[0,1,{},2]
fpForInitStateFunc[-.4,0,0,huh01$02[[-1,-1]]]

huh10$02=forIOrdNPtsPF[0,10,{},2]
fpForInitStateFunc[-.4,0,0,huh10$02[[-1,-1]]]

huh01$01E=forIOrdNPtsPF[0,1,z01ExactInitPF,1]
fpForInitStateFunc[-.4,0,0,huh01$01E[[-1,-1]]]

huh10$01E=forIOrdNPtsPF[0,10,z01ExactInitPF,1]
fpForInitStateFunc[-.4,0,0,huh10$01E[[-1,-1]]]


huh10$01E2=forIOrdNPtsPF[2,10,z01ExactInitPF,1]
fpForInitStateFunc[-.4,0,0,huh10$01E2[[-1,-1]]]

huh10$02=forIOrdNPtsPF[2,20,{},2]
fpForInitStateFunc[-.4,0,0,huh10$02[[-1,-1]]]



huh10$02=forIOrdNPtsPF[1,10,{},2]
fpForInitStateFunc[-.4,0,0,huh10$02[[-1,-1]]]

trynow=Function[{qq,ru,eps}, Max[Abs[
hmatApp[qq,ru,eps,huh[[-1,-1]]]]]]

infNorm[trynow]


hey=Function[
{ww,xx,yy,zz},
With[{huhxx=forIOrdNPtsPF[ww,xx,{},1]},
Transpose[{Through[huhxx[[-1,-1]][yy,zz]],Through[z01ExactInitPF[yy,zz]]}]]]


xx=1;yy=5;
huh10$01E2n=forIOrdNPtsPF[xx,yy,z01ExactInitPF,2]
aPath[-.4,-.020,-.01,huh10$01E2n[[-1,-1]]]
hmatApp[-.4,-.020,-.01,huh10$01E2n[[-1,-1]]]


xx=1;yy=5;
huh10$01E2n=forIOrdNPtsPF[xx,yy,{},2]
aPath[-.4,-.020,-.01,huh10$01E2n[[-1,-1]]]
hmatApp[-.4,-.020,-.01,huh10$01E2n[[-1,-1]]]
getNextPt[-.4,-.020,-.01,huh10$01E2n[[-1,-1]]]
simPFPath[2,-.4,-.020,-.01,huh10$01E2n]
fpForInitStateFunc[-.4,0,0,huh10$01E2[[-1,-1]]]




xx=1;yy=15;
huh10$01E2n=forIOrdNPtsPF[xx,yy,{},3]
aPath[-.4,-.020,-.01,huh10$01E2n[[-1,-1]]]
hmatApp[-.4,-.020,-.01,huh10$01E2n[[-1,-1]]]
getNextPt[-.4,-.020,-.01,huh10$01E2n[[-1,-1]]]


simPFPath[2,-.4,-.020,-.01,huh10$01E2n]



xx=1;yy=10;
huh10$01E2n=forIOrdNPtsPF[xx,yy,z01ExactInitPF,3]
aPath[-.4,-.020,-.01,huh10$01E2n[[-1,-1]]]
hmatApp[-.4,-.020,-.01,huh10$01E2n[[-1,-1]]]
getNextPt[-.4,-.020,-.01,huh10$01E2n[[-1,-1]]]
simPFPath[-.4,-.020,-.01,huh10$01E2n]

(*
aPath[-.4,-.020,-.01,huh10$01E2n[[-1,-1]],0]
]

Out[25]= {{-0.4}, {rtm1}, {-0.02}, {-0.489917}, {0.02}, {-0.02}, {-0.504884}, 
 
>    {0.02}, {-0.01}, {-0.464495}, {0.02}, {-0.005}, {-0.377885}, {0.02}, 
 
>    {-0.0025}, {-0.248762}, {0.02}, {-0.00125}, {-0.0779192}, {-0.0629192}, 
 
>    {-0.000625}}
*)




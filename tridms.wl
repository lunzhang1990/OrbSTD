(* ::Package:: *)

tridms[]:=Module[{dir,reducedineqs,reducedpolys,pos,vars,firstindex,secondindex,tempindex,fullindex,kk,statement,j,result,qef,temp,beta,mu},
dir = "/home/lz210/tridms";
SetDirectory[dir];
reducedpolys={{beta-e[2],beta-e[2]+f e[2]},{1+beta-e[2],1+beta-e[2]+f e[2]},{beta-e[2]+f e[2],beta+f e[1]-e[2]},{1+beta-e[2]+f e[2],1+beta+f e[1]-e[2]},{1+beta-e[1],1+beta-e[1]+f e[2]},{2+2 beta+mu+beta mu-e[1]-mu e[1],2+2 beta+mu+beta mu-e[1]-mu e[1]+f e[2]+mu f e[2]},{1+beta-e[1]+f e[2],1+beta-e[1]+f e[1]},{2+2 beta+mu+beta mu-e[1]-mu e[1]+f e[2]+mu f e[2],2+2 beta+mu+beta mu-e[1]+f e[1]-mu e[1]+mu f e[1]},{beta mu+f e[1]+mu f e[1]-e[2]-mu e[2],1},{mu+mu beta+f e[1]+mu f e[1]-e[2]-mu e[2],1},{mu+mu beta-e[1]+f e[1]-mu e[1]+mu f e[1],1},{1+beta-e[1]+f e[1],1}};
reducedineqs =Table[{{reducedpolys[[i]][[1]]<0,reducedpolys[[i]][[2]]<0},{reducedpolys[[i]][[1]]<0,reducedpolys[[i]][[2]]>0},{reducedpolys[[i]][[1]]>0,reducedpolys[[i]][[2]]>0}},{i,1,12}];
pos = (beta>0)\[And](mu>0)\[And](0<f<1)\[And](e[1]>e[2]>0);
vars= {mu,beta,e[1],e[2],f};

firstindex=Tuples[{1,2,3},8];
secondindex = Tuples[{2,3},4];
tempindex=Tuples[{firstindex,secondindex}];
fullindex = Table[%[tempindex[[k]][[1]],tempindex[[k]][[2]]],{k,1,Length[tempindex]}];


For[kk=1,kk<=20,
ii=fullindex[[kk]];statement={};
For[j=1,j<=Length[reducedineqs],statement=Join[statement,reducedineqs[[j]][[ii[[j]]]]];j++];
qef = And@@statement\[And]pos;
result=TimeConstrained[FindInstance[qef,vars],2];
If[result==={},None,If[Length[result]>0,temp={kk,ii,result[[1]]},temp={kk,ii,result}];
file =OpenAppend["server_output.txt"];Write[file,temp];Close[file]];
Print[kk];
kk++;]
]

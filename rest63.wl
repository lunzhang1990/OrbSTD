(* ::Package:: *)

restofrest[]:=Module[{dir,ret,ineqs,instance,polys,pos,expr,file,newvars,firstindex,secondindex,fullindex,allindex,statement},
dir = "/Users/lunzhang/Desktop/OrbSTD";
SetDirectory[dir];
ret= Import["rest63.json","JSON"];
polys={{beta-e[2],beta-e[2]+f[2]},{1+beta-e[2],1+beta-e[2]+f[2]},{beta-e[2]+f[2],beta+f[1]-e[2]},{1+beta-e[2]+f[2],1+beta+f[1]-e[2]},{1+beta-e[1],1+beta-e[1]+f[2]},{2+2 beta+mu+beta mu-e[1]-mu e[1],2+2 beta+mu+beta mu-e[1]-mu e[1]+f[2]+mu f[2]},{1+beta-e[1]+f[2],1+beta-e[1]+f[1]},{2+2 beta+mu+beta mu-e[1]-mu e[1]+f[2]+mu f[2],2+2 beta+mu+beta mu-e[1]+f[1]-mu e[1]+mu f[1]},{beta mu+f[1]+mu f[1]-e[2]-mu e[2],1},{mu+mu beta+f[1]+mu f[1]-e[2]-mu e[2],1},{mu+mu beta-e[1]+f[1]-mu e[1]+mu f[1],1},{1+beta-e[1]+f[1],1}};
ineqs =Table[{{polys[[i]][[1]]<0,polys[[i]][[2]]<0},{polys[[i]][[1]]<0,polys[[i]][[2]]>0},{polys[[i]][[1]]>0,polys[[i]][[2]]>0}},{i,1,12}];
pos = (beta>0)\[And](mu>0)\[And](0<e[2] < e[1])\[And]f[1]*e[2] == f[2]*e[1]\[And](0<f[2] < f[1]);
newvars = {e[1],e[2],f[1],f[2],beta,mu};
firstindex=Table[{1,2,3},{i,1,8}];
secondindex = Table[{2,3},{i,1,4}];
allindex =Join[firstindex,secondindex];
fullindex = Tuples[allindex];
Print[Length[fullindex]];

For[i=1,i<=Length[ret],i++,
ii=fullindex[[ret[[i]]]];statement={};
For[j=1,j<=Length[ineqs],j++,statement=Join[statement,ineqs[[j]][[ii[[j]]]]];];
expr = Simplify[And@@statement\[And]pos];
instance=TimeConstrained[FindInstance[expr,Evaluate[newvars]],1000];
Print[ret[[i]],instance];
file =OpenAppend["rest63_result.txt"];Write[file,instance];Close[file];]; 
(*For[i=1,i<=Length[rest],i++,
ii=fullindex[[rest[[i]]]];statement={};
For[j=1,j<=Length[reducedineqs],j++,statement=Join[statement,reducedineqs[[j]][[ii[[j]]]]];];
qef= And@@statement\[And]pos;
output = Timing[CylindricalDecomposition[Exists[Evaluate[vars],qef],{}]];
file =OpenAppend["rest_output.txt"];Write[file,output];Close[file];];*)
]


restofrest[]




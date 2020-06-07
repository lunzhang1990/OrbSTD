(* ::Package:: *)

restofrest[]:=Module[{dir,rest,reducedineqs,output,reducedpolys,pos,qef,file,vars,firstindex,secondindex,fullindex,allindex,statement},
dir = "/home/lz210/tridms";
SetDirectory[dir];
rest={5,13,19,21,23,29,31,50,54,62,66,68,70,72,78,80,179,181,183,189,191,226,228,230,232,238,240,323,325,327,333,335,370,372,374,376,382,384,530,534,542,546,548,550,552,558,560,706,708,710,712,718,720,850,852,854,856,862,864,962,966,974,978,980,982,984,990,992,1138,1140,1142,1144,1150,1152,1286,1288,1294,1296,1309,1315,1319,1325,1327,1346,1350,1358,1362,1364,1366,1368,1374,1376,1475,1477,1479,1485,1487,1522,1524,1526,1528,1534,1536,1619,1621,1623,1629,1631,1666,1668,1670,1672,1678,1680,1826,1830,1838,1842,1844,1846,1848,1854,1856,2002,2004,2006,2008,2014,2016,2146,2148,2150,2152,2158,2160,2258,2262,2270,2274,2276,2278,2280,2286,2288,2434,2436,2438,2440,2446,2448,2578,2580,2582,2584,2590,2592,5203,5207,5215,5234,5238,5246,5250,5252,5254,5256,5262,5264,5363,5365,5367,5373,5375,5410,5412,5414,5416,5422,5424,5505,5507,5509,5511,5517,5519,5554,5556,5558,5560,5566,5568,5714,5718,5726,5730,5732,5734,5736,5742,5744,5890,5892,5894,5896,5902,5904,6034,6036,6038,6040,6046,6048,6146,6150,6158,6162,6164,6166,6168,6174,6176,6322,6324,6326,6328,6334,6336,6466,6468,6470,6472,6478,6480,14269,14275,14279,14285,14287,14306,14310,14318,14322,14324,14334,14336,14435,14439,14445,14447,14484,14494,14496,14579,14581,14583,14589,14591,14628,14632,14638,14640,14786,14790,14798,14802,14804,14806,14808,14814,14816,14964,14974,14976,15108,15118,15120,15218,15222,15230,15234,15236,15238,15240,15246,15248,15394,15396,15398,15400,15406,15408,15538,15540,15542,15544,15550,15552,18163,18167,18175,18194,18198,18206,18210,18212,18323,18327,18335,18372,18467,18469,18471,18477,18479,18516,18520,18528,18674,18678,18686,18690,18692,18694,18696,18702,18704,18852,18996,19106,19110,19118,19122,19124,19126,19128,19134,19136,19282,19284,19286,19288,19294,19296,19426,19428,19430,19432,19438,19440,25933,25939,25943,25949,25951,25970,25974,25982,25986,25988,25998,26000,26099,26103,26109,26111,26146,26148,26158,26160,26243,26247,26253,26255,26292,26302,26304,26450,26454,26462,26466,26468,26470,26472,26478,26480,26626,26628,26638,26640,26772,26782,26784,26882,26886,26894,26898,26900,26902,26904,26910,26912,27058,27060,27062,27064,27070,27072,27204,27214,27216,29827,29831,29839,29858,29862,29870,29874,29876,29987,29991,29999,30034,30036,30131,30135,30143,30180,30184,30338,30342,30350,30354,30356,30358,30360,30366,30368,30514,30516,30660,30770,30774,30782,30786,30788,30790,30792,30798,30800,30946,30948,30950,30952,30958,30960,31092,57043,57047,57055,57074,57078,57086,57090,57092,57094,57096,57203,57207,57215,57252,57256,57347,57349,57351,57357,57359,57396,57400,57408,57554,57558,57566,57570,57572,57574,57576,57582,57584,57732,57876,57986,57990,57998,58002,58004,58006,58008,58014,58016,58162,58164,58166,58168,58174,58176,58306,58308,58310,58312,58318,58320,68707,68711,68719,68738,68742,68750,68754,68756,68758,68760,68867,68871,68879,68914,68916,68920,69011,69015,69023,69060,69064,69218,69222,69230,69234,69236,69238,69240,69246,69248,69394,69396,69540,69650,69654,69662,69666,69668,69670,69672,69678,69680,69826,69828,69830,69832,69838,69840,69972,103699,103703,103711,103730,103734,103742,103746,103748,103750,103752,103859,103863,103871,103906,103908,103910,103912,104003,104007,104015,104052,104056,104210,104214,104222,104226,104228,104230,104232,104238,104240,104386,104388,104390,104392,104532,104536,104642,104646,104654,104658,104660,104662,104664,104670,104672,104818,104820,104822,104824,104830,104832};
reducedpolys={{beta-e[2],beta-e[2]+f e[2]},{1+beta-e[2],1+beta-e[2]+f e[2]},{beta-e[2]+f e[2],beta+f e[1]-e[2]},{1+beta-e[2]+f e[2],1+beta+f e[1]-e[2]},{1+beta-e[1],1+beta-e[1]+f e[2]},{2+2 beta+mu+beta mu-e[1]-mu e[1],2+2 beta+mu+beta mu-e[1]-mu e[1]+f e[2]+mu f e[2]},{1+beta-e[1]+f e[2],1+beta-e[1]+f e[1]},{2+2 beta+mu+beta mu-e[1]-mu e[1]+f e[2]+mu f e[2],2+2 beta+mu+beta mu-e[1]+f e[1]-mu e[1]+mu f e[1]},{beta mu+f e[1]+mu f e[1]-e[2]-mu e[2],1},{mu+mu beta+f e[1]+mu f e[1]-e[2]-mu e[2],1},{mu+mu beta-e[1]+f e[1]-mu e[1]+mu f e[1],1},{1+beta-e[1]+f e[1],1}};
reducedineqs =Table[{{reducedpolys[[i]][[1]]<0,reducedpolys[[i]][[2]]<0},{reducedpolys[[i]][[1]]<0,reducedpolys[[i]][[2]]>0},{reducedpolys[[i]][[1]]>0,reducedpolys[[i]][[2]]>0}},{i,1,12}];
pos = (beta>0)\[And](mu>0)\[And](0<f<1)\[And](e[1]>e[2]>0);
vars= {beta,mu,e[1],e[2],f};

firstindex=Table[{1,2,3},{i,1,8}];
secondindex = Table[{2,3},{i,1,4}];
allindex =Join[firstindex,secondindex];
fullindex = Tuples[allindex];
For[i=1,i<=Length[rest],i++,
ii=fullindex[[rest[[i]]]];statement={};
For[j=1,j<=Length[reducedineqs],j++,statement=Join[statement,reducedineqs[[j]][[ii[[j]]]]];];
qef= And@@statement\[And]pos;
output = Timing[CylindricalDecomposition[Exists[Evaluate[vars],qef],{}]];
file =OpenAppend["rest_output.txt"];Write[file,output];Close[file];];
]
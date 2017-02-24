
BeginPackage["AMASeriesRepresentation`",
 {"JLink`","ProtectedSymbols`","mathSmolyak`"}]

(*Begin Usage Definitions*)
PerfectForesight::usage="degenerate distribution implementing perfect foresight"
smolyakInterpolation::usage=
"place holder for makeSmolyakInterpFunc"

smolyakInterpolationPrep::usage="place holder"

genLilXkZkFunc::usage=
"genLilXkZkFunc[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
                psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} ,XZFuncs:{(_Function|_InterpolatingFunction|_CompiledFunction),numSteps_Integer},xtGuess_?MatrixQ]"<>
"\ngenerate a function that computes x and z given a guess for xt\n"<>
"genLilXkZkFunc[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
                psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} ,fCon_?MatrixQ]"<>
"\ngenerate a function that computes x z based on an assumed F sum\n"<>
"genLilXkZkFunc[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
                psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} ,theZs:{_?MatrixQ..}]"<>
"\ngenerate a function that computes x and z given sequence of Zs\n"<>
" genLilXkZkFunc[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
                 psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} ,{}]"<>
"\ngenerate a function that computes x for Zs = 0\n"
 genLilXkZkFunc::usage="place holder"

(*some getters usage*)

worstPathForErrDRREIntegrate::usage=
"place holder for worstPathForErrDRREIntegrate"

evalBadPathErrDRREIntegrate::usage=
"place holder for evalBadPathErrDRREIntegrate"

evalPathErrDRREIntegrate::usage=
"place holder for evalPathErrDRREIntegrate"

doFuncArg::usage=
"place holder for doFuncArg"

pathErrsDRPF::usage=
"place holder for pathErrsDRPF"

pathErrsDRREIntegrate::usage=
"place holder for pathErrsDRREIntegrate"

iterateDRPF::usage=
"place holder for iterateDRPF"

genNSFunc::usage=
"place holder for genNSFunc"

makeREIterFunc::usage=
"place holder for makeREIterFunc"

myNExpectation::usage=
"place holder for myNExpectation"

getDistribs::usage=
"place holder for getDistribs"

genXZFuncRE::usage=
"place holder for genXZFuncRE"

genIntVars::usage=
"place holder for genIntVars"


parallelGenXZREInterpFunc::usage=
"place holder for parallelGenXZREInterpFunc"

genSmolyakXZREInterpFunc::usage=
"place holder for genSmolyakXZREInterpFunc"

genXZREInterpFunc::usage=
"place holder for genXZREInterpFunc"

genX0Z0Funcs::usage=
"place holder for genX0Z0Funcs"

checkMod::usage=
"place holder for checkMod"

genFRFunc::usage=
"place holder for genFRFunc"

genFPFunc::usage=
"place holder for genFPFunc"

myFixedPoint::usage=
"place holder for myFixedPoint"


getH::usage=
"getH[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
      psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} ]"<>
"number of z variables"


getB::usage=
"getB[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
      psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} ]"<>
"number of z variables"


getF::usage=
"getF[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
      psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} ]"<>
"number of z variables"

getGridPtTrips::usage=
"place holder for getGridPtTrips"

getNumVars::usage=
"place holder for getNumVars"

makeSmolyakInterpFunc::usage=
"place holder for makeSmolyakInterpFunc"

parallelMakeInterpFunc::usage=
"place holder for parallelMakeInterpFunc"

makeInterpFunc::usage=
"place holder for makeInterpFunc"

parallelSmolyakGenInterpData::usage=
"place holder for parallelSmolyakGenInterpData"

smolyakGenInterpData::usage=
"place holder for smolyakGenInterpData"

parallelGenInterpData::usage=
"place holder for parallelGenInterpData"

genInterpData::usage=
"place holder for genInterpData"

oneDimGridPts::usage=
"place holder for oneDimGridPts"

gridPts::usage=
"place holder for gridPts"

fillIn::usage=
"place holder for fillIn"

fillInSymb::usage=
"place holder for fillInSymb"

parallelNestIterREInterp::usage=
"place holder for parallelNestIterREInterp"
 
parallelDoIterREInterp::usage=
"place holder for parallelDoIterREInterp"
 
nestIterREInterp::usage=
"place holder for nestIterREInterp"

doIterREInterp::usage=
"place holder for doIterREInterp"
 
nestSmolyakIterREInterp::usage=
"place holder for nestSmolyakIterREInterp"

doSmolyakIterREInterp::usage=
"place holder for doSmolyakIterREInterp"


getPhi::usage=
"getPhi[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
        psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} ]"<>
"number of z variables"


getPsiZ::usage=
"getPsiZ[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
         psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} ]"<>
"number of z variables"


getPsiC::usage=
"getPsiC[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
         psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} ]"<>
"number of z variables"


getPsiEps::usage=
"getPsiEps[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
           psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} ]"<>
"number of z variables"


getNumZ::usage=
"getNumZ[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
         psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} ]"<>
"number of z variables"


getNumX::usage=
"getNumX[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
         psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} ]"<>
"number of x variables"


getNumEps::usage=
"getNumEps[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
           psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} ]"<>
"number of eps variables"

multiStep::usage=
"place holder for multiStep"

multiStepZ::usage=
"place holder for multiStepZ"

multiStepX::usage=
"place holder for multiStepX"

checkLinMod::usage=
"place holder for checkLinMod"


fSumC::usage=
"compiled function computing the sum of the Zs weighted by F"


fSum::usage=
"place holder fSum"

getNumEpsVars::usage=
"place holder for getNumEpsVars"

iterateDRREIntegrate::usage=
"place holder for iterateDRREIntegrate"

genPath::usage=
"place holder for genPath"


getNumIgnored::usage=
"getNumIgnored[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
               psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} ]"<>
"number of eps variables"


getNumInterpVars::usage=
"getNumInterpVars[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
                  psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} ]"<>
"number of eps variables"


Begin["`Private`"]


(*begin code for parallelSmolyakGenInterpData*)

 
parallelSmolyakGenInterpData[aVecFunc:(_Function|_CompiledFunction),gSpec:{toIgnore:{_Integer...},iOrd_Integer,rngs:{{_Integer,_?NumberQ,_?NumberQ}..}},smolGSpec:{smolToIgnore:{_Integer...},smolRngs:{{_?NumberQ,_?NumberQ}..},smolPts_?MatrixQ,smolMat_?MatrixQ,smolPolys_?VectorQ,smolIntPolys_?VectorQ,numEps_Integer}]:=
With[{filledPts=ParallelMap[
Function[xx,fillIn[{{},smolToIgnore,xx}]],smolyakPts]},
With[{theVals=ParallelMap[
Function[xx,(Apply[aVecFunc,xx])],filledPts]},
With[{interpData=Transpose[{smolyakPts,theVals}]},
interpData]]]





(*end code for parallelSmolyakGenInterpData*)


(*begin code for smolyakGenInterpData*)

 
smolyakGenInterpData[
aVecFunc:(_Function|_CompiledFunction),smolGSpec:{smolToIgnore:{_Integer...},smolRngs:{{_?NumberQ,_?NumberQ}..},smolPts_?MatrixQ,smolMat_?MatrixQ,smolPolys_?VectorQ,smolIntPolys_?VectorQ,numEps_Integer}]:=
With[{filledPts=Map[
Function[xx,fillIn[{{},smolToIgnore,xx}]],N[smolPts]]},
(*Print["smolyakGenInterpData:",filledPts//InputForm];*)
With[{theVals=Map[
Function[xx,(Apply[aVecFunc,xx])],filledPts]},
With[{interpData=Map[Flatten,theVals]},
interpData]]]





(*end code for smolyakGenInterpData*)


smolyakInterpolation[fVals:{_?NumberQ..},smolGSpec:{smolToIgnore:{_Integer...},smolRngs:{{_?NumberQ,_?NumberQ}..},smolPts_?MatrixQ,smolMat_?MatrixQ,smolPolys_?VectorQ,smolIntPolys_?VectorQ,numEps_Integer}]:=
With[{wts=LinearSolve[smolMat,fVals],numVars=Length[smolRngs]},
With[{origXs=Table[xx[ii],{ii,numVars}],
theXs=Table[Unique["xx"],{ii,numVars}]},
{Apply[Function,({theXs,Simplify[(wts.(smolPolys/.Thread[origXs->theXs]))/.Thread[theXs->MapThread[xformXValToCheb,{theXs,smolRngs}]]]})],
Apply[Function,({Drop[theXs,-numEps],Simplify[(wts.(smolIntPolys/.
Thread[Drop[origXs,-numEps]->Drop[theXs,-numEps]]))/.Thread[theXs->MapThread[xformXValToCheb,{theXs,smolRngs}]]]})]}]]



smolyakInterpolationPrep[approxLevels_?listOfIntegersQ,smolRngs_?MatrixQ,
distribSpec:{expctSpec:{{_Symbol,_}..}}]:=
Module[{smolRes=sparseGridEvalPolysAtPts[approxLevels],
numVars=Length[approxLevels],numEps=Length[distribSpec[[1]]]},
With[{thePts=smolRes[[1]],smolPolys=smolRes[[2]],smolMat=smolRes[[3]]},
With[{xPts=Map[Function[xx,xformToXVec[xx,smolRngs]],thePts]},
(*Print["smolyPrep:",{xPts,thePts}];*)
With[{numPolys=Length[smolPolys]},
{xPts,smolMat,smolPolys,{}}]]]]/;
And[Length[smolRngs]==Length[approxLevels]]

newSmolyakInterpolationPrep[approxLevels_?listOfIntegersQ,smolRngs_?MatrixQ,
momSubs:{{(_->_)..}...}]:=
Module[{smolRes=sparseGridEvalPolysAtPts[approxLevels],
numVars=Length[approxLevels],numEps=Length[momSubs]},
With[{thePts=smolRes[[1]],smolPolys=smolRes[[2]],smolMat=smolRes[[3]]},
With[{xPts=Map[Function[xx,xformToXVec[xx,smolRngs]],thePts]},
(*Print["smolyPrep:",{xPts,thePts}];*)
With[{numPolys=Length[smolPolys]},
{xPts,smolMat,smolPolys,compRawMoments[smolPolys,xx[3]]/.Flatten[momSubs]}]]]]/;
And[Length[smolRngs]==Length[approxLevels]]


compRawMoments[expr_,errVar:anX_Symbol[ii_Integer]]:=
expr/.{errVar^nn_Integer->mom[ii,nn],errVar->mom[ii,1]}


xformXValToCheb[xVal_,
range:{lowVal_?NumberQ,highVal_?NumberQ}]:=
xFormToChebInterval[xVal,lowVal,highVal]

xformChebValToX[chebVal_,
range:{lowVal_?NumberQ,highVal_?NumberQ}]:=
xFormFromChebInterval[chebVal,lowVal,highVal]

xformToXVec[chebPt_?VectorQ,ranges_?MatrixQ]:=
MapThread[xformChebValToX,{chebPt,ranges}]




(*some getters*)


getNumIgnored[gSpec:{toIgnore:{_Integer...},iOrd_Integer,rngs:{{_Integer,_?NumberQ,_?NumberQ}..}}]:=
Length[gSpec[[1]]]


getNumInterpVars[gSpec:{toIgnore:{_Integer...},iOrd_Integer,rngs:{{_Integer,_?NumberQ,_?NumberQ}..}}]:=
Length[gSpec[[3]]]


(*begin code for worstPathForErrDRREIntegrate*)

worstPathForErrDRREIntegrate[drFunc_Function,noEpsVec_?MatrixQ,distribSpec:{expctSpec:{{_Symbol,_}..}},eqnsFunc:(_Function|_CompiledFunction)]:=
With[{fMinRes=evalBadPathErrDRREIntegrate[drFunc,noEpsVec,distribSpec,eqnsFunc]},
        With[{badEps=Transpose[{(Map[First,fMinRes[[2]]])/.fMinRes[[2]]}]},
        With[{badPath=iterateDRREIntegrate[drFunc,Join[noEpsVec,badEps],distribSpec,2]},
                Join[badPath,badEps]]]]

worstPathForErrDRREIntegrate[phi_?MatrixQ,
drFunc_Function,noEpsVec_?MatrixQ,distribSpec:{expctSpec:{{_Symbol,_}..}},eqnsFunc:(_Function|_CompiledFunction)]:=
With[{fMinRes=
evalBadPathErrDRREIntegrate[phi,drFunc,noEpsVec,distribSpec,eqnsFunc]},
        With[{badEps=Transpose[{(Map[First,fMinRes[[2]]])/.fMinRes[[2]]}]},
        With[{badPath=iterateDRREIntegrate[drFunc,Join[noEpsVec,badEps],distribSpec,2]},
                Join[badPath,badEps]]]]

(*end code for worstPathForErrDRREIntegrate*)


(*begin code for evalBadPathErrDRREIntegrate*)
evalBadPathErrDRREIntegrate[drFunc_Function,noEpsVec_?MatrixQ,distribSpec:{expctSpec:{{_Symbol,_}..}},eqnsFunc:(_Function|_CompiledFunction)]:=
With[{funcName=Unique["fName"]},
funcName[tryEps:{_?NumberQ..}]:=
        With[{theVal=evalPathErrDRREIntegrate[drFunc,Join[noEpsVec,Transpose[{tryEps}]],distribSpec,eqnsFunc]},
                With[{theNorm=Norm[theVal,Infinity]},
                (*Print["stillex:",{tryEps,theVal,Norm[theVal,Infinity],theNorm}];*)theNorm]];
        With[{outerEVars=Table[Unique["eVs"],{getNumEpsVars[distribSpec]}]},
        With[{maxArgs=Map[Function[xx,{xx,0}],outerEVars],cons=Apply[And,  (Map[Function[xx,(-0.01<=xx<=0.01)], outerEVars])]},
        FindMaximum[{funcName[outerEVars],cons},maxArgs]]]]


evalBadPathErrDRREIntegrate[phi_?MatrixQ,
drFunc_Function,noEpsVec_?MatrixQ,distribSpec:{expctSpec:{{_Symbol,_}..}},eqnsFunc:(_Function|_CompiledFunction)]:=
With[{funcName=Unique["fName"]},
funcName[tryEps:{_?NumberQ..}]:=
With[{theVal=evalPathErrDRREIntegrate[drFunc,Join[noEpsVec,Transpose[{tryEps}]],distribSpec,eqnsFunc]},
                (*Print["otherex:",theVal,Norm[theVal,Infinity]];*)Norm[theVal,Infinity]];
        With[{outerEVars=Table[Unique["eVs"],{getNumEpsVars[distribSpec]}]},
        With[{maxArgs=Map[Function[xx,{xx,0}],outerEVars],cons=Apply[And,  (Map[Function[xx,(-0.01<=xx<=0.01)], outerEVars])]},
        FindMaximum[{funcName[outerEVars],cons},maxArgs]]]]

(*end code for evalBadPathErrDRREIntegrate*)


(*begin code for evalPathErrDRREIntegrate*)
evalPathErrDRREIntegrate[drFunc_Function,initVec_?MatrixQ,distribSpec:{expctSpec:{{_Symbol,_}..}},eqnsFunc:(_Function|_CompiledFunction)]:=
pathErrsDRREIntegrate[drFunc,initVec,distribSpec,eqnsFunc,2]//First



evalPathErrDRREIntegrate[phi_?MatrixQ,
drFunc_Function,initVec_?MatrixQ,distribSpec:{expctSpec:{{_Symbol,_}..}},eqnsFunc:(_Function|_CompiledFunction)]:=
phi . (pathErrsDRREIntegrate[drFunc,initVec,distribSpec,eqnsFunc,2])//First




(*end code for evalPathErrDRREIntegrate*)


(*begin code for doFuncArg*)
doFuncArg[pathNow_?MatrixQ,epsVals_?MatrixQ,numX_Integer,oSet_Integer]:=
With[{firstArg=Join[Identity[pathNow[[oSet*numX+Range[3*numX]]]],Identity[epsVals]]},
firstArg]


(*end code for doFuncArg*)


(*begin code for genPath*)


genPath[xzFunc_Function,
XZFuncs:{(_Function|_InterpolatingFunction|_CompiledFunction),numSteps_Integer},xtm1Val_?MatrixQ,epsVal_?MatrixQ,numTerms_Integer:1]:=
With[{numXVars=Length[xtm1Val]},
With[{xtVal=Apply[xzFunc,Flatten[Join[xtm1Val,epsVal]]]},
With[{xzRes=If[numTerms==1,{},
Apply[multiStepX[XZFuncs,numXVars,numTerms-1],Flatten[xtVal]]]},
        Join[xtm1Val,xtVal[[Range[numXVars]]],Apply[Join,xzRes]]]]]
(*end code for genPath*)


(*begin code for pathErrsDRPF*)
   
 
pathErrsDRPF[drFunc_Function,initVec_?MatrixQ,numEps_Integer,eqnsFunc:(_Function|_CompiledFunction),numPers_Integer]:=
With[{pathNow=iterateDRPF[drFunc,initVec,numEps,numPers],numX=Length[initVec]-numEps},
With[{firstArg=doFuncArg[pathNow,Identity[Reverse[initVec[[-Range[numEps]]]]],numX,0],
        restArgs=(Map[Function[xx,
doFuncArg[pathNow,Table[{0},{numEps}],numX,xx-2]],Range[3,numPers]])},
With[{first=Transpose[{Apply[eqnsFunc,Flatten[firstArg]]}]},
        With[{theRest=Map[Function[xx,Transpose[{(Apply[eqnsFunc,Flatten[xx]])}]],restArgs]},
                Prepend[theRest,first]
]]]]/;
And[numPers>1]


(*end code for pathErrsDRPF*)


(*begin code for pathErrsDRREIntegrate*)
pathErrsDRREIntegrate[drFunc_Function,initVec_?MatrixQ,distribSpec:{expctSpec:{{_Symbol,_}..}},eqnsFunc:(_Function|_CompiledFunction),numPers_Integer]:=
With[{numEps=getNumEpsVars[distribSpec]},
With[{pathNow=iterateDRREIntegrate[drFunc,initVec,distribSpec,numPers],numX=Length[initVec]-numEps},(*Print["pathErrsDRREIntegrate:",pathNow];*)
With[{firstArg=doFuncArg[pathNow,Identity[Reverse[initVec[[-Range[numEps]]]]],numX,0],
        restArgs=(Map[doFuncArg[pathNow,Table[{0},{numEps}],numX,genSlot[1]-2]&,Range[3,numPers]])},
With[{first=Transpose[{Apply[eqnsFunc,Flatten[firstArg]]}]},
        With[{theRest=Map[Transpose[{(Apply[eqnsFunc,Flatten[genSlot[1]]])}]&,restArgs]},(*Print["pathErrs:",{pathNow,theRest,first}];*)
                Prepend[theRest,first]
]]]]]/;
And[numPers>1]
 

(*end code for pathErrsDRREIntegrate*)


(*begin code for iterateDRPF*)
 
iterateDRPF[drFunc_Function,initVec_?MatrixQ,numEps_Integer,numPers_Integer]:=
With[{firVal=Apply[drFunc,Flatten[initVec]],numX=Length[initVec]-numEps,theZeros=Table[0,{numEps}]},
With[{iterated=
NestList[Function[xx,(Apply[drFunc,Flatten[Append[xx[[Range[numX]]],theZeros]]])],firVal,numPers-1]},
Join[initVec[[Range[numX]]],Apply[Join,(Map[Function[xx,xx[[Range[numX]]]],iterated])]]]]/;
And[numPers>0]


(*end code for iterateDRPF*)


(*begin code for genNSFunc*)
genNSFunc[{numX_Integer,numEps_Integer,numZ_Integer},
xkFunc:(_Function|_CompiledFunction),eqnsFunc:(_Function|_CompiledFunction),opts:OptionsPattern[]]:=
With[{funcArgs=Table[Unique["theFRFuncArgs"],{numX+numEps}],
zArgs=Table[Unique["theFRZArgs"],{numZ}]},
funcName[funcArgsNot:{_(*?NumberQ*)..}]:=
Module[{theVars=Join[funcArgsNot]},
Apply[eqnsFunc,(Flatten[Apply[xkFunc,theVars]])]];
ReplacePart[
Function[xxxx,With[{zVals=zArgs/.NSolve[funcName[Join[funcArgs,zArgs]],zArgs,Reals,Apply[Sequence,FilterRules[{opts},Options[NSolve]]]][[1]]},
Join[(Apply[xkFunc,Join[funcArgs,zVals]])[[numX+Range[numX]]],
Transpose[{zVals}]]]],
1->funcArgs]]


(*end code for genNSFunc*)


(*begin code for iterateDRREIntegrate*)
iterateDRREIntegrate[drFunc:(_Function|_CompiledFunction),initVec_?MatrixQ,
        distribSpec:{expctSpec:{{_Symbol,_}..}},numPers_Integer]:=
With[{numEps=getNumEpsVars[distribSpec],firVal=Apply[drFunc,Flatten[initVec]]},
        With[{numX=Length[initVec]-numEps,iterFunc=makeREIterFunc[drFunc,distribSpec]},
With[{iterated=
NestList[Function[xx,((Transpose[{Flatten[Apply[iterFunc,Flatten[xx]]]}]))],firVal,numPers-1]},
Join[initVec[[Range[numX]]],Apply[Join,
(Map[Function[xx,Identity[xx[[Range[numX]]]]],iterated])]]]]]/;
And[numPers>0]


(*end code for iterateDRREIntegrate*)


(*begin code for makeREIterFunc*)

makeREIterFunc[drFunc:(_Function|_CompiledFunction),distribSpec:{expctSpec:{{_Symbol,_}..}}]:=
With[{numEps=getNumEpsVars[distribSpec]},
With[{numX=Length[drFunc[[1]]]-numEps,numZ=0},
        genXZFuncRE[{numX,numEps,numZ},drFunc,distribSpec]]]


(*end code for makeREIterFunc*)


(*begin code for getNumEpsVars*)
getNumEpsVars[distribSpec:{expctSpec:{{_Symbol,_}..}}]:=Length[expctSpec]


(*end code for getNumEpsVars*)


(*begin code for myNExpectation*)



myNExpectation[funcName_Symbol[funcArgs_List,idx_Integer],anEpsVar_\[Distributed] PerfectForesight]:=
Apply[funcName,Append[ReplacePart[{funcArgs},{{(1),(-1)}->0}],idx]]
myNExpectation[funcName_Symbol[funcArgs_List,idx_Integer],{anEpsVar_\[Distributed] PerfectForesight}]:=
Apply[funcName,Append[ReplacePart[{funcArgs},{{1,(-1)}->0}],idx]]

myNExpectation[funcName_Symbol[farg_List,idx_Integer],nArgs_List]:=Chop[NExpectation[funcName[farg,idx],nArgs]]


myNewNExpectation[fff_[fargs___],anEpsVar_\[Distributed] PerfectForesight]:=Module[{},Print["there",{(Apply[fff,{fargs}]),{fargs}/.anEpsVar->0}];(Apply[fff,{fargs}])/.anEpsVar->0]


myNewNExpectation[fff_[fargs___],distStuff_]:=Module[{},Print["jhere",{(Apply[fff,{fargs}]),{fargs}}];Chop[NExpectation[Apply[fff,{fargs}],distStuff]]]



(*end code for myNExpectation*)


(*begin code for getDistribs*)

getDistribs[distribSpec:{expctSpec:{{_Symbol,_}..}}]:= Map[Last,expctSpec]
(*{numReg,tranType,tranFunc}*)

(*end code for getDistribs*)


(*begin code for getNumVars*)
getNumVars[gSpec:{toIgnore:{_Integer...},iOrd_Integer,rngs:{{_Integer,_?NumberQ,_?NumberQ}..}}]:=
(Length[getGridPtTrips[gSpec]])

(*end code for getNumVars*)


(*begin code for getGridPtTrips*)

getGridPtTrips[gSpec:{toIgnore:{_Integer...},iOrd_Integer,rngs:{{_Integer,_?NumberQ,_?NumberQ}..}}]:=gSpec[[3]]
  

(*end code for getGridPtTrips*)


getH[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
     psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} ]:=
theHMat


getB[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
     psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} ]:=
BB


getF[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
     psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} ]:=
FF


getPhi[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
       psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} ]:=
phi


getPsiZ[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
        psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} ]:=
psiZ


getPsiC[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
        psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} ]:=
psiC


getPsiEps[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
          psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} ]:=
psiEps


getNumZ[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
        psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} ]:=
Length[getPsiZ[linMod][[1]]]


getNumX[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
        psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} ]:=
Length[getB[linMod]]


getNumEps[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
          psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} ]:=
Length[getPsiEps[linMod][[1]]]


 genLilXkZkFunc[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
                psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} ,{}]:=
With[{numZ=getNumZ[linMod]},
With[{fCon=ConstantArray[0,{1,numZ,1}]},
With[{theRes=genLilXkZkFunc[linMod,fCon]},theRes]]]


genLilXkZkFunc[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
               psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} ,theZs:{_?MatrixQ..}]:=
With[{fCon=fSumC[phi,FF,psiZ,theZs]},
With[{theRes=genLilXkZkFunc[linMod,fCon]},
theRes]]


genLilXkZkFunc[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
               psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} ,XZFuncs:{(_Function|_InterpolatingFunction|_CompiledFunction),numSteps_Integer},xtGuess_?MatrixQ]:=
With[{fCon=fSum[linMod,XZFuncs,xtGuess]},
With[{theRes=genLilXkZkFunc[linMod,fCon]},
theRes]]


genLilXkZkFunc[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
               psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} ,fCon_?MatrixQ]:=
With[{numXVars=getNumX[linMod],numEpsVars=getNumEps[linMod],
numZVars=getNumZ[linMod]},
With[{theSlots=genSlots[numXVars+numEpsVars+numZVars]},
With[{xtm1Vars=theSlots[[Range[numXVars]]],
epsVars=theSlots[[numXVars+Range[numEpsVars]]],
zVars=theSlots[[numXVars+numEpsVars+Range[numZVars]]]},
With[{xtVals=genXtOfXtm1[linMod,xtm1Vars,epsVars,zVars,fCon]},
With[{xtp1Vals=genXtp1OfXt[linMod,xtVals,fCon]},
With[{fullVec=Join[xtm1Vars,xtVals,xtp1Vals,epsVars]},
Function[fullVec]]
]]]]]




fSumC=Compile[{{phi,_Real,2},{FF,_Real,2},{psiZ,_Real,2},{zPath,_Real,3}},
With[{numXVars=Length[psiZ]},
With[{fPows=Drop[NestList[Function[xx,FF. xx],IdentityMatrix[numXVars],Length[zPath]],-1]},
Apply[Plus,
MapThread[Function[{xx,yy},Dot[xx,phi.psiZ.yy]],{fPows , zPath}]]]]]



fSum[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
     psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} ,
        {},
        xtGuess_?MatrixQ]:=
ConstantArray[0,{Length[psiZ],1}]

fSum[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
     psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} ,
        XZFuncs:{(_Function|_InterpolatingFunction|_CompiledFunction),numSteps_Integer},xtGuess_?MatrixQ]:=
With[{numXVars=Length[BB],numZVars=Length[psiZ[[1]]]},
With[{xzRes=Apply[multiStepZ[XZFuncs,numXVars,numZVars,numSteps], Flatten[xtGuess]]},
fSumC[phi,FF,psiZ,xzRes]]]


(*begin code for genSlots*)
genSlots[numVars_Integer]:=
Module[{},
genSlots[numVars]=
replaceMySlotStandIn[Table[{mySlotStandIn[ii]},{ii,numVars}]]]/;And[numVars>=0]

replaceMySlotStandIn[xx_]:=xx/.mySlotStandIn->Slot



genSlot[slotNum_Integer]:=
Module[{},
genSlot[slotNum]=
replaceMySlotStandIn[mySlotStandIn[slotNum]]]/;And[slotNum>0]

(*end code for genSlots*)


(*begin code for genXtOfXtm1*)
genXtOfXtm1[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
            psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} ,xtm1Vars_?MatrixQ,epsVars_?MatrixQ,zVars_?MatrixQ,
        fCon_?MatrixQ]:=
With[{xtVals=BB.xtm1Vars+
Inverse[IdentityMatrix[Length[xtm1Vars]]-FF] . phi . psiC + phi . psiEps . epsVars+
phi . psiZ . zVars +FF.fCon},xtVals]

(*end code for genXtOfXtm1*)


(*begin code for genXtp1OfXt*)

genXtp1OfXt[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
            psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} ,xtVals_?MatrixQ,
        fCon_?MatrixQ]:=
With[{xtp1Vals=BB.xtVals+Inverse[IdentityMatrix[Length[xtVals]]-FF] . phi . psiC+fCon},xtp1Vals]


(*end code for genXtp1OfXt*)


(*begin code for genX0Z0Funcs*)
genX0Z0Funcs[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
             psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} ]:=
With[{numXVars=Length[BB],numZVars=Length[psiZ[[1]]]},
With[{xtm1Vars=genSlots[numXVars]},
Apply[Function, {Join[BB.xtm1Vars+
Inverse[IdentityMatrix[Length[xtm1Vars]]-FF] . phi . psiC,ConstantArray[0,{numZVars,1}]]}]]]
(*end code for genX0Z0Funcs*)


(*begin code for multiStep*)

multiStep[XZFuncs:{(_Function|_InterpolatingFunction|_CompiledFunction),numSteps_Integer},numX_Integer,valRange:{_Integer..},numTerms_Integer]:=
With[{funcArgs=Flatten[genSlots[numX]]},
With[{appGuts=(Apply[XZFuncs[[1]],Flatten[funcArgs]][[Range[numX]]])},
With[{xtFunc01=Function[appGuts]},
With[{iterGuts=
NestList[Function[xx,Apply[xtFunc01,Flatten[xx]]],funcArgs,numTerms-1]},
With[{theXZGuts=Map[(Function[xx,
Apply[XZFuncs[[1]],Flatten[xx]][[valRange]]]),iterGuts]},
With[{theFunc=Function[theXZGuts]},
theFunc]]]]]]/;numSteps>0


(*end code for multiStep*)


(*begin code for multiStepZ*)
multiStepZ[XZFuncs:{(_Function|_InterpolatingFunction|_CompiledFunction),numSteps_Integer},numX_Integer,numZ_Integer,numTerms_Integer]:=
multiStep[XZFuncs,numX,numX+Range[numZ],numTerms]

(*end code for multiStepZ*)


(*begin code for multiStepX*)
multiStepX[XZFuncs:{(_Function|_InterpolatingFunction|_CompiledFunction),numSteps_Integer},numX_Integer,numTerms_Integer]:=
multiStep[XZFuncs,numX,Range[numX],numTerms]

(*end code for multiStepX*)


(*begin code for checkLinMod*)

checkLinMod[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
            psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} ,
anX_?MatrixQ,anEps_?MatrixQ]:=
With[{X0Z0=genX0Z0Funcs[linMod],numZ=Length[psiZ[[1]]]},
With[{lilxz=genLilXkZkFunc[linMod, {X0Z0,2}, Join[anX,anEps]]},
        {Eigenvalues[BB]//Abs,Eigenvalues[FF]//Abs,Apply[X0Z0,Flatten[anX]],Apply[lilxz,Flatten[Join[anX,anEps,Table[{0},{numZ}]]]]}]]


(*end code for checkLinMod*)


(*begin code for checkMod*)



checkMod[theSolver:(({genFRFunc,opts:OptionsPattern[]}|{genNSFunc,opts:OptionsPattern[]}|{specialSolver,opts:OptionsPattern[]})),linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
          psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} ,
gSpec:{toIgnore:{_Integer...},iOrd_Integer,rngs:{{_Integer,_?NumberQ,_?NumberQ}..}},
distribSpec:{expctSpec:{{_Symbol,_}..}},anX_?MatrixQ,anEps_?MatrixQ,ss_?MatrixQ,
eqnsFunc:(_Function|_CompiledFunction)]:=
With[{X0Z0=genX0Z0Funcs[linMod],numX=Length[BB],numEps=Length[psiEps[[1]]],numZ=Length[psiZ[[1]]]},
With[{lilxz=
genLilXkZkFunc[linMod, {X0Z0,1}, Join[anX,anEps]]},
With[{xzFuncNow=theSolver[[1]][{numX,numEps,numZ},lilxz,eqnsFunc,Method->"JenkinsTraub"]},
With[{fp=genFPFunc[theSolver,linMod,{X0Z0,2},eqnsFunc]},
{Apply[lilxz,Flatten[Join[anX,anEps,Table[0,{numZ}]]]],
Apply[xzFuncNow,Flatten[Join[anX,anEps]]],
Apply[fp,Flatten[Join[anX,anEps]]],
Apply[eqnsFunc,Flatten[Join[ss,{{0}}]]]
}]]]]



(*end code for checkMod*)



(*begin code for genFRFunc*)
 
genFRFunc[{numX_Integer,numEps_Integer,numZ_Integer},
xkFunc:(_Function|_CompiledFunction),eqnsFunc:(_Function|_CompiledFunction),opts:OptionsPattern[]]:=
With[{funcArgs=Flatten[genSlots[numX+numEps]],
zArgs=Table[Unique["theFRZArgs"],{numZ}]},
With[{zArgsInit=Map[Function[xx,{xx,0}],zArgs],funcName=Unique["fName"]},
funcName[theVars:{_?NumberQ..}]:=
Apply[eqnsFunc,Flatten[Apply[xkFunc,theVars]]];Off[FindRoot::nlnum];
With[{frRes=FindRoot[funcName[Join[funcArgs,zArgs]],zArgsInit],
xzRes=Drop[Apply[xkFunc,Join[funcArgs,zArgs]],numX][[Range[numX]]]},
With[{otherGuts=cmpXZVals[xzRes,zArgs,frRes]},
On[FindRoot::nlnum];
Function[otherGuts]]]]]

(* input   [function (xt,eps,zt)->(xtm1,xt,xtp1,eps), function (xtm1,xt,xtp1,eps)->me]*)
(* output   [function  (xt,eps) ->(xt,zt)] *)
 
cmpXZVals[xzVals_?MatrixQ,theZArgs:{_Symbol..},theResult:{(_->_)..}]:=
Transpose[{Flatten[Join[xzVals,theZArgs]/.theResult]}]


(*end code for genFRFunc*)


(*begin code for genFPFunc*)
        
fixedPointLimit=30;
genFPFunc[theSolver:(({genFRFunc,opts:OptionsPattern[]}|{genNSFunc,opts:OptionsPattern[]}|{specialSolver,opts:OptionsPattern[]})),linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
           psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} ,XZFuncs:{(_Function|_InterpolatingFunction|_CompiledFunction),numSteps_Integer},eqnsFunc:(_Function|_CompiledFunction)]:=
With[{numX=getNumX[linMod],numEps=getNumEps[linMod],numZ=getNumZ[linMod]},
With[{funcArgs=Table[Unique["theFPFuncArgs"],{numX+numEps}]},
ReplacePart[
Function[xxxx,Sow[
myFixedPoint[Function[xx,With[{
xzFuncNow=theSolver[[1]][
{numX,numEps,numZ},
genLilXkZkFunc[linMod,XZFuncs,xx[[Range[numX]]]],
eqnsFunc,{opts}]},
Apply[xzFuncNow,funcArgs]]],(Apply[XZFuncs[[1]],funcArgs])[[Range[numX]]],
fixedPointLimit]]],
1->funcArgs]]]
(* input   [linMod,XZ, xguess,function (xt,eps,zt)->(xtm1,xt,xtp1,eps), function (xtm1,xt,xtp1,eps)->me]*)
(* output   [function  (xt,eps) ->(xt,zt)] *)


(*end code for genFPFunc*)


(*begin code for myFixedPoint*)

myFixedPoint[firstArg_,secondArg_,thirdArg_]:=
Module[{},
FixedPoint[firstArg,secondArg,thirdArg]]
        

(*end code for myFixedPoint*)


(*begin code for makeSmolyakInterpFunc*)


makeSmolyakInterpFunc[aVecFunc:(_Function|_CompiledFunction),smolGSpec:{smolToIgnore:{_Integer...},smolRngs:{{_?NumberQ,_?NumberQ}..},smolPts_?MatrixQ,smolMat_?MatrixQ,smolPolys_?VectorQ,smolIntPolys_?VectorQ,numEps_Integer}]:=
With[{interpData=smolyakGenInterpData[aVecFunc,smolGSpec],
numArgs=Length[smolPts[[1]]]},
(*Print["makeSmol:",interpData];*)
With[{numFuncs=Length[interpData],
funcArgs=Table[Unique["fArgs"],{numArgs}]},
With[{longFuncArgs=fillInSymb[{{},smolToIgnore,funcArgs}]},
With[{interpFuncList=
Map[Function[funcIdx,
With[{smolApp=smolyakInterpolation[
Map[Function[xx,xx[[funcIdx]]] , 
                interpData],smolGSpec]},
(*Print["smolApp=",smolApp];*)
smolApp]],
Range[numFuncs]]},
(*Print["interpFuncList",interpFuncList];*)
                With[{applied=Transpose[{Through[Apply[interpFuncList,funcArgs]]}]},
        ReplacePart[
        Function[xxxxxxx, applied],
                {1->longFuncArgs}]
        ]
]]]]



(*end code for makeSmolyakInterpFunc*)


(*begin code for parallelMakeInterpFunc*)

parallelMakeInterpFunc[aVecFunc:(_Function|_CompiledFunction),gSpec:{toIgnore:{_Integer...},iOrd_Integer,rngs:{{_Integer,_?NumberQ,_?NumberQ}..}}]:=
With[{interpData=parallelGenInterpData[aVecFunc,gSpec],numArgs=getNumVars[gSpec]},
With[{numFuncs=Length[interpData[[1,2]]],
funcArgs=Table[Unique["fArgs"],{numArgs}]},
With[{longFuncArgs=fillInSymb[{{},toIgnore,funcArgs}]},
With[{interpFuncList=
Map[Function[funcIdx,Interpolation[
Map[Function[xx,{xx[[1]], xx[[2, funcIdx, 1]]}] , 
                interpData],InterpolationOrder -> iOrd]],Range[numFuncs]]},
                With[{applied=Transpose[{Through[Apply[interpFuncList,funcArgs]]}]},
        ReplacePart[
        Function[xxxxxxx, applied],
                {1->longFuncArgs}]
        ]
]]]]




(*end code for parallelMakeInterpFunc*)


(*begin code for makeInterpFunc*)

makeInterpFunc[aVecFunc:(_Function|_CompiledFunction),gSpec:{toIgnore:{_Integer...},iOrd_Integer,rngs:{{_Integer,_?NumberQ,_?NumberQ}..}}]:=
With[{interpData=genInterpData[aVecFunc,gSpec],numArgs=getNumVars[gSpec]},
With[{numFuncs=Length[interpData[[1,2]]],
funcArgs=Table[Unique["fArgs"],{numArgs}]},
With[{longFuncArgs=fillInSymb[{{},toIgnore,funcArgs}]},
With[{interpFuncList=
Map[Function[funcIdx,Interpolation[
Map[Function[xx,{xx[[1]], xx[[2, funcIdx, 1]]}] , 
                interpData],InterpolationOrder -> iOrd]],Range[numFuncs]]},
                With[{applied=Transpose[{Through[Apply[interpFuncList,funcArgs]]}]},
        ReplacePart[
        Function[xxxxxxx, applied],
                {1->longFuncArgs}]
        ]
]]]]




(*end code for makeInterpFunc*)


(*begin code for parallelGenInterpData*)

 
parallelGenInterpData[aVecFunc:(_Function|_CompiledFunction),gSpec:{toIgnore:{_Integer...},iOrd_Integer,rngs:{{_Integer,_?NumberQ,_?NumberQ}..}}]:=
With[{thePts=gridPts[gSpec]},
With[{filledPts=ParallelMap[
Function[xx,fillIn[{{},toIgnore,xx}]],thePts]},
With[{theVals=ParallelMap[
Function[xx,(Apply[aVecFunc,xx])],filledPts]},
With[{interpData=Transpose[{thePts,theVals}]},
interpData]]]]





(*end code for parallelGenInterpData*)


(*begin code for genInterpData*)

 
genInterpData[aVecFunc:(_Function|_CompiledFunction),gSpec:{toIgnore:{_Integer...},iOrd_Integer,rngs:{{_Integer,_?NumberQ,_?NumberQ}..}}]:=
With[{thePts=gridPts[gSpec]},
With[{filledPts=Map[
Function[xx,fillIn[{{},toIgnore,xx}]],thePts]},
With[{theVals=Map[
Function[xx,(Apply[aVecFunc,xx])],filledPts]},
With[{interpData=Transpose[{thePts,theVals}]},
interpData]]]]





(*end code for genInterpData*)


(*begin code for gridPts*)
 
gridPts[gSpec:{toIgnore:{_Integer...},iOrd_Integer,rngs:{{_Integer,_?NumberQ,_?NumberQ}..}}]:=
With[{funcForPts=
Function[yy,
(Function[xx,oneDimGridPts[xx[[1]],xx[[{2,3}]]]][yy])]},
With[{oneDimPts=Map[funcForPts,rngs]},
With[{theOuter=Function[xx,Outer[List,Apply[Sequence,xx]]][oneDimPts]},
Flatten[theOuter,Depth[theOuter]-3]]]]



(*end code for gridPts*)


(*begin code for oneDimGridPts*)

oneDimGridPts[iPts_Integer,{xLow_?NumberQ,xHigh_?NumberQ}]:=
If[iPts==0,{{(xLow+xHigh)2}},
Table[ii,{ii,xLow,xHigh,N[xHigh-xLow]/iPts}]]/;iPts>=0


(*end code for oneDimGridPts*)


(*begin code for fillIn*)

fillIn[args___]:=Print["wrong args for fillIn",{args}];
fillIn[{theRes:{_?NumberQ...},toIgnore:{_Integer...},shortVec:{_?NumberQ...}}]:=
Module[{},
If[toIgnore=={}==shortVec,theRes,
If[MemberQ[toIgnore,
Length[theRes]+1],fillIn[{Append[theRes,1],Drop[toIgnore,1],shortVec}],
fillIn[{Append[theRes,shortVec[[1]]],toIgnore,Drop[shortVec,1]}]]]]/;OrderedQ[toIgnore]


(*end code for fillIn*)


(*begin code for fillInSymb*)

fillInSymb[{theRes:{___},toIgnore:{_Integer...},shortVec:{___}}]:=
Module[{},
If[toIgnore=={}==shortVec,theRes,
        If[MemberQ[toIgnore,Length[theRes]+1],fillInSymb[{Append[theRes,Unique["ig"]],Drop[toIgnore,1],shortVec}],
                fillInSymb[{Append[theRes,shortVec[[1]]],toIgnore,Drop[shortVec,1]}]]]]/;OrderedQ[toIgnore]

fillInSymb[{theRes:{___},toIgnore:{_Integer...},shortVec:{___}}]:=
fillInSymb[{theRes,Sort[toIgnore],shortVec}]

fillIn[{theRes:{_?NumberQ...},toIgnore:{_Integer...},shortVec:{_?NumberQ...}}]:=
fillIn[{theRes,Sort[toIgnore],shortVec}]


(*end code for fillInSymb*)


(*begin code for parallelNestIterREInterp*)


parallelNestIterREInterp[theSolver:(({genFRFunc,opts:OptionsPattern[]}|{genNSFunc,opts:OptionsPattern[]}|{specialSolver,opts:OptionsPattern[]})),linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
                          psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} ,
XZFuncs:{(_Function|_InterpolatingFunction|_CompiledFunction),numSteps_Integer},eqnsFunc:(_Function|_CompiledFunction),
gSpec:{toIgnore:{_Integer...},iOrd_Integer,rngs:{{_Integer,_?NumberQ,_?NumberQ}..}},
distribSpec:{expctSpec:{{_Symbol,_}..}},numIters_Integer]:=
NestList[Function[xx,parallelDoIterREInterp[theSolver,linMod,
{xx[[2]],numSteps},eqnsFunc,gSpec,distribSpec]],{ig,XZFuncs[[1]]},numIters]




(*end code for parallelNestIterREInterp*)
 

(*begin code for parallelDoIterREInterp*)
parallelDoIterREInterp[theSolver:(({genFRFunc,opts:OptionsPattern[]}|{genNSFunc,opts:OptionsPattern[]}|{specialSolver,opts:OptionsPattern[]})),
        linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
        psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} ,
        XZFuncs:{(_Function|_InterpolatingFunction|_CompiledFunction),numSteps_Integer},
eqnsFunc:(_Function|_CompiledFunction),gSpec:{toIgnore:{_Integer...},iOrd_Integer,rngs:{{_Integer,_?NumberQ,_?NumberQ}..}},distribSpec:{expctSpec:{{_Symbol,_}..}}]:=
With[{numX=Length[BB],numEps=Length[psiEps[[1]]],numZ=Length[psiZ[[1]]]},
tn=AbsoluteTime[];
DistributeDefinitions[XZFuncs[[1]]];
DistributeDefinitions[eqnsFunc];
DistributeDefinitions[linMod];
With[{theFuncs=parallelMakeInterpFunc[genFPFunc[theSolver,linMod,XZFuncs,eqnsFunc],gSpec]},
Print["parallelMakeInterpTime=",(tn2=AbsoluteTime[])-tn];
With[{XZRE=parallelGenXZREInterpFunc[{numX,numEps,numZ},theFuncs,gSpec,distribSpec]},
Print["parallelgenXZREInterpTime=",(AbsoluteTime[])-tn2];
{theFuncs,XZRE}]]]




(*end code for parallelDoIterREInterp*)
 

(*begin code for nestIterREInterp*)


nestIterREInterp[theSolver:(({genFRFunc,opts:OptionsPattern[]}|{genNSFunc,opts:OptionsPattern[]}|{specialSolver,opts:OptionsPattern[]})),linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
                  psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} ,
XZFuncs:{(_Function|_InterpolatingFunction|_CompiledFunction),numSteps_Integer},eqnsFunc:(_Function|_CompiledFunction),
gSpec:{toIgnore:{_Integer...},iOrd_Integer,rngs:{{_Integer,_?NumberQ,_?NumberQ}..}},
distribSpec:{expctSpec:{{_Symbol,_}..}},numIters_Integer]:=
NestList[Function[xx,doIterREInterp[theSolver,linMod,
{xx[[2]],numSteps},eqnsFunc,gSpec,distribSpec]],{ig,XZFuncs[[1]]},numIters]




(*end code for nestIterREInterp*)


(*begin code for doIterREInterp*)
doIterREInterp[theSolver:(({genFRFunc,opts:OptionsPattern[]}|{genNSFunc,opts:OptionsPattern[]}|{specialSolver,opts:OptionsPattern[]})),
        linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
        psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} ,
        XZFuncs:{(_Function|_InterpolatingFunction|_CompiledFunction),numSteps_Integer},
eqnsFunc:(_Function|_CompiledFunction),gSpec:{toIgnore:{_Integer...},iOrd_Integer,rngs:{{_Integer,_?NumberQ,_?NumberQ}..}},distribSpec:{expctSpec:{{_Symbol,_}..}}]:=
With[{numX=Length[BB],numEps=Length[psiEps[[1]]],numZ=Length[psiZ[[1]]]},
tn=AbsoluteTime[];
With[{theFuncs=makeInterpFunc[genFPFunc[theSolver,linMod,XZFuncs,eqnsFunc],gSpec]},
Print["makeInterpTime=",(tn2=AbsoluteTime[])-tn];
With[{XZRE=genXZREInterpFunc[{numX,numEps,numZ},theFuncs,gSpec,distribSpec]},
Print["genXZREInterpTime=",(AbsoluteTime[])-tn2];
{theFuncs,XZRE}]]]




(*end code for doIterREInterp*)


(*begin code for nestSmolyakIterREInterp*)


nestSmolyakIterREInterp[theSolver:(({genFRFunc,opts:OptionsPattern[]}|{genNSFunc,opts:OptionsPattern[]}|{specialSolver,opts:OptionsPattern[]})),linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
                         psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} ,
XZFuncs:{(_Function|_InterpolatingFunction|_CompiledFunction),numSteps_Integer},eqnsFunc:(_Function|_CompiledFunction),
gSpec:{toIgnore:{_Integer...},iOrd_Integer,rngs:{{_Integer,_?NumberQ,_?NumberQ}..}},
distribSpec:{expctSpec:{{_Symbol,_}..}},numIters_Integer]:=
NestList[Function[xx,doIterREInterp[theSolver,linMod,
{xx[[2]],numSteps},eqnsFunc,gSpec,distribSpec]],{ig,XZFuncs[[1]]},numIters]




(*end code for nestSmolyakIterREInterp*)


(*begin code for doSmolyakIterREInterp*)
doSmolyakIterREInterp[theSolver:(({genFRFunc,opts:OptionsPattern[]}|{genNSFunc,opts:OptionsPattern[]}|{specialSolver,opts:OptionsPattern[]})),
        linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
        psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} ,
        XZFuncs:{(_Function|_InterpolatingFunction|_CompiledFunction),numSteps_Integer},
eqnsFunc:(_Function|_CompiledFunction),gSpec:{toIgnore:{_Integer...},iOrd_Integer,rngs:{{_Integer,_?NumberQ,_?NumberQ}..}},smolGSpec:{smolToIgnore:{_Integer...},smolRngs:{{_?NumberQ,_?NumberQ}..},smolPts_?MatrixQ,smolMat_?MatrixQ,smolPolys_?VectorQ,smolIntPolys_?VectorQ,numEps_Integer},distribSpec:{expctSpec:{{_Symbol,_}..}}]:=
With[{numX=Length[BB],numEps=Length[psiEps[[1]]],numZ=Length[psiZ[[1]]]},
tn=AbsoluteTime[];
With[{theFuncs=makeSmolyakInterpFunc[genFPFunc[theSolver,linMod,XZFuncs,eqnsFunc],smolGSpec]},
Print["makeSmolyakInterpTime=",(tn2=AbsoluteTime[])-tn];
With[{XZRE=genXZREInterpFunc[{numX,numEps,numZ},theFuncs(*,smolToIgnore,smolRngs,smolPts,smolMat,smolPolys*),gSpec,distribSpec]},
Print["genXZREInterpTime=",(AbsoluteTime[])-tn2];
{theFuncs,XZRE}]]]




(*end code for doSmolyakIterREInterp*)


(*begin code for parallelGenXZREInterpFunc*)
 
parallelGenXZREInterpFunc[probDims:{numX_Integer,numEps_Integer,numZ_Integer},
aLilXkZkFunc_Function,gSpec:{toIgnore:{_Integer...},iOrd_Integer,rngs:{{_Integer,_?NumberQ,_?NumberQ}..}},distribSpec:{expctSpec:{{_Symbol,_}..}}]:=
With[{theFuncNow=genXZFuncRE[{numX,numEps,numZ},aLilXkZkFunc,distribSpec]},
parallelMakeInterpFunc[theFuncNow,elimGSpecShocks[gSpec,numEps]]]
  
elimGSpecShocks[gSpec:{toIgnore:{_Integer...},iOrd_Integer,rngs:{{_Integer,_?NumberQ,_?NumberQ}..}},numEps_Integer]:=
{toIgnore,gSpec[[2]],Drop[getGridPtTrips[gSpec],-(numEps)]}

(*end code for parallelGenXZREInterpFunc*)


(*begin code for genSmolyakXZREInterpFunc*)
 
genSmolyakXZREInterpFunc[probDims:{numX_Integer,numEps_Integer,numZ_Integer},
aLilXkZkFunc_Function,toIgnore:{_Integer...},ranges_?MatrixQ,smolPts_?MatrixQ,smolMat_?MatrixQ,smolPolys_?VectorQ,gSpec:{toIgnore:{_Integer...},iOrd_Integer,rngs:{{_Integer,_?NumberQ,_?NumberQ}..}},distribSpec:{expctSpec:{{_Symbol,_}..}}]:=
With[{theFuncNow=genXZFuncRE[{numX,numEps,numZ},aLilXkZkFunc,distribSpec]},
Print["genSmol:",InputForm[theFuncNow]];
makeInterpFunc[theFuncNow,elimGSpecShocks[gSpec,numEps]]]
  
elimGSpecShocks[gSpec:{toIgnore:{_Integer...},iOrd_Integer,rngs:{{_Integer,_?NumberQ,_?NumberQ}..}},numEps_Integer]:=
{toIgnore,gSpec[[2]],Drop[getGridPtTrips[gSpec],-(numEps)]}

(*end code for genSmolyakXZREInterpFunc*)


(*begin code for genXZREInterpFunc*)
 
genXZREInterpFunc[probDims:{numX_Integer,numEps_Integer,numZ_Integer},
aLilXkZkFunc_Function,gSpec:{toIgnore:{_Integer...},iOrd_Integer,rngs:{{_Integer,_?NumberQ,_?NumberQ}..}},distribSpec:{expctSpec:{{_Symbol,_}..}}]:=
With[{theFuncNow=genXZFuncRE[{numX,numEps,numZ},aLilXkZkFunc,distribSpec]},
makeInterpFunc[theFuncNow,elimGSpecShocks[gSpec,numEps]]]
  
elimGSpecShocks[gSpec:{toIgnore:{_Integer...},iOrd_Integer,rngs:{{_Integer,_?NumberQ,_?NumberQ}..}},numEps_Integer]:=
{toIgnore,gSpec[[2]],Drop[getGridPtTrips[gSpec],-(numEps)]}

(*end code for genXZREInterpFunc*)


(*begin code for genXZFuncRE*)



genXZFuncRE[{numX_Integer,ignored_Integer,numZ_Integer},
aLilXkZkFunc_Function,distribSpec:{expctSpec:{{_Symbol,_}..}}]:=
With[{intVarRes=genIntVars[numX,distribSpec],
funcName=Unique["fName"]},
funcName[fNameArgs:{_?NumberQ..},idx_Integer]:=Module[{},
(Apply[aLilXkZkFunc,fNameArgs])[[idx,1]]];
With[{funcGuts=
Function[xxxx,Module[{},
Transpose[{Map[Function[xx,myNExpectation[
        (funcName[intVarRes[[2]],xx]),intVarRes[[3]]]],Range[numX+numZ]]}]]]},
        ReplacePart[funcGuts,1->intVarRes[[1]]]]]



(*end code for genXZFuncRE*)


(*begin code for genIntVars*)
 genIntVars[numX_Integer,distribSpec:{expctSpec:{{_Symbol,_}..}}]:=
With[{xVars=Table[Unique["xV"],{numX}],
        dists=getDistribs[distribSpec],
        distVars=Table[Unique["epIntV"],{getNumEpsVars[distribSpec]}]},
With[{xEpsVars=Join[xVars,distVars],
        intArg=
MapThread[Function[{xx,yy},xx \[Distributed] yy],{distVars,dists}]},
        {xVars,xEpsVars,intArg}]]


(*end code for genIntVars*)


End[]
EndPackage[]



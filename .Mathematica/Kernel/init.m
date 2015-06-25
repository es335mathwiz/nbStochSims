(** User Mathematica initialization file **)
SetOptions[Trace,TraceForward->True]


windowsQ[]:=Not[StringPosition[$Version,"Windows"]==={}];
If[windowsQ[],Print["windows environment: executing commands in init.m"<>FindFile["init.m"]],
Print["linux environment: executing commands in init.m  "<>FindFile["init.m"]]]

Needs["JLink`"]
If[windowsQ[],
ReinstallJava[CommandLine->"C:/Program Files/Java/jdk1.7.0_60/bin/java  -showversion"],
ReinstallJava[CommandLine->"/msu/scratch2/m1gsa00/jdk1.7.0/bin/java  -showversion"]]
Off[Syntax::sntufn];
Needs["console`"]
On[Syntax::sntufn];
(*

When you first install Mathematica it's configured so that when you start up the program, the Welcome screen comes up as well as the Documentation Center dialog. Having to close both of these windows every time Mathematica starts is annoying. You can prevent these windows from opening by adding the following two lines to the Mathematica initialization file:


*)

(*
If[$FrontEnd=!=Null,
SetOptions[$FrontEnd, DockedCells->{}];
SetOptions[$FrontEnd, AutoOpenNotebooks->{}]]
*)



If[windowsQ[],
$pertPath="G:/scratch/tryBenchWindows3.5/Perturbation/";
$umbraPath="C:/gitHome/git/forUmbraCalculus/umbraCalculus/umbraCalculus/";
$AMAPath="G:/RES2/mathAMA/AndersonMooreAlgorithm/";
$FormatPath="S:/learnProjection/proto/";
$projPath="G:/RES2/ProjectionMethodTools/projection/";
$projCPath={"G:/RES2/ProjectionMethodTools/projection/target/test-classes", 
"G:/RES2/ProjectionMethodTools/projection/target/classes", 
"r:/Software/mavenRepositories/tryRep/gov/frb/ma/msu/commons-math/2.2-SNAPSHOT/commons-math-2.2-SNAPSHOT.jar", 
"r:/Software/mavenRepositories/tryRep/gov/frb/ma/msu/Jama-1.0.2/1.0-SNAPSHOT/Jama-1.0.2-1.0-SNAPSHOT.jar",
"r:/Software/mavenRepositories/tryRep/gov/frb/ma/msu/projection/1.0-SNAPSHOT/projection-1.0-SNAPSHOT.jar"},
   $pertPath="/msu/home/m1gsa00/scratch/tryBenchWindows3.5/Perturbation/";
$umbraPath="/msu/home/m1gsa00/git/umbralCalculus/umbralCalculus";
   $eigProjPath="/msu/res2/m1gsa00/dynareAntlr/";
$eigProjCPath={"/msu/res2/m1gsa00/dynareAntlr/target/test-classes", 
"/msu/res2/m1gsa00/dynareAntlr/target/classes", 
"/msu/res1/Software/mavenRepositories/tryRep/gov/frb/ma/msu/commons-math/2.2-SNAPSHOT/commons-math-2.2-SNAPSHOT.jar", 
"/msu/res1/Software/mavenRepositories/tryRep/gov/frb/ma/msu/Jama-1.0.2/1.0-SNAPSHOT/Jama-1.0.2-1.0-SNAPSHOT.jar",
"/msu/res1/Software/mavenRepositories/tryRep/gov/frb/ma/msu/commons-math/2.2-SNAPSHOT/commons-math-2.2-SNAPSHOT.jar"};
$AMAPath="/msu/home/m1gsa00/RES2/mathAMA/AndersonMooreAlgorithm/";
$FormatPath="/msu/scratch/m1gsa00/learnProjection/proto/";
$projPath="/msu/home/m1gsa00/RES2/ProjectionMethodTools/projection/";
$projCPath={"/msu/home/m1gsa00/RES2/ProjectionMethodTools/projection/target/test-classes", 
"/msu/home/m1gsa00/RES2/ProjectionMethodTools/projection/target/classes", 
"/msu/res1/Software/mavenRepositories/tryRep/gov/frb/ma/msu/commons-math/2.2-SNAPSHOT/commons-math-2.2-SNAPSHOT.jar", 
"/msu/res1/Software/mavenRepositories/tryRep/gov/frb/ma/msu/Jama-1.0.2/1.0-SNAPSHOT/Jama-1.0.2-1.0-SNAPSHOT.jar",
"/msu/res1/Software/mavenRepositories/tryRep/gov/frb/ma/msu/projection/1.0-SNAPSHOT/projection-1.0-SNAPSHOT.jar"};
$eigProjPath="/msu/res2/m1gsa00/dynareAntlr/";
$eigProjCPath={"/msu/res2/m1gsa00/dynareAntlr/target/test-classes", 
"/msu/res2/m1gsa00/dynareAntlr/target/classes", 
"/msu/res1/Software/mavenRepositories/tryRep/gov/frb/ma/msu/commons-math/2.2-SNAPSHOT/commons-math-2.2-SNAPSHOT.jar", 
"/msu/res1/Software/mavenRepositories/tryRep/gov/frb/ma/msu/Jama-1.0.2/1.0-SNAPSHOT/Jama-1.0.2-1.0-SNAPSHOT.jar",
"/msu/res1/Software/mavenRepositories/tryRep/gov/frb/ma/msu/dynareAntlr/1.0-SNAPSHOT/dynareAntlr-1.0-SNAPSHOT.jar",
"/msu/home/m1gsa00/scratch/tryRep/org/antlr/antlr-runtime/3.2/antlr-runtime-3.2.jar"};
]






(*

(*prep for 2011 sce presentation calcs*)
prepSCE2011[]:=
Module[{},
$Path=PrependTo[$Path,$pertPath];
$Path=PrependTo[$Path,$umbraPath];
$Path=PrependTo[$Path,$AMAPath];
$Path=PrependTo[$Path,$projPath];
$Path=PrependTo[$Path,$eigProjPath];
$Path=PrependTo[$Path,$FormatPath];
<<SymbolicAMA`;
  <<NumericAMA`;
  <<AndersonMooreAlgorithm`AMAModel`;
If[Not[StringQ[$installDir]], 
$installDir = 
   StringReplace[FindFile["AndersonMooreAlgorithm`AMAModel`"], 
     "AMAModel.m" -> ""]];(*
SetDirectory[StringReplace[FindFile["paper.mth"],"paper.mth" -> ""]];*)
(*SetDirectory["dynareExamples/examples"];*)
Needs["SSProjection`"];
Needs["RootProjection`"];
Needs["EigenspaceProjection`"];
Needs["Format`"];
Needs["Experimental`"];
ReinstallJava[JVMArguments -> "-Xmx1080m"];
AddToClassPath[
"/msu/res2/m1gsa00/sce11/",
  "/msu/res1/Software/mavenRepositories/tryRep/gov/frb/ma/msu/projection/1.0-SNAPSHOT/projection-1.0-SNAPSHOT.jar",   "/msu/res1/Software/mavenRepositories/tryRep/gov/frb/ma/msu/commons-math/1.0-SNAPSHOT/commons-math-1.0-SNAPSHOT.jar",   "/msu/res1/Software/mavenRepositories/tryRep/gov/frb/ma/msu/Jama-1.0.2/1.0-SNAPSHOT/Jama-1.0.2-1.0-SNAPSHOT.jar"];
AddToClassPath/@$projCPath;
AddToClassPath/@$eigProjCPath;
]

(*prep for dynare calculations*)
prepDynare[]:=
Module[{},
$Path=PrependTo[$Path,$AMAPath];
$Path=PrependTo[$Path,$projPath];
$Path=PrependTo[$Path,$eigProjPath];
$Path=PrependTo[$Path,$FormatPath];
<< AndersonMooreAlgorithm`SymbolicAMA`;
  << AndersonMooreAlgorithm`NumericAMA`;
  << AndersonMooreAlgorithm`AMAModel`;
If[Not[StringQ[$installDir]], 
$installDir = 
   StringReplace[FindFile["AndersonMooreAlgorithm`AMAModel`"], 
     "AMAModel.m" -> ""]];(*
(*SetDirectory[StringReplace[FindFile["paper.mth"],"paper.mth" -> ""]];*)
SetDirectory["dynareExamples/examples"];*)
Needs["RootProjection`"];
Needs["EigenspaceProjection`"];
Needs["Format`"];
Needs["Experimental`"];
ReinstallJava[JVMArguments -> "-Xmx1080m"];
AddToClassPath/@$projCPath;
AddToClassPath/@$eigProjCPath;
]

computeLSFullSymbolic[]:=
Module[{},
$Path=PrependTo[$Path,"/msu/home/m1gsa00/RES2/sce11/"];
Get["computeLSFullSymbolic.mth"];
]

prepForLSProjection[]:=
Module[{},
$Path=PrependTo[$Path,"/msu/home/m1gsa00/RES2/sce11/"];
Get["prepForLSProjection.mth"]]




(*prep for 2011 sce presentation calcs*)
prepSCE2011[]:=
Module[{},
$Path=PrependTo[$Path,$pertPath];
$Path=PrependTo[$Path,$umbraPath];
$Path=PrependTo[$Path,$AMAPath];
$Path=PrependTo[$Path,$projPath];
(*
$Path=PrependTo[$Path,$eigProjPath];
$Path=PrependTo[$Path,$FormatPath];
<<SymbolicAMA`;
  <<NumericAMA`;
  <<AndersonMooreAlgorithm`AMAModel`;
If[Not[StringQ[$installDir]], 
$installDir = 
   StringReplace[FindFile["AndersonMooreAlgorithm`AMAModel`"], 
     "AMAModel.m" -> ""]];(*
SetDirectory[StringReplace[FindFile["paper.mth"],"paper.mth" -> ""]];*)
(*SetDirectory["dynareExamples/examples"];*)
Needs["SSProjection`"];
Needs["RootProjection`"];
Needs["EigenspaceProjection`"];
Needs["Format`"];
Needs["Experimental`"];
ReinstallJava[JVMArguments -> "-Xmx1080m"];
AddToClassPath[
"/msu/res2/m1gsa00/sce11/",
  "/msu/res1/Software/mavenRepositories/tryRep/gov/frb/ma/msu/projection/1.0-SNAPSHOT/projection-1.0-SNAPSHOT.jar",   "/msu/res1/Software/mavenRepositories/tryRep/gov/frb/ma/msu/commons-math/1.0-SNAPSHOT/commons-math-1.0-SNAPSHOT.jar",   "/msu/res1/Software/mavenRepositories/tryRep/gov/frb/ma/msu/Jama-1.0.2/1.0-SNAPSHOT/Jama-1.0.2-1.0-SNAPSHOT.jar"];
AddToClassPath/@$projCPath;
AddToClassPath/@$eigProjCPath;
*)
]


(*prep for 2013 sce presentation calcs*)

$gitDir=If[windowsQ[],
"g:/git/","/msu/home/m1gsa00/git/"];

$pertPath=$gitDir<>"MatPert/MatPert";
$umbraPath=$gitDir<>"umbralCalculus/umbralCalculus";
$impPath=$gitDir<>"ImpulseResponseErgodic/ImpulseResponseErgodic";
$nAMAPath=$gitDir<>"mathAMA/NumericAMA/NumericAMA";
$sAMAPath=$gitDir<>"mathAMA/SymbolicAMA/SymbolicAMA";
$oldPertPath=$gitDir<>"mathAMA/Perturbation/Perturbation";
$projPath=$gitDir<>"ProjectionMethodTools/ProjectionMethodTools";

prepSCE2013[]:=
Module[{},
$Path=PrependTo[$Path,$pertPath];
$Path=PrependTo[$Path,$umbraPath];
$Path=PrependTo[$Path,$impPath];
$Path=PrependTo[$Path,$nAMAPath];
$Path=PrependTo[$Path,$sAMAPath];
$Path=PrependTo[$Path,$oldPertPath];
$Path=PrependTo[$Path,$projPath];
Needs["ImpulseResponseErgodic`"];
Needs["ProjectionMethodTools`"]; pre = "gov.frb.ma.msu."; InstallJava[];
Get["neoKeynesianEqnsSubs.mth"]
AddToClassPath[
"/msu/res2/m1gsa00/sce13/",
  "/msu/res1/Software/mavenRepositories/tryRep/gov/frb/ma/msu/ProjectionMethodToolsJava/0.0.1-SNAPSHOT/ProjectionMethodToolsJava-0.0.1-SNAPSHOT.jar",   "/msu/res1/Software/mavenRepositories/tryRep/gov/frb/ma/msu/commons-math/1.0-SNAPSHOT/commons-math-1.0-SNAPSHOT.jar",   "/msu/res1/Software/mavenRepositories/tryRep/math/nist/gov/Jama/1.0.3/Jama-1.0.3.jar"];
]
*)

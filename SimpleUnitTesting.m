(* ::Package:: *)

(* ::Title:: *)
(*Simple Unit Testing*)


(* ::Subsubtitle:: *)
(*v14.05.17 (latest review by maintainer)*)


(* ::Text:: *)
(*Akater*)
(*nuclearspace@gmail.com*)


(* ::Text:: *)
(*No licensing. Use the contents in any way you want to. WTFPL*)


(* ::Subsection:: *)
(*Overview*)


(* ::Text:: *)
(*Create SimpleUnitTestingTesting directory in Applications, put SuperCalculations.mt file into Tests subdirectory, and run TestMyApp@"SuperCalculations". Or jut execute QuickExampleInstall and go to Examples section.*)
(**)
(*Test files were meant to be compatible with Workbench .mt ones.*)


(* ::Subsection:: *)
(*Credits*)


(* ::Text:: *)
(*No contributors but the maintainer. This package would never see the light of the day if not for all the wonderful people at and behind mma.sx*)


(* ::Text:: *)
(*Write me if you like this. I'm not git-literate.*)


(* ::Section:: *)
(*Initialization*)


(* ::Subsection::Closed:: *)
(*Dependencies*)


(* ::Text:: *)
(*Definitions in this section are non-essential for understanding of the main functionality but are essential for it to work properly. They could be placed in a separate package, or a set of packages (and probably should be, unless the users and developers want to avoid lots of interdependent layers in the packages system). They should not be discussed and updated in the user domain of this Pluck. Therefore, they are provided here without examples, and outside of the package context. Hence, BeginPackage includes the call to current Context.*)


iterateOverList[f_, {relativePredicate_, absolutePredicate_}, list_List] :=
Fold[
  With[{oldValue = #1[[2]]},
  With[{newValue = absolutePredicate[oldValue, f@#2]},
  If[relativePredicate[oldValue, newValue], {#2, newValue}, #1]]]&
, {Null, absolutePredicate[]}
, list]


maximizeOverListIteratively[f_, list_List] :=
iterateOverList[f, {Less, Max}, list]


stringOfMaxLength =
Composition[
  Part[#, 1]&
, maximizeOverListIteratively[StringLength, #]&];


gatherByInOpeners[list_List, f_, sortedListsWrapper_:Identity] :=
SortBy[
  OpenerView@{f@First@#, sortedListsWrapper@#}& /@
  GatherBy[list, f]
, #[[1, 1]]&] //
Column


someLongReportOpener[expr_, header:_String:"Report"] :=
OpenerView @
{ Row@{Spacer@2, Text@Style[header, Bold, 18]}
, expr }


(* ::Subsection:: *)
(*Public Context*)


BeginPackage["SimpleUnitTesting`", {Context[]}];


(* ::Subsection:: *)
(*Documentation*)


Test::usage = "Tests container. I am surprised this one was not defined initially.";


TestID::usage = "TestID container. I am surprised this one was not defined, too. Am I missing something?";


$CurrentApp::usage = "";


TestMyApp::usage = "TestMyApp[\"\!\(\*
StyleBox[\"file\",\nFontSlant->\"Italic\"]\)\"] tests \[OpenCurlyDoubleQuote]\!\(\*
StyleBox[\"file\",\nFontSlant->\"Italic\"]\).mt\[CloseCurlyDoubleQuote] in Tests subdirectory of $CurrentApp, pretty printing the result.";


QuickExampleInstall::usage = "Installs toy example to run.";


(* ::Subsection::Closed:: *)
(*Private Context*)


Begin["`Private`"];


(* ::Section:: *)
(*Implementation*)


(* ::Subsubsection:: *)
(*TestCheck, UnitTestOpener*)


$CurrentApp = "SimpleUnitTestingTesting";


SetAttributes[Test, HoldAll];


SetAttributes[expected, HoldFirst];


TestCheck@Test[expr_, result_, TestID -> id_]:=
With[{exprEvaluated = expr},
"Test Result"[TestID@id
, "Passed"@Or[exprEvaluated === result, False@expected@result]
, Test@expr
, "Evaluated Test Expression"@exprEvaluated]]


Options@UnitTestOpener = {"Always show originals" -> False};


UnitTestOpener[tr:"Test Result"@__, longestTestIDInARow_:"", opts:OptionsPattern[]] :=
With[
{ expected =
  Quiet @
  Replace[
    Fold[Composition[First, Cases[#1, #2, {1}, 1]&]
    , #, {p:Blank@"Passed" :> p, f_False :> f, expected@val_ :> HoldForm@val}]
  , _First -> None, {0}]& @
  tr
, testID = First@Cases[tr, TestID@x_ :> x, {1}, 1]
, evaluatedTestExpression =
  First@Cases[tr, "Evaluated Test Expression"@x_ :> x, {1}, 1]
, passedQ = TrueQ@First@Cases[tr, "Passed"@x_ :> x, {1}, 1]},
OpenerView[#, !passedQ]& @
{ Framed[
    Overlay[
      Style[#, Bold]& /@
      { testID
      , Invisible@longestTestIDInARow }]
  , Background -> If[passedQ, Darker@Green, Lighter@Red]
  , FrameStyle -> None]
, { {   "Tested:" , First@Cases[tr, Test@x_ :> HoldForm@x, {1}, 1] }
  , { "Expected:" , If[passedQ
                    ,  evaluatedTestExpression
                    ,  ReleaseHold@expected] }
  , If[(!passedQ && HoldForm@Evaluate@ReleaseHold@expected =!= expected)|| OptionValue@"Always show originals"
    ,  { "Written:" , expected }
    ,  ##&[]] }
  ~Join~
  If[passedQ
  ,  {}
  ,  {{"Evaluated:", evaluatedTestExpression}}] //
  Grid[#, Alignment -> {{Right, Left}}]& }]


TestCheck[tl:{__Test}] :=
gatherByInOpeners[
  TestCheck /@ tl
, If[MemberQ[#, "Passed"@False@_]
  ,  "Failed Tests"
  ,  "Passed Tests"]&
, With[
  { longestTestID =
    stringOfMaxLength @
      Flatten[
        StringSplit[#, "\n"]& /@
        Identity @@@ (First@Cases[#, t_TestID :> t, {1}, 1]& /@ #)]}
, Column[UnitTestOpener[#, longestTestID]& /@ #]]&]


(* ::Subsubsection:: *)
(*UnitTestReport*)


UnitTestReport[tl:{__Test}] :=
Deploy@someLongReportOpener[TestCheck@tl, "Unit Test Report"]


UnitTestReport@_ := "Not a List of Test's."


(* ::Subsubsection:: *)
(*RunUnitTest*)


RunUnitTest@file_String :=
If[FileExistsQ@file
,  UnitTestReport@Import[file, "Package"]
,  OpenerView @
   { "No test file."
   , StringJoin["This is RunUnitTest. I could not perform the test because I could not find the following test file: "
     , file
     , "\nCould be a human mistake, but, more probably, the file name was provided by some other expression, and I'm still a bit dumb and can't tell you what was the expression, exactly."]}]


appDir :=
FileNameJoin @
{ $UserBaseDirectory, "Applications", $CurrentApp }


TestMyApp@testName_String :=
With[{ testFileName =
       FileNameJoin @
       { appDir, "Tests"
       , testName <> ".mt"}}
, If[FileExistsQ@#
  ,  RunUnitTest@#
  ,  "Error"@Row[{"No test file:", #}, "  "]]&@testFileName]


QuickExampleInstall := {
Quiet@DeleteDirectory[appDir, DeleteContents -> True],
Quiet@CreateDirectory@appDir,
Quiet@CreateDirectory@FileNameJoin@{appDir, "Tests"},
Export[
  FileNameJoin @
  { appDir, "Tests"
  , "SuperCalculations" <> ".mt"},
With[{var = ToExpression@"x"},
Unevaluated @
{

Test[Plus @@ Prime /@ Range@2
, 2+3
, TestID -> "Sum of the first two primes"],

Test[Plus @@ Prime /@ Range@3
, 2+35
, TestID -> "Sum of the first three primes"],

Test[Sqrt[var^2]
, PlusMinus@var
, TestID -> "Some serious square root"]

}], "Package"],
"",
"No error checking implemented here (yet?). Hopefully, all went fine."} // Column


(* ::Section::Closed:: *)
(*Finalization*)


End[]; (* `Private` *)


EndPackage[]; (* SimpleUnitTesting`` *)

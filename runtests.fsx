
#r "bin/FsLexYacc.Runtime.dll";;
#load "Environ.fs";;
#load "Absyn.fs";;
#load "PlcParserAux.fs";;
#load "PlcParser.fs";;
#load "PlcLexer.fs";;
#load "PlcInterp.fs";;
#load "PlcChecker.fs";;
#load "Parse.fs";;
#load "TestAux.fs";;

open Absyn
open TestAux
let fromString = Parse.fromString // string parser function
let run e = printfn "\nThe output is:     %s\n" (Plc.run e)


#load "Test.fs"
#load "/home/hbarbosa/Downloads/Test.fs"

testAll Test.cases

let checkerTest e = PlcChecker.teval e []
let interpreterTest e = PlcInterp.eval e []

//Tests for checker and interpreter

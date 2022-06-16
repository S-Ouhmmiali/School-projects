
open Compilateur

(* Changer le chemin d'accès du jar. *)
let runtamcmde = "java -jar ../../runtam.jar"
(* let runtamcmde = "java -jar /mnt/n7fs/.../tools/runtam/runtam.jar" *)

(* Execute the TAM code obtained from the rat file and return the ouptut of this code *)
let runtamcode cmde ratfile =
  let tamcode = compiler ratfile in
  (* let (tamfile, chan) = Filename.open_temp_file "test" ".tam" in*)
  let tamfile = Filename.chop_extension ratfile ^ ".tam" in
  let chan = open_out tamfile in
  output_string chan tamcode;
  close_out chan;
  let ic = Unix.open_process_in (cmde ^ " " ^ tamfile) in
  let printed = input_line ic in
  close_in ic;
  (*Sys.remove tamfile;    (* à commenter si on veut étudier le code TAM. *)*)
  String.trim printed

(* Compile and run ratfile, then print its output *)
let runtam ratfile =
  print_string (runtamcode runtamcmde ratfile)

(* requires ppx_expect in jbuild, and `opam install ppx_expect` *)
let%expect_test "testprintint" =
  runtam "../../fichiersRat/src-rat-tam-test/testprintint.rat";
  [%expect{| 42 |}]

let%expect_test "testprintbool" =
  runtam "../../fichiersRat/src-rat-tam-test/testprintbool.rat";
  [%expect{| true |}]

let%expect_test "testprintrat" =
   runtam "../../fichiersRat/src-rat-tam-test/testprintrat.rat";
   [%expect{| [4/5] |}]

let%expect_test "testaddint" =
  runtam "../../fichiersRat/src-rat-tam-test/testaddint.rat";
  [%expect{| 42 |}]

let%expect_test "testaddrat" =
  runtam "../../fichiersRat/src-rat-tam-test/testaddrat.rat";
  [%expect{| [7/6] |}]

let%expect_test "testmultint" =
  runtam "../../fichiersRat/src-rat-tam-test/testmultint.rat";
  [%expect{| 440 |}]

let%expect_test "testmultrat" =
  runtam "../../fichiersRat/src-rat-tam-test/testmultrat.rat";
  [%expect{| [14/3] |}]

let%expect_test "testnum" =
  runtam "../../fichiersRat/src-rat-tam-test/testnum.rat";
  [%expect{| 4 |}]

let%expect_test "testdenom" =
  runtam "../../fichiersRat/src-rat-tam-test/testdenom.rat";
  [%expect{| 7 |}]

let%expect_test "testwhile1" =
  runtam "../../fichiersRat/src-rat-tam-test/testwhile1.rat";
  [%expect{| 19 |}]

let%expect_test "testif1" =
  runtam "../../fichiersRat/src-rat-tam-test/testif1.rat";
  [%expect{| 18 |}]

let%expect_test "testif2" =
  runtam "../../fichiersRat/src-rat-tam-test/testif2.rat";
  [%expect{| 21 |}]

let%expect_test "factiter" =
  runtam "../../fichiersRat/src-rat-tam-test/factiter.rat";
  [%expect{| 120 |}]

let%expect_test "complique" =
  runtam "../../fichiersRat/src-rat-tam-test/complique.rat";
  [%expect{| [9/4][27/14][27/16][3/2] |}]

let%expect_test "factfun1" =
  runtam "../../fichiersRat/src-rat-tam-test/testfun1.rat";
  [%expect{| 1 |}]

let%expect_test "factfun2" =
  runtam "../../fichiersRat/src-rat-tam-test/testfun2.rat";
  [%expect{| 7 |}]

let%expect_test "factfun3" =
  runtam "../../fichiersRat/src-rat-tam-test/testfun3.rat";
  [%expect{| 10 |}]

let%expect_test "factfun4" =
  runtam "../../fichiersRat/src-rat-tam-test/testfun4.rat";
  [%expect{| 10 |}]

(*let%expect_test "factfun5" =
  runtam "../../fichiersRat/src-rat-tam-test/testfun5.rat";
  [%expect{| |}] *)

let%expect_test "factfun6" =
  runtam "../../fichiersRat/src-rat-tam-test/testfun6.rat";
  [%expect{|truetrue|}]

let%expect_test "factfuns" =
  runtam "../../fichiersRat/src-rat-tam-test/testfuns.rat";
  [%expect{| 28 |}]

let%expect_test "factrec" =
  runtam "../../fichiersRat/src-rat-tam-test/factrec.rat";
  [%expect{| 120 |}] 

  (************************************************************************)

(* Test Pointeurs *)
 let%expect_test "test_Int" = 
  runtam "../../fichiersRat/src-rat-pointeurs/testInt.rat";
  [%expect{| 3 |}]

  let%expect_test "test_Rat" = 
  runtam "../../fichiersRat/src-rat-pointeurs/testRat.rat";
  [%expect{| [3/2] |}]

  let%expect_test "test_Bool" = 
  runtam "../../fichiersRat/src-rat-pointeurs/testBool.rat";
  [%expect{| false |}]

  let%expect_test "test_Pt_Double" = 
  runtam "../../fichiersRat/src-rat-pointeurs/testDoublePt.rat";
  [%expect{| 7 |}]

  let%expect_test "test_Pt_Operations" = 
  runtam "../../fichiersRat/src-rat-pointeurs/testOperations.rat";
  [%expect{| falsefalse1235 |}]


(* Test Assignation *)
let%expect_test "tesInt" = 
runtam "../../fichiersRat/src-rat-assign/testInt.rat";
[%expect{| 13 |}]

let%expect_test "tesInt" = 
runtam "../../fichiersRat/src-rat-assign/testRat.rat";
[%expect{| [2/1] |}]

let%expect_test "testPt" = 
runtam "../../fichiersRat/src-rat-assign/testPt.rat";
[%expect{| 8 |}]



(* Test Type Nommé *)
let%expect_test "testRat" = 
runtam "../../fichiersRat/src-rat-typeNomme/testRat.rat";
[%expect{| [2/1] |}]
let%expect_test "tesBool" = 
runtam "../../fichiersRat/src-rat-typeNomme/testBool.rat";
[%expect{| true |}] 
let%expect_test "testInt" = 
runtam "../../fichiersRat/src-rat-typeNomme/testInt.rat";
[%expect{| 8 |}]
let%expect_test "testGlobalRat" = 
runtam "../../fichiersRat/src-rat-typeNomme/testGlobalRat.rat";
[%expect{| [2/1] |}]
let%expect_test "tesGlobalBool" = 
runtam "../../fichiersRat/src-rat-typeNomme/testGlobalBool.rat";
[%expect{| true |}] 
let%expect_test "testGlobalInt" = 
runtam "../../fichiersRat/src-rat-typeNomme/testGlobalInt.rat";
[%expect{| 8 |}]
let%expect_test "test_double" = 
runtam "../../fichiersRat/src-rat-typeNomme/testDouble.rat";
[%expect{| [1/2][2/1] |}]

let%expect_test "test_fun" = 
runtam "../../fichiersRat/src-rat-typeNomme/testfun.rat";
[%expect{| 7 |}]


(* ******************************************************************** *)

(*Tests Enregistrement *)

let%expect_test "testRat" = 
runtam "../../fichiersRat/src-rat-struct/testrat.rat";
[%expect{| [1/2][3/2] |}] 
let%expect_test "tesBool" = 
runtam "../../fichiersRat/src-rat-struct/testbool.rat";
[%expect{| truetruefalse |}] 
let%expect_test "testInt" = 
runtam "../../fichiersRat/src-rat-struct/testInt.rat";
[%expect{| 24 |}]
let%expect_test "test" = 
runtam "../../fichiersRat/src-rat-struct/test.rat";
[%expect{| 4[1/2] |}]
let%expect_test "testmixte" = 
runtam "../../fichiersRat/src-rat-struct/testmixte.rat";
[%expect{| 4[1/2]false |}] 

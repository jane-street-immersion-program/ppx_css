
(* css -> t-> sexp_of_t 
let of_string ?pos s =
    (* the parser produces different output depending on if there is 
       a leading space or not.  They're equivalent semantically, but 
       I can add this to remove ambiguity. *)
    let s = " " ^ s in
    Css_parser.Parser.parse_stylesheet ?pos s
  ;;

  let sexp_of_t = Sexpers.sexp_of_stylesheet 
  
  We want to test for a sexp_of_t version of a css
  *)
(* 
  let catch_automatic_error ~f =
    let return_value = Css_jane.of_string s in
    let sexp_of_css = Css_jane.sexp_of_t () in 
    if (return_value = sexp_of_css) 
      then let return_value = "You passed the test" in
  else let return_value = error in

  - create a list of all of the selectors list to check that the selectors are part fo that list and not something different?

  ;; *)
 
  let%expect_test "Test hello" = 
  print_endline "hello";
    [%expect{| hello |}] 

  let%expect_test "Test Website Example" = 
  let result = Css_jane.Stylesheet.sexp_of_t (Css_jane.Stylesheet.of_string 
    {|dd > p:first-child,
    li > p:first-child {
      margin-top: 0;
    }
  |}) in 
    Stdio.print_s result;
    [%expect{|
((Style_rule
  ((prelude
    ((Ident dd) (Delim >) (Ident p) (Delim :) (Ident first-child) (Delim ,)
     (Ident li) (Delim >) (Ident p) (Delim :) (Ident first-child)))
   (block
    ((Declaration ((name margin-top) (value ((Number 0))) (important false)))))))) |}]  

    let%expect_test "Test hello" = 
    let result = Css_jane.Stylesheet.sexp_of_t (Css_jane.Stylesheet.of_string "") in 
    Stdio.print_s result;
    [%expect{| () |}] 

    let%expect_test "Test Simple CSS" = 
    let result = Css_jane.Stylesheet.sexp_of_t (Css_jane.Stylesheet.of_string " a { } ") in 
    Stdio.print_s result;
    [%expect{|
      ((Style_rule ((prelude ((Ident a))) (block ())))) |}] 

    let%expect_test "Test Background Colour" = 
    let result = Css_jane.Stylesheet.sexp_of_t (Css_jane.Stylesheet.of_string "body {
      background: lightblue;
    }") in 
    Stdio.print_s result;
    [%expect{|
      ((Style_rule
        ((prelude ((Ident body)))
         (block
          ((Declaration
            ((name background) (value ((Ident lightblue))) (important false)))))))) |}] 

    let%expect_test "Test Background Colour" = 
    let result = Css_jane.Stylesheet.sexp_of_t (Css_jane.Stylesheet.of_string "h1 {
      color: white;
      text-align: center;}") in 
    Stdio.print_s result;
    [%expect{|
      ((Style_rule
        ((prelude ((Ident h1)))
         (block
          ((Declaration ((name color) (value ((Ident white))) (important false)))
           (Declaration
            ((name text-align) (value ((Ident center))) (important false)))))))) |}] 

  let%expect_test "Test Sibling Slector Example" = 
    let result = Css_jane.Stylesheet.sexp_of_t (Css_jane.Stylesheet.of_string 
    "label, 
    ul {
    border: 1px solid #cecfd5;
    border-radius: 6px;
    }") in 
    Stdio.print_s result;
    [%expect{|
      ((Style_rule
        ((prelude ((Ident label) (Delim ,)  (Hash ul)))
         (block
          ((Declaration
            ((name border)
             (value ((Float_dimension (1 px Length)) (Ident solid) (Hash cecfd5)))
             (important false)))
           (Declaration
            ((name border-radius) (value ((Float_dimension (6 px Length))))
             (important false)))))))) |}] 


    let%expect_test "Test CSS Paragraphs" = 
    let result = Css_jane.Stylesheet.sexp_of_t (Css_jane.Stylesheet.of_string 
    {|#para1 {
      text-align: center;
      color: red;
    }
    |}) in 
    Stdio.print_s result;
    [%expect{|
      ((Style_rule
        ((prelude ((Delim *) (Hash para1)))
         (block
          ((Declaration
            ((name text-align) (value ((Ident center))) (important false)))
           (Declaration ((name color) (value ((Ident red))) (important false)))))))) |}] 

  let%expect_test "Test Sibling Slector Example" = 
    let result = Css_jane.Stylesheet.sexp_of_t (Css_jane.Stylesheet.of_string 
    "a {
      background-position: 0 50%;
      background-repeat: no-repeat;
      color: #0087cc;
      padding-left: 22px;
      text-decoration: none;
    }") in 
    Stdio.print_s result;
    [%expect{|
      ((Style_rule
        ((prelude ((Ident a)))
         (block
          ((Declaration
            ((name background-position) (value ((Number 0) (Percentage 50)))
             (important false)))
           (Declaration
            ((name background-repeat) (value ((Ident no-repeat)))
             (important false)))
           (Declaration ((name color) (value ((Hash 0087cc))) (important false)))
           (Declaration
            ((name padding-left) (value ((Float_dimension (22 px Length))))
             (important false)))
           (Declaration
            ((name text-decoration) (value ((Ident none))) (important false)))))))) |}] 


    let%expect_test "Test non-CSS files" = 
    let result = Css_jane.Stylesheet.sexp_of_t (Css_jane.Stylesheet.of_string 
    "hi") in 
    Stdio.print_s result;
    [%expect.unreachable]
        [@@expect.uncaught_exn {|
          (* CR expect_test_collector: This test expectation appears to contain a backtrace.
             This is strongly discouraged as backtraces are fragile.
             Please change this test to not include a backtrace. *)
    
          ("Css_parser.Lexer.ParseError(_)")
          Raised at Css_parser__Lexer.parse in file "vendor/css_parser/src/lexer.ml", line 346, characters 9-39
          Called from Test_css_jane.(fun) in file "css_jane/test/test_css_jane.ml", line 149, characters 47-88
          Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 247, characters 12-19 |}] 

open OUnit2
open Types
open Eval

let env = [("x", VInt 4); 
           ("b", VStr "foo"); 
           ("pr", VPair(VInt 10,VBool true)); 
           ("lst",VCons(VStr "david",VCons(VInt 42,VNil)))]
let mlst = [(PCons (PVar "x", PVar "y"), Bin (Add, Var "x", Int 1)); 
            (PInt 3,Int 4);(PPair(PInt 10,PBool true), Str "hi");
            (PStr "hello",Str "world");(PInt 42,Int 43);
            (PBool true,Str "true");
            (PStr "nate",Str "foster");
            (PVar "x",Bin (Add, Var "x", Int 1));]

let eval_tests = [
  "unit test 1" >:: (fun _ ->
      assert_equal (eval env Unit) (VUnit));

  "bool test 1" >:: (fun _ ->
      assert_equal (eval env (Bool true)) (VBool true));

  "pair test 1" >:: (fun _ ->
      assert_equal 
        (eval env (Pair ((Bin (Add, Int 3, Int 2)), 
                         (Bin (Add, Int 3, Int 2))))) 
        (VPair (VInt 5, VInt 5)));
  "pair test 2" >:: (fun _ ->
      assert_equal (eval env (Pair (Int 3, Int 3))) (VPair (VInt 3, VInt 3)));
  "pair test 3" >:: (fun _ ->
      assert_equal (eval env (Pair (Int 1, ((Bin (Add, Int 3, Int 2))))))
        (VPair (VInt 1, VInt 5)));
  "pair test 4" >:: (fun _ ->
      assert_equal (eval env (Pair ((Bin (Add, Int 3, Int 2)), Int 1)))
        (VPair (VInt 5, VInt 1)));
  "pair test 5, pair of pairs" >:: (fun _ ->
      assert_equal (eval env (Pair
                                (Pair(Int 42, Int 3110),
                                 Pair(Int 42,Int 3110)))) 
        (VPair(VPair(VInt 42,VInt 3110),VPair(VInt 42,VInt 3110))));
  "pair test 6, pair of pairs with variables and function" >:: (fun _ -> 
      assert_equal ~printer:Pretty.print 
        (eval env (Pair 
                     (Pair (Var "x",
                            Bool true),
                      Pair(Fun (PVar "fun", Bin(Add,Int 1,Var "x")), 
                           Var "b")))) 
        (VPair 
           ((VPair (VInt 4, 
                    VBool true)), 
            (VPair (VFun (PVar "fun", Bin(Add,Int 1,Var "x"), env), 
                    VStr "foo")))));

  "int test 1" >:: (fun _ ->
      assert_equal (eval env (Int 2)) (VInt 2));

  "string test 1" >:: (fun _ ->
      assert_equal (eval env (Str "hello")) (VStr "hello"));

  "variable test 1" >:: (fun _ ->
      assert_equal (eval env (Var "x")) (VInt 4));
  "variable test 2" >:: (fun _ -> try let _ = eval env (Var "d") in
                            assert false with UnboundVariable -> assert true);

  "function test 1" >:: (fun _ ->
      assert_equal (eval env (Fun (PInt 3, Var "y"))) 
        (VFun (PInt 3, Var "y", [("x", VInt 4);("b", VStr "foo");
                                 ("pr",VPair(VInt 10,VBool true));
                                 ("lst",VCons(VStr "david",VCons(VInt 42,VNil)))
                                ])));
  "function test 2" >:: (fun _ ->
      assert_equal (eval env (Fun (PVar "z", Var "y"))) 
        (VFun (PVar "z", Var "y", [("x", VInt 4); 
                                   ("b", VStr "foo");
                                   ("pr",VPair(VInt 10,VBool true));
                                   ("lst",VCons(VStr "david",VCons(VInt 42,VNil)))
                                  ])));

  "function application test 1"  >:: (fun _ ->
      assert_equal (eval env (App ((Fun (PInt 3, Int 9)), (Int 3)))) (VInt 9));
  "function application test 2"  >:: (fun _ ->
      assert_equal (eval env (App ((Fun (PWild, Int 9)), (Int 3)))) (VInt 9));
  "function application test 3"  >:: (fun _ ->
      try let _ = eval env (App ((Fun (PInt 3, Int 9)), (Int 2))) in
        assert false with ArgumentMismatch -> assert true);
  "function application test 4"  >:: (fun _ ->
      try let _ = eval env (App (Str "hello", (Int 2))) in
        assert false with ExpectedFunction -> assert true);
  "function application test 5"  >:: (fun _ ->
      assert_equal 
        (eval env (App ((Fun (PVar "y", 
                              (Bin (Add, Var "y", Int 1)))), (Int 5)))) 
        (VInt 6));
  "function application test 6"  >:: (fun _ ->
      assert_equal (eval env (App ((Fun (PVar "y", Int 6)), (Int 5)))) (VInt 6));
  "function application test 7"  >:: (fun _ ->
      assert_equal 
        (eval env (App ((Fun (PVar "y",
                              (Bin (Add, Int 3, Int 1)))), 
                        (Int 5)))) 
        (VInt 4));
  "function application test 8"  >:: (fun _ ->
      assert_equal (eval env (App ((Fun (PVar "y", Var "y")), (Int 5)))) (VInt 5));
  "function application test 9"  >:: (fun _ ->
      assert_equal (eval env (App ((Fun (PVar "x", Var "x")), (Int 3)))) (VInt 3));
  "function application test 10"  >:: (fun _ ->
      assert_equal 
        (eval env (App 
                     ((Fun (PPair (PInt 1, PInt 2), Str "hello")), 
                      (Pair (Int 1, Int 2))))) 
        (VStr "hello"));
  "function application test 11"  >:: (fun _ ->
      assert_equal 
        (eval env (App ((Fun (PPair (PPair (PInt 1, PInt 2), 
                                     PPair (PInt 3, PInt 4)), Str "hello")), 
                        (Pair (Pair (Int 1, Int 2), Pair (Int 3, Int 4)))))) 
        (VStr "hello"));
  "function application test 12"  >:: (fun _ ->
      assert_equal 
        (eval env (App ((Fun (PVar "y", Var "x")), (Int 5)))) (VInt 4));

  "binop test 1" >:: (fun _ ->
      assert_equal 
        ~printer:Pretty.print (eval env (Bin (Mod, Int 21, Int 5))) (VInt 1));
  "binop test 2" >:: (fun _ -> 
      try let _ = (eval env (Bin (Eq, Int 2, Str "pancake"))) in assert false 
      with ArgumentMismatch -> assert true);
  "binop test 3" >:: (fun _ ->
      assert_equal 
        ~printer:Pretty.print (eval env (Bin (Or, Bool true, Bool false))) 
        (VBool true));
  "binop test 4" >:: (fun _ -> 
      try let _ = (eval env (Bin (Or, Bool false, Str "pancake"))) in assert false 
      with ArgumentMismatch -> assert true);
  "binop test 3" >:: (fun _ ->
      assert_equal 
        ~printer:Pretty.print (eval env (Bin (Cat, Str "moo", Var "b"))) 
        (VStr "moofoo"));
  "let test 1" >:: (fun _ ->
      assert_equal 
        ~printer:Pretty.print 
        (eval env (Let (PVar "x", Bool true, Bin (Eq, Bool false, Var "x")))) 
        (VBool false));
  "let test 2" >:: (fun _ ->
      assert_equal 
        ~printer:Pretty.print 
        (eval env (Let (PStr "hi", Str "hi", Bool true))) 
        (VBool true));
  "let test 3" >:: (fun _ -> 
      try let _ = (eval env (Let (PStr "hi", Str "bye", Bool true))) in assert false 
      with LetMismatch -> assert true);

  "una test 1" >:: (fun _ -> 
      assert_equal 
        ~printer:Pretty.print (eval env (Una(Neg,Int 3))) (VInt (-3)));
  "una test 2" >:: (fun _ ->
      assert_equal 
        ~printer:Pretty.print (eval env (Una(Neg,Int(-32)))) (VInt (32)));  
  "una test 3" >:: (fun _ ->
      assert_equal 
        ~printer:Pretty.print (eval env (Una(Not,Bool true))) (VBool false));
  "una test 4" >:: (fun _ -> 
      try let _ = assert_equal 
              ~printer:Pretty.print (eval env (Una(Not,Int 42))) in assert false
      with ArgumentMismatch -> assert true);
  "una test 5" >:: (fun _ ->
      try let _ = assert_equal 
              ~printer:Pretty.print (eval env (Una(Neg,Str "hey"))) in assert false
      with ArgumentMismatch -> assert true);
  "una test 6, using var in environment" >:: (fun _ ->
      assert_equal ~printer:Pretty.print 
        (eval env (Una (Neg, Var "x"))) (VInt (-4)));

  "seq test 1" >:: (fun _ -> 
      assert_equal ~printer:Pretty.print (eval env(Seq(Unit,Int 42))) (VInt 42));
  "seq test 2" >:: (fun _ -> 
      try let _ = assert_equal 
              ~printer:Pretty.print 
              (eval env(Seq(Bool true,Int 42))) in assert false 
      with ExpectedUnit -> assert true);

  "ifthen test 1" >:: (fun _ ->
      assert_equal 
        ~printer:Pretty.print 
        (eval env(IfThen(Bin(Gt,Int 4,Int 3),Bool true, Bool false))) 
        (VBool true)); 
  "ifthen test 2" >:: (fun _ ->
      assert_equal 
        ~printer:Pretty.print 
        (eval env(IfThen(Bin(Eq,Str "nate",Str "foster"),Str "true", Str "false"))) 
        (VStr "false"));
  "ifthen test 3" >:: (fun _ -> 
      try let _ = assert_equal 
              ~printer:Pretty.print(eval env(IfThen(Str "true", Int 42, Int 12))) 
        in assert false 
      with ExpectedBool -> assert true);

  "match test 1" >:: (fun _ ->
      assert_equal 
        ~printer:Pretty.print
        (eval env(Match(Str "hello",mlst))) (VStr "world"));
  "match test 2" >:: (fun _ ->
      assert_equal 
        ~printer:Pretty.print
        (eval env(Match(Str "nate",mlst))) (VStr "foster"));
  "match test 3" >:: (fun _ ->
      assert_equal 
        ~printer:Pretty.print
        (eval env(Match(Int 42,mlst))) (VInt 43));
  "match test 4" >:: (fun _ ->
      assert_equal 
        ~printer:Pretty.print
        (eval env(Match(Bool true,mlst))) (VStr "true"));
  "match test 6" >:: (fun _ ->
      assert_equal 
        ~printer:Pretty.print
        (eval env(Match(Int 3,mlst))) (VInt 4));
  "match test 7" >:: (fun _ ->
      assert_equal 
        ~printer:Pretty.print
        (eval env(Match(Pair(Int 10,Bool true),mlst))) (VStr "hi"));
  "match test 8" >:: (fun _ ->
      assert_equal 
        ~printer:Pretty.print
        (eval env(Match(Int 3,mlst))) (VInt 4));
  "match test 9" >:: (fun _ ->
      assert_equal 
        ~printer:Pretty.print
        (eval env(Match(Cons (Int 3, Nil),mlst))) (VInt 4));

  "ref test 1" >:: (fun _ -> 
      assert_equal 
        ~printer:Pretty.print
        (eval env(Ref(Int 42))) (VRef(ref(VInt 42))));
  "ref test 2" >:: (fun _ -> 
      assert_equal 
        ~printer:Pretty.print
        (eval env(Ref(Bool true))) (VRef(ref(VBool true))));
  "ref test 3" >:: (fun _ -> 
      assert_equal 
        ~printer:Pretty.print
        (eval env(Ref(Int 4))) (VRef(ref(VInt 4))));
  "ref test 4" >:: (fun _ -> 
      assert_equal 
        ~printer:Pretty.print
        (eval env(Ref(Str "foo"))) (VRef(ref(VStr "foo"))));
  "ref test 5" >:: (fun _ -> 
      assert_equal 
        ~printer:Pretty.print
        (eval env(Ref(Pair(Int 10,Bool true)))) 
        (VRef(ref(VPair(VInt 10,VBool true)))));
  "ref test 6" >:: (fun _ -> 
      assert_equal 
        ~printer:Pretty.print
        (eval env(Ref(Cons(Str "david",Cons(Int 42,Nil))))) 
        (VRef(ref(VCons(VStr "david",VCons(VInt 42,VNil))))));

  "deref test 1" >:: (fun _ -> 
      assert_equal
        ~printer:Pretty.print
        (eval env(Deref(Ref(Int 42)))) (VInt 42));
  "deref test 2" >:: (fun _ -> 
      assert_equal 
        ~printer:Pretty.print
        (eval env(Deref(Ref(Int 4)))) (VInt 4));
  "deref test 3" >:: (fun _ -> 
      assert_equal 
        ~printer:Pretty.print
        (eval env(Deref(Ref(Str "foo")))) ((VStr "foo")));
  "deref test 4" >:: (fun _ -> 
      assert_equal 
        ~printer:Pretty.print
        (eval env(Deref(Ref(Pair(Int 10,Bool true))))) 
        ((VPair(VInt 10,VBool true))));
  "deref test 5" >:: (fun _ -> 
      assert_equal 
        ~printer:Pretty.print
        (eval env(Deref(Ref(Cons(Str "david",Cons(Int 42,Nil)))))) 
        ((VCons(VStr "david",VCons(VInt 42,VNil)))));
  "deref test 6" >:: (fun _ ->
      try let _ = assert_equal 
              ~printer:Pretty.print(eval env(Deref(Int 12))) in assert false
      with ExpectedRef -> assert true);

  "assign test 1" >:: (fun _ -> 
      let expr = 
        Let (PVar "ref1", Ref (Int 4), 
             Let(PWild,(Assign (Var "ref1", Int 42)),Deref(Var "ref1"))) in
      assert_equal ~printer:Pretty.print(eval env expr) (VInt 42));
  "assign test 2" >:: (fun _ ->
      let expr = 
        Let (PVar "ref2", Ref (Str "foo"), 
             Let(PWild,(Assign(Var "ref2", Str "ocaml")),Deref(Var "ref2"))) in
      assert_equal ~printer:Pretty.print(eval env expr) (VStr "ocaml"));
  "assign test 3" >:: (fun _ ->
      let expr = 
        Let (PVar "ref3", Ref (Pair(Int 10,Bool true)), 
             Let(PWild,(Assign(Var "ref3", Nil)),Deref(Var "ref3"))) in
      assert_equal ~printer:Pretty.print(eval env expr) (VNil));
  "assign test 4" >:: (fun _ ->
      try let expr = 
            Let (PVar "var1", Int 7, 
                 Let(PWild,(Assign(Var "var1", Int 2)),Deref(Var "ref4"))) in 
        let _ = assert_equal ~printer:Pretty.print(eval env expr) in assert false
      with ExpectedRef -> assert true);

  "rec test 1" >:: (fun _ -> 
      assert_equal 
        ~printer:Pretty.print
        (eval env (LetRec("fun",
                          Fun(PVar "y",(Bin(Add,Var "y",Int 1))),
                          App (Var "fun",Int 3)))) (VInt 4));
  "rec test 2" >:: (fun _ -> 
      let func = LetRec("f1",
                        Fun(PVar "y", 
                            IfThen (Bin (Eq, Var "y", Int 0), Int 3, 
                                    Bin (Add, 
                                         App (Var "f1", (Bin (Sub, Var "y", Int 1))), 
                                         Int 1))),
                        App(Var "f1",Int 2)) in 
      assert_equal ~printer:Pretty.print (eval env func) (VInt 5));
  "rec test 3, with matching" >:: (fun _ -> 
      let func = 
        LetRec("sum_lst_123",
               Fun(PVar "lst", 
                   Match(Var "lst",
                         [(PNil, Int 0); 
                          (PCons (PInt 3, PNil), 
                           Bin(Add, Int 3, App(Var "sum_lst_123", Nil)));
                          (PCons (PInt 2, PCons (PInt 3, PNil)), 
                           Bin(Add, Int 2, 
                               App(Var "sum_lst_123", Cons (Int 3, Nil))));
                          (PCons (PInt 1, PCons (PInt 2, PCons (PInt 3, PNil))), 
                           Bin(Add, Int 1, 
                               App(Var "sum_lst_123", 
                                   Cons (Int 2, Cons (Int 3, Nil)))));
                         ])),
               App(Var "sum_lst_123",
                   Cons (Int 1, Cons (Int 2, Cons (Int 3, Nil))))) in
      assert_equal ~printer:Pretty.print (eval env func) (VInt 6));
]

let suite = "interpreter test suite" >::: List.flatten [
    eval_tests
  ]

let _ = run_test_tt_main suite

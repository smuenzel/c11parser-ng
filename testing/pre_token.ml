open! Core
open! Include

let%expect_test ""=
  test_pre_token
    {|
    |};
  [%expect {| ((1:0-2:0 Newline) (2:4-2:4 Eof)) |}]

let%expect_test ""=
  test_pre_token
    {|#define X Y
    |};
  [%expect {|
    ((1:0-1:1 (Punctuator (preceeded_by_whitespace false) (value #)))
     (1:1-1:7 (Identifier define)) (1:8-1:9 (Identifier X))
     (1:10-1:11 (Identifier Y)) (1:11-2:0 Newline) (2:4-2:4 Eof))
    |}]

let%expect_test ""=
  test_pre_token
    {|u'Xy'
    |};
  [%expect {|
    ((1:0-1:5 (Character_constant ((kind U16) (value ((Plain X) (Plain y))))))
     (1:5-2:0 Newline) (2:4-2:4 Eof))
    |}]

let%expect_test ""=
  test_pre_token
    {|#if 'z' - 'a' == 25
    |};
  [%expect {|
    ((1:0-1:1 (Punctuator (preceeded_by_whitespace false) (value #)))
     (1:1-1:3 (Identifier if))
     (1:4-1:7 (Character_constant ((kind Plain) (value ((Plain z))))))
     (1:7-1:9 (Punctuator (preceeded_by_whitespace true) (value -)))
     (1:10-1:13 (Character_constant ((kind Plain) (value ((Plain a))))))
     (1:13-1:16 (Punctuator (preceeded_by_whitespace true) (value ==)))
     (1:17-1:19 (Preprocessing_number 25)) (1:19-2:0 Newline) (2:4-2:4 Eof))
    |}]

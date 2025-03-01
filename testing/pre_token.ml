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
     (1:8-1:9 (Punctuator (preceeded_by_whitespace true) (value -)))
     (1:10-1:13 (Character_constant ((kind Plain) (value ((Plain a))))))
     (1:14-1:16 (Punctuator (preceeded_by_whitespace true) (value ==)))
     (1:17-1:19 (Preprocessing_number 25)) (1:19-2:0 Newline) (2:4-2:4 Eof))
    |}]

let%expect_test ""=
  test_pre_token
    {|#include <stdio.h>
      #include "myprog.h"
    |};
  [%expect {|
    ((1:0-1:1 (Punctuator (preceeded_by_whitespace false) (value #)))
     (1:1-1:8 (Identifier include)) (1:9-1:18 (Header_name (System stdio.h)))
     (1:18-2:0 Newline)
     (2:6-2:7 (Punctuator (preceeded_by_whitespace true) (value #)))
     (2:7-2:14 (Identifier include)) (2:15-2:25 (Header_name (Local myprog.h)))
     (2:25-3:0 Newline) (3:4-3:4 Eof))
    |}]

let%expect_test ""=
  test_pre_token
    {|
#if VERSION == 1
#define INCFILE "vers1.h"
#elif VERSION == 2
#define INCFILE "vers2.h" // and so on
#else
#define INCFILE "versN.h"
#endif
#include INCFILE
    |};
  [%expect {|
    ((1:0-2:0 Newline)
     (2:0-2:1 (Punctuator (preceeded_by_whitespace false) (value #)))
     (2:1-2:3 (Identifier if)) (2:4-2:11 (Identifier VERSION))
     (2:12-2:14 (Punctuator (preceeded_by_whitespace true) (value ==)))
     (2:15-2:16 (Preprocessing_number 1)) (2:16-3:0 Newline)
     (3:0-3:1 (Punctuator (preceeded_by_whitespace false) (value #)))
     (3:1-3:7 (Identifier define)) (3:8-3:15 (Identifier INCFILE))
     (3:16-3:25 (Header_name (Local vers1.h))) (3:25-4:0 Newline)
     (4:0-4:1 (Punctuator (preceeded_by_whitespace false) (value #)))
     (4:1-4:5 (Identifier elif)) (4:6-4:13 (Identifier VERSION))
     (4:14-4:16 (Punctuator (preceeded_by_whitespace true) (value ==)))
     (4:17-4:18 (Preprocessing_number 2)) (4:18-5:0 Newline)
     (5:0-5:1 (Punctuator (preceeded_by_whitespace false) (value #)))
     (5:1-5:7 (Identifier define)) (5:8-5:15 (Identifier INCFILE))
     (5:16-5:25 (Header_name (Local vers2.h))) (5:26-6:0 Newline)
     (6:0-6:1 (Punctuator (preceeded_by_whitespace false) (value #)))
     (6:1-6:5 (Identifier else)) (6:5-7:0 Newline)
     (7:0-7:1 (Punctuator (preceeded_by_whitespace false) (value #)))
     (7:1-7:7 (Identifier define)) (7:8-7:15 (Identifier INCFILE))
     (7:16-7:25 (Header_name (Local versN.h))) (7:25-8:0 Newline)
     (8:0-8:1 (Punctuator (preceeded_by_whitespace false) (value #)))
     (8:1-8:6 (Identifier endif)) (8:6-9:0 Newline)
     (9:0-9:1 (Punctuator (preceeded_by_whitespace false) (value #)))
     (9:1-9:8 (Identifier include)) (9:9-9:16 (Identifier INCFILE))
     (9:16-10:0 Newline) (10:4-10:4 Eof))
    |}]

let%expect_test ""=
  test_pre_token
    {|
#define hash_hash # ## #
#define mkstr(a) # a
#define in_between(a) mkstr(a)
#define join(c, d) in_between(c hash_hash d)
char p[] = join(x, y); // equivalent to
                       // char p[] = "x ## y";
    |};
  [%expect {|
    ((1:0-2:0 Newline)
     (2:0-2:1 (Punctuator (preceeded_by_whitespace false) (value #)))
     (2:1-2:7 (Identifier define)) (2:8-2:17 (Identifier hash_hash))
     (2:18-2:19 (Punctuator (preceeded_by_whitespace true) (value #)))
     (2:20-2:22 (Punctuator (preceeded_by_whitespace true) (value ##)))
     (2:23-2:24 (Punctuator (preceeded_by_whitespace true) (value #)))
     (2:24-3:0 Newline)
     (3:0-3:1 (Punctuator (preceeded_by_whitespace false) (value #)))
     (3:1-3:7 (Identifier define)) (3:8-3:13 (Identifier mkstr))
     (3:13-3:14 (Punctuator (preceeded_by_whitespace false) (value "(")))
     (3:14-3:15 (Identifier a))
     (3:15-3:16 (Punctuator (preceeded_by_whitespace false) (value ")")))
     (3:17-3:18 (Punctuator (preceeded_by_whitespace true) (value #)))
     (3:19-3:20 (Identifier a)) (3:20-4:0 Newline)
     (4:0-4:1 (Punctuator (preceeded_by_whitespace false) (value #)))
     (4:1-4:7 (Identifier define)) (4:8-4:18 (Identifier in_between))
     (4:18-4:19 (Punctuator (preceeded_by_whitespace false) (value "(")))
     (4:19-4:20 (Identifier a))
     (4:20-4:21 (Punctuator (preceeded_by_whitespace false) (value ")")))
     (4:22-4:27 (Identifier mkstr))
     (4:27-4:28 (Punctuator (preceeded_by_whitespace false) (value "(")))
     (4:28-4:29 (Identifier a))
     (4:29-4:30 (Punctuator (preceeded_by_whitespace false) (value ")")))
     (4:30-5:0 Newline)
     (5:0-5:1 (Punctuator (preceeded_by_whitespace false) (value #)))
     (5:1-5:7 (Identifier define)) (5:8-5:12 (Identifier join))
     (5:12-5:13 (Punctuator (preceeded_by_whitespace false) (value "(")))
     (5:13-5:14 (Identifier c))
     (5:14-5:15 (Punctuator (preceeded_by_whitespace false) (value ,)))
     (5:16-5:17 (Identifier d))
     (5:17-5:18 (Punctuator (preceeded_by_whitespace false) (value ")")))
     (5:19-5:29 (Identifier in_between))
     (5:29-5:30 (Punctuator (preceeded_by_whitespace false) (value "(")))
     (5:30-5:31 (Identifier c)) (5:32-5:41 (Identifier hash_hash))
     (5:42-5:43 (Identifier d))
     (5:43-5:44 (Punctuator (preceeded_by_whitespace false) (value ")")))
     (5:44-6:0 Newline) (6:0-6:4 (Identifier char)) (6:5-6:6 (Identifier p))
     (6:6-6:7 (Punctuator (preceeded_by_whitespace false) (value [)))
     (6:7-6:8 (Punctuator (preceeded_by_whitespace false) (value ])))
     (6:9-6:10 (Punctuator (preceeded_by_whitespace true) (value =)))
     (6:11-6:15 (Identifier join))
     (6:15-6:16 (Punctuator (preceeded_by_whitespace false) (value "(")))
     (6:16-6:17 (Identifier x))
     (6:17-6:18 (Punctuator (preceeded_by_whitespace false) (value ,)))
     (6:19-6:20 (Identifier y))
     (6:20-6:21 (Punctuator (preceeded_by_whitespace false) (value ")")))
     (6:21-6:22 (Punctuator (preceeded_by_whitespace false) (value ";")))
     (6:23-7:0 Newline) (7:23-8:0 Newline) (8:4-8:4 Eof))
    |}]

let%expect_test ""=
  test_pre_token
    {|
    #
    |};
  [%expect {|
    ((1:0-2:0 Newline)
     (2:4-2:5 (Punctuator (preceeded_by_whitespace true) (value #)))
     (2:5-3:0 Newline) (3:4-3:4 Eof))
    |}]

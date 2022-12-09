{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Closure.Convert where

import Control.Monad.State.Strict (evalState)
import Miniml.Closure.Convert (convert)
import Miniml.Closure.Free (calcFreeVars, fnsCalledInBody, fnsInSameFix, initIteration)
import Miniml.Cps (Cexp (..), Value (..))
import Miniml.Optimization.Hoist (escaping)
import Miniml.Shared (Access (Offp), Primop (Plus, Times))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Closure conversion"
    [ testCase "Example in figure 10.2 in book" testExample,
      testCase "Fix with only known functions doesn't create closure" testNoClosure,
      testCase "Fix with known closure functions uses fresh vars for record" testClosureFreshVar,
      testCase "Fix with known recursive closure" testKnownRecClos
    ]

testExample :: IO ()
testExample = do
  let (h : x : d : w : t1 : f : arg : c : g : y : j : a : k : b : t2 : z : i : m : n : r : q : t : s : p : e : u : _) = [1 ..]
  let e0 =
        Fix
          [ ( h,
              [x, d],
              Primop
                Times
                [Var x, Var w]
                [t1]
                [App (Var d) [Var t1]]
            ),
            ( f,
              [arg, c],
              Select
                0
                (Var arg)
                g
                ( Select
                    1
                    (Var arg)
                    y
                    ( Fix
                        [ ( j,
                            [a],
                            Fix
                              [ ( k,
                                  [b],
                                  Primop
                                    Plus
                                    [Var a, Var b]
                                    [t2]
                                    [App (Var c) [Var t2]]
                                )
                              ]
                              (App (Label h) [Var z, Var k])
                          )
                        ]
                        (App (Var g) [Var y, Var j])
                    )
                )
            )
          ]
          ( Record
              [(Var f, Offp 0), (Int 1, Offp 0)]
              i
              ( Fix
                  [ ( m,
                      [n, r],
                      Primop
                        Plus
                        [Var n, Var t]
                        [q]
                        [App (Var r) [Var q]]
                    ),
                    ( s,
                      [p],
                      Primop
                        Plus
                        [Var p, Var e]
                        [100]
                        [App (Label m) [Var 100, Var 0]]
                    )
                  ]
                  ( Record
                      [(Var m, Offp 0), (Int 1, Offp 0)]
                      u
                      (App (Label f) [Var u, Var s])
                  )
              )
          )
  let esc = escaping e0
      iter = calcFreeVars 10 (fnsInSameFix e0) (fnsCalledInBody e0) (initIteration e0)
      e0' = evalState (convert esc iter e0) 100
  -- 1   2   3   4   5    6   7     8   9  10  11  12  13  14   15  16  17  18  19  20  21  22  23  24  25  26
  -- h : x : d : w : t1 : f : arg : c : g : y : j : a : k : b : t2 : z : i : m : n : r : q : t : s : p : e : u
  let [w', d', f', f'', j', j'', k', k'', m', m'', s', s'', a', c'', c', c''', w'', z', w''', z'', t', r', m''', e', m'''', g'] =
        [117, 118, 101, 119, 122, 124, 128, 129, 102, 105, 103, 112, 131, 130, 132, 126, 125, 127, 120, 121, 108, 110, 111, 115, 116, 123]
  e0'
    @?= Fix
      [ ( h,
          [x, d, w'],
          Primop
            Times
            [Var x, Var w']
            [t1]
            [Select 0 (Var d) d' (App (Var d') [Var d, Var t1])]
        ),
        ( f',
          [f'', arg, c],
          Select
            1
            (Var f'')
            w'''
            ( Select
                2
                (Var f'')
                z''
                ( Select
                    0
                    (Var arg)
                    g
                    ( Select
                        1
                        (Var arg)
                        y
                        ( Fix
                            [ ( j',
                                [j'', a],
                                Select
                                  1
                                  (Var j'')
                                  w''
                                  ( Select
                                      2
                                      (Var j'')
                                      c'''
                                      ( Select
                                          3
                                          (Var j'')
                                          z'
                                          ( Fix
                                              [ ( k',
                                                  [k'', b],
                                                  Select
                                                    1
                                                    (Var k'')
                                                    c''
                                                    ( Select
                                                        2
                                                        (Var k'')
                                                        a'
                                                        ( Primop
                                                            Plus
                                                            [Var a', Var b]
                                                            [t2]
                                                            [ Select
                                                                0
                                                                (Var c'')
                                                                c'
                                                                (App (Var c') [Var c'', Var t2])
                                                            ]
                                                        )
                                                    )
                                                )
                                              ]
                                              ( Record
                                                  [(Label k', Offp 0), (Var c''', Offp 0), (Var a, Offp 0)]
                                                  k
                                                  (App (Label h) [Var z', Var k, Var w''])
                                              )
                                          )
                                      )
                                  )
                              )
                            ]
                            ( Record
                                [(Label j', Offp 0), (Var w''', Offp 0), (Var c, Offp 0), (Var z'', Offp 0)]
                                j
                                ( Select
                                    0
                                    (Var g)
                                    g'
                                    (App (Var g') [Var g, Var y, Var j])
                                )
                            )
                        )
                    )
                )
            )
        )
      ]
      ( Record
          [(Label f', Offp 0), (Var w, Offp 0), (Var z, Offp 0)]
          f
          ( Record
              [(Var f, Offp 0), (Int 1, Offp 0)]
              i
              ( Fix
                  [ ( m',
                      [m'', n, r],
                      Offset
                        1
                        (Var m'')
                        106
                        ( Select
                            2
                            (Var m'')
                            107
                            ( Select
                                3
                                (Var m'')
                                t'
                                ( Select
                                    4
                                    (Var m'')
                                    109
                                    ( Primop
                                        Plus
                                        [Var n, Var t']
                                        [q]
                                        [Select 0 (Var r) r' (App (Var r') [Var r, Var q])]
                                    )
                                )
                            )
                        )
                    ),
                    ( s',
                      [s'', p],
                      Offset
                        (-1)
                        (Var s'')
                        m'''
                        ( Select
                            1
                            (Var s'')
                            113
                            ( Select
                                2
                                (Var s'')
                                114
                                ( Select
                                    3
                                    (Var s'')
                                    e'
                                    ( Primop
                                        Plus
                                        [Var p, Var e']
                                        [100]
                                        [Select 0 (Var m''') m'''' (App (Var m'''') [Var m''', Var 100, Var 113])]
                                    )
                                )
                            )
                        )
                    )
                  ]
                  ( Record
                      [(Label m', Offp 0), (Label s', Offp 0), (Var 0, Offp 0), (Var t, Offp 0), (Var e, Offp 0)]
                      m
                      ( Offset
                          1
                          (Var m)
                          s
                          ( Record
                              [(Var m, Offp 0), (Int 1, Offp 0)]
                              u
                              (Select 0 (Var f) 104 (App (Var 104) [Var f, Var u, Var s]))
                          )
                      )
                  )
              )
          )
      )

testNoClosure :: IO ()
testNoClosure = do
  let e0 =
        Fix
          [ (1, [2, 3], Primop Plus [Var 2, Var 3] [4] [App (Var 5) [Var 4]]),
            (5, [6], App (Var 0) [Var 6])
          ]
          (App (Var 1) [Int 2, Int 3])
      esc = escaping e0
      iter = calcFreeVars 10 (fnsInSameFix e0) (fnsCalledInBody e0) (initIteration e0)
      e0' = evalState (convert esc iter e0) 100
  e0'
    @?= Fix
      [ ( 1,
          [2, 3, 101],
          Primop
            Plus
            [Var 2, Var 3]
            [4]
            [App (Label 5) [Var 4, Var 101]]
        ),
        (5, [6, 102], Select 0 (Var 102) 103 (App (Var 103) [Var 102, Var 6]))
      ]
      (App (Label 1) [Int 2, Int 3, Var 0])

testClosureFreshVar :: IO ()
testClosureFreshVar = do
  let e0 =
        Primop
          Plus
          [Int 4, Int 5]
          [7]
          [ Primop
              Plus
              [Int 6, Int 7]
              [8]
              [ Fix
                  [ ( 1,
                      [2, 3],
                      Primop
                        Plus
                        [Var 2, Var 3]
                        [4]
                        [ Primop
                            Plus
                            [Var 7, Var 8]
                            [9]
                            [ Primop
                                Plus
                                [Var 4, Var 9]
                                [10]
                                [App (Var 5) [Var 10]]
                            ]
                        ]
                    ),
                    (5, [6], App (Var 0) [Var 6]),
                    (11, [], App (Var 1) [Int 5, Int 6])
                  ]
                  (App (Var 1) [Int 2, Int 3])
              ]
          ]
      esc = escaping e0
      iter = calcFreeVars 3 (fnsInSameFix e0) (fnsCalledInBody e0) (initIteration e0)
      e0' = evalState (convert esc iter e0) 100
  e0'
    @?= Primop
      Plus
      [Int 4, Int 5]
      [7]
      [ Primop
          Plus
          [Int 6, Int 7]
          [8]
          [ Fix
              [ ( 1,
                  [105, 2, 3],
                  Select
                    0
                    (Var 105)
                    102
                    ( Select
                        1
                        (Var 105)
                        103
                        ( Select
                            2
                            (Var 105)
                            104
                            ( Primop
                                Plus
                                [Var 2, Var 3]
                                [4]
                                [ Primop
                                    Plus
                                    [Var 103, Var 104]
                                    [9]
                                    [ Primop
                                        Plus
                                        [Var 4, Var 9]
                                        [10]
                                        [App (Label 5) [Var 10, Var 102]]
                                    ]
                                ]
                            )
                        )
                    )
                ),
                (5, [6, 106], Select 0 (Var 106) 107 (App (Var 107) [Var 106, Var 6])),
                ( 11,
                  [111],
                  Select
                    0
                    (Var 111)
                    108
                    ( Select
                        1
                        (Var 111)
                        109
                        ( Select
                            2
                            (Var 111)
                            110
                            (App (Label 1) [Var 111, Int 5, Int 6])
                        )
                    )
                )
              ]
              ( Record
                  [(Var 0, Offp 0), (Var 7, Offp 0), (Var 8, Offp 0)]
                  101
                  (App (Label 1) [Var 101, Int 2, Int 3])
              )
          ]
      ]

testKnownRecClos :: IO ()
testKnownRecClos = do
  let e0 =
        Primop
          Plus
          [Int 4, Int 5]
          [7]
          [ Primop
              Plus
              [Int 6, Int 7]
              [8]
              [ Fix
                  [ ( 1,
                      [2, 3],
                      Primop
                        Plus
                        [Var 2, Var 3]
                        [4]
                        [Primop Plus [Var 7, Var 8] [9] [App (Var 1) [Var 4, Var 9]]]
                    )
                  ]
                  (App (Var 1) [Int 2, Int 3])
              ]
          ]
      esc = escaping e0
      iter = calcFreeVars 3 (fnsInSameFix e0) (fnsCalledInBody e0) (initIteration e0)
      e0' = evalState (convert esc iter e0) 100
  e0'
    @?= Primop
      Plus
      [Int 4, Int 5]
      [7]
      [ Primop
          Plus
          [Int 6, Int 7]
          [8]
          [ Fix
              [ ( 1,
                  [104, 2, 3],
                  Select
                    0
                    (Var 104)
                    102
                    ( Select
                        1
                        (Var 104)
                        103
                        ( Primop
                            Plus
                            [Var 2, Var 3]
                            [4]
                            [ Primop
                                Plus
                                [Var 102, Var 103]
                                [9]
                                [App (Label 1) [Var 104, Var 4, Var 9]]
                            ]
                        )
                    )
                )
              ]
              ( Record
                  [(Var 7, Offp 0), (Var 8, Offp 0)]
                  101
                  (App (Label 1) [Var 101, Int 2, Int 3])
              )
          ]
      ]
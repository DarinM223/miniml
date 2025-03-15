{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Closure.Lift where

import Miniml.Closure.Lift (lift)
import Miniml.Cps (Cexp (..), Value (..))
import Miniml.Shared (Access (Offp), Primop (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Lambda lifting"
    [ testCase "Tests figure 10.2 example after being closure converted" testExample
    ]

testExample :: IO ()
testExample = do
  let (h : x : d : w : t1 : f : arg : c : g : y : j : a : k : b : t2 : z : i : m : n : r : q : t : s : p : e : u : _) = [1 ..]
  let [w', d', f', f'', j', j'', k', k'', m', m'', s', s'', a', c'', c', c''', w'', z', w''', z'', t', r', m''', e', m'''', g'] =
        [117, 118, 101, 119, 122, 124, 128, 129, 102, 105, 103, 112, 131, 130, 132, 126, 125, 127, 120, 121, 108, 110, 111, 115, 116, 123]
  let e0 =
        Fix
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
  lift e0
    @?= Fix
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
                            [ Select
                                0
                                (Var m''')
                                m''''
                                (App (Var m'''') [Var m''', Var 100, Var 113])
                            ]
                        )
                    )
                )
            )
        ),
        ( h,
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
                        ( Record
                            [(Label j', Offp 0), (Var w''', Offp 0), (Var c, Offp 0), (Var z'', Offp 0)]
                            j
                            (Select 0 (Var g) g' (App (Var g') [Var g, Var y, Var j]))
                        )
                    )
                )
            )
        ),
        ( j',
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
                    ( Record
                        [(Label k', Offp 0), (Var c''', Offp 0), (Var a, Offp 0)]
                        k
                        (App (Label h) [Var z', Var k, Var w''])
                    )
                )
            )
        ),
        ( k',
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
          [(Label f', Offp 0), (Var w, Offp 0), (Var z, Offp 0)]
          f
          ( Record
              [(Var f, Offp 0), (Int 1, Offp 0)]
              i
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
                          ( Select
                              0
                              (Var f)
                              104
                              (App (Var 104) [Var f, Var u, Var s])
                          )
                      )
                  )
              )
          )
      )

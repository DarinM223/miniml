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
    [ testCase "Example in figure 10.2 in book" testExample
    -- TODO(DarinM223): Fix with only known functions doesn't create closure
    -- TODO(DarinM223): Fix with only known functions that creates closure uses
    -- fresh variable for record name and closure parameter
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
                                                  [(Var k', Offp 0), (Var c''', Offp 0), (Var a, Offp 0)]
                                                  k
                                                  (App (Var h) [Var z', Var k, Var w''])
                                              )
                                          )
                                      )
                                  )
                              )
                            ]
                            ( Record
                                [(Var j', Offp 0), (Var w''', Offp 0), (Var c, Offp 0), (Var z'', Offp 0)]
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
          [(Var f', Offp 0), (Var w, Offp 0), (Var z, Offp 0)]
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
                      [(Var m', Offp 0), (Var s', Offp 0), (Var 0, Offp 0), (Var t, Offp 0), (Var e, Offp 0)]
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
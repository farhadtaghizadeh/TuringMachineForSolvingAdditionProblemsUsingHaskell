module MonoTMExamples where

import MonoTM
    ( TM(TM),
      configs,
      accepts,
      goRight,
      goLeft,
      checkRight,
      checkLeft,
      loopRight,
      loopLeft )


----------------------------------------------------------------------
-- recognize {a^n b^n c^n | n in Nat }
tripletm =
  TM [1 .. 6] "abc" "abc*! " ' ' '!' trans 1 [6]
  where
    trans = checkRight 1 ' ' 6 ++
            loopRight 1 "*" ++
            goRight 1 'a' '*' 2 ++
            loopRight 2 "a*" ++
            goRight 2 'b' '*' 3 ++
            loopRight 3 "b*" ++
            goRight 3 'c' '*' 4 ++
            loopRight 4 "c*" ++
            checkLeft 4 ' ' 5 ++
            loopLeft 5 "abc*" ++
            checkRight 5 '!' 1 

test = configs tripletm 35 "aabbcc"

----------------------------------------------------------------------
-- recognize language { ww | w in {a,b}* }
ww =
  TM [1..13] "abc" "abc*! " ' ' '!' trans 1 [7]
  where
    checkABLeft p q = 
      checkLeft p 'a' q ++
      checkLeft p 'b' q
    checkABRight p q = 
      checkRight p 'a' q ++
      checkRight p 'b' q
    trans =

      -- [w2] (i) nondeterministically pick an a and erase it
      loopRight 1 "ab" ++
      goLeft 1 'a' '*' 2 ++

      -- [w2] (ii) erase a b
      goLeft 1 'b' '*' 8 ++

      ------------------------------------------
      -- the a loop
      ------------------------------------------
      
      -- [w1] move left through a's and b's only until hitting erasure or left end, then erase matching a
      loopLeft 2 "ab" ++
      checkRight 2 '*' 12 ++
      checkRight 2 '!' 12 ++ 
      goRight 12 'a' '*' 3 ++
      
      -- [w1] skip a's and b's to get to w2
      loopRight 3 "ab" ++

      -- [w2] skip erasures moving right
      checkRight 3 '*' 4 ++
      loopRight 4 "*" ++

      -- [w2] (i) erase first a encountered, then skip all the erasures moving left
      goLeft 4 'a' '*' 5 ++
      loopLeft 5 "*" ++

      -- [w1] (a) skip at least one a or b to move back to the start of the a loop
      checkABLeft 5 2 ++

      -- [w1] (b) instead nondeterministically erase an a, because otherwise you might be moving to the left of the last remaining a
      goRight 5 'a' '*' 3 ++
  
      -- [w2] (ii) if instead of an a, you see b, drop down to the b loop
      goLeft 4 'b' '*' 11 ++

      -- [w2] skip erasures
      loopLeft 11 "*" ++

      -- [w1] (a) skip at least one a or b to move back to start of the b loop
      checkABLeft 11 8 ++

      -- [w1] (b) instead nondeterministically erase a b, since otherwise you might move past the last remaining b
      goRight 11 'b' '*' 9 ++

      ------------------------------------------
      -- the b loop
      ------------------------------------------

      -- [w1] move left through a's and b's to erase a b
      loopLeft 8 "ab" ++
      checkRight 8 '*' 13 ++
      checkRight 8 '!' 13 ++
      goRight 13 'b' '*' 9 ++
      
      -- [w1] skip a's and b's to get to w2
      loopRight 9 "ab" ++

      -- [w2] skip erasures moving right
      checkRight 9 '*' 10 ++
      loopRight 10 "*" ++

      -- [w2] (i) erase first b encountered, then skip all the erasures moving left
      goLeft 10 'b' '*' 11 ++

      -- [w2] (ii) if instead of a b, you see an a, pop up to the a loop
      goLeft 10 'a' '*' 5 ++

      ---------------------------------
      -- check all erased
      ---------------------------------

      -- if we reach a blank (so after w2)
      checkLeft 4 ' ' 6 ++
      checkLeft 10 ' ' 6 ++

      -- skip all erasures, looking for the left endmarker
      loopLeft 6 "*" ++
      checkRight 6 '!' 7

test2 = accepts ww "aa"

additionProblem =
  TM [1 .. 69] "10+=" "10+=x " ' ' '!' trans 1 [32]
  where
    trans =
      loopRight 1 "x" ++
      checkRight 1 '+' 23 ++ -- null+n
      goRight 1 '0' 'x' 64 ++
      loopRight 64 "10" ++
      checkRight 64 '+' 65 ++
      loopRight 65 "x" ++
      checkRight 65 '=' 51 ++ -- 0+null
      goRight 65 '0' 'x' 68 ++ -- 0+0
      loopRight 68 "10" ++
      checkRight 68 '=' 69 ++
      loopRight 69 "x" ++
      goRight 69 '0' 'x' 60 ++
      goRight 65 '1' 'x' 66 ++ -- 0+1
      loopRight 66 "10" ++
      checkRight 66 '=' 67 ++
      loopRight 67 "x" ++
      goRight 67 '1' 'x' 60 ++
      goRight 1 '1' 'x' 2 ++
      loopRight 2 "10" ++
      checkRight 2 '+' 3 ++
      loopRight 3 "x" ++
      checkRight 3 '=' 49 ++ -- 1+null
      goRight 3 '0' 'x' 62 ++ -- 1+0
      loopRight 62 "10" ++
      checkRight 62 '=' 63 ++
      loopRight 63 "x" ++
      goRight 63 '1' 'x' 60 ++
      goRight 3 '1' 'x' 4 ++ -- 1+1
      loopRight 4 "10" ++
      checkRight 4 '=' 5 ++
      loopRight 5 "x" ++
      goRight 5 '0' 'x' 6 ++ -- 1+1=0, est carry
      loopRight 6 "10" ++
      checkLeft 6 ' ' 7 ++
      loopLeft 7 "10x=" ++
      checkLeft 7 '+' 8 ++
      loopLeft 8 "10" ++ 
      checkRight 8 'x' 9 ++
      checkRight 9 '+' 14 ++ -- carry, null+n
      loopRight 14 "x" ++
      checkRight 14 '=' 33 ++ -- carry, null+null
      loopRight 33 "x" ++
      goRight 33 '1' 'x' 34 ++ -- null+null=1, remov carry
      loopRight 34 "0" ++
      checkRight 34 ' ' 32 ++ -- null+null = 10*, accept
      goRight 14 '0' 'x' 19 ++ -- carry, null+0
      loopRight 19 "10" ++
      checkRight 19 '=' 20 ++
      loopRight 20 "x" ++
      goRight 20 '1' 'x' 21 ++ -- null+0 = 1, remov carry
      loopRight 21 "10" ++
      checkLeft 21 ' ' 22 ++
      loopLeft 22 "10x=" ++
      checkRight 22 '+' 23 ++
      loopRight 23 "x" ++
      checkRight 23 '=' 31 ++ -- null+null
      loopRight 31 "0x" ++
      checkLeft 31 ' ' 32 ++ -- null+null = 0*, accept
      goRight 23 '0' 'x' 28 ++ -- null+0
      loopRight 28 "10" ++
      checkRight 28 '=' 29 ++
      loopRight 29 "x" ++
      goRight 29 '0' 'x' 30 ++ -- null+0 = 0
      loopRight 30 "10" ++
      checkLeft 30 ' ' 27 ++
      goRight 23 '1' 'x' 24 ++ -- null+1
      loopRight 24 "10" ++
      checkRight 24 '=' 25 ++
      loopRight 25 "x" ++
      goRight 25 '1' 'x' 26 ++ -- null+1 = 1
      loopRight 26 "10" ++
      checkLeft 26 ' ' 27 ++
      loopLeft 27 "10x=" ++
      checkRight 27 '+' 23 ++
      goRight 14 '1' 'x' 15 ++ -- carry, null+1
      loopRight 15 "10" ++
      checkRight 15 '=' 16 ++
      loopRight 16 "x" ++
      goRight 16 '0' 'x' 17 ++ -- carry stays, null+1 = 0
      loopRight 17 "10" ++
      checkLeft 17 ' ' 18 ++
      loopLeft 18 "10x=" ++
      checkRight 18 '+' 14 ++
      goRight 9 '0' 'x' 54 ++ -- carry, 0+n
      loopRight 54 "10" ++
      checkRight 54 '+' 55 ++
      loopRight 55 "x" ++
      checkRight 55 '=' 44 ++ -- carry, 0+null
      goRight 55 '0' 'x' 58 ++ -- carry, 0+0
      loopRight 58 "10" ++
      checkRight 58 '=' 59 ++
      loopRight 59 "x" ++
      goRight 59 '1' 'x' 60 ++ -- 0+0=1, remov carry
      loopLeft 60 "10x= " ++
      checkLeft 60 '+' 61 ++
      loopLeft 61 "10" ++
      checkRight 61 'x' 1 ++
      goRight 55 '1' 'x' 56 ++ -- carry, 0+1
      loopRight 56 "10" ++
      checkRight 56 '=' 57 ++
      loopRight 57 "x" ++
      goRight 57 '0' 'x' 6 ++ -- carry stays, 0+1=0
      goRight 9 '1' 'x' 10 ++ -- carry, 1+n
      loopRight 10 "10" ++
      checkRight 10 '+' 11 ++
      loopRight 11 "x" ++
      checkRight 11 '=' 37 ++ -- carry, 1+null
      loopRight 37 "x" ++
      goRight 37 '0' 'x' 38 ++ -- carry stays, 1+null=0
      loopLeft 38 "10x= " ++
      checkLeft 38 '+' 39 ++
      loopLeft 39 "01" ++
      checkRight 39 'x' 40 ++
      goRight 40 '0' 'x' 43 ++ --carry, 0+null
      loopRight 43 "01x+" ++
      checkRight 43 '=' 44 ++
      loopRight 44 "x" ++
      goRight 44 '1' 'x' 45 ++ -- 0+null=1, remov carry
      loopLeft 45 "10x= " ++
      checkLeft 45 '+' 46 ++
      loopLeft 46 "01" ++
      checkRight 46 'x' 47 ++
      checkRight 47 '+' 52 ++ -- null+null
      loopRight 52 "x" ++
      checkRight 52 '=' 53 ++
      loopRight 53 "0x" ++
      checkLeft 53 ' ' 32 ++ -- null+null = 0*, accept
      goRight 47 '0' 'x' 50 ++ -- 0+null
      loopRight 50 "01x+" ++
      checkRight 50 '=' 51 ++
      loopRight 51 "x" ++
      goRight 51 '0' 'x' 45 ++ -- 0+null=0
      goRight 47 '1' 'x' 48 ++ -- 1+null
      loopRight 48 "01x+" ++
      checkRight 48 '=' 49 ++
      loopRight 49 "x" ++
      goRight 49 '1' 'x' 45 ++ -- 1+null=1
      goRight 40 '1' 'x' 41 ++ -- carry, 1+null
      loopRight 41 "01x+" ++
      checkRight 41 '=' 42 ++
      loopRight 42 "x" ++
      goRight 42 '0' 'x' 38 ++ -- carry stays, 1+null=0
      goRight 11 '0' 'x' 35 ++ -- carry, 1+0
      loopRight 35 "10" ++
      checkRight 35 '=' 36 ++
      loopRight 36 "x" ++
      goRight 36 '0' 'x' 6 ++ -- carry stays, 1+0=0
      goRight 11 '1' 'x' 12 ++ -- carry, 1+1
      loopRight 12 "10" ++
      checkRight 12 '=' 13 ++
      loopRight 13 "x" ++
      goRight 13 '1' 'x' 6 -- carry stays, 1+1=1

test3 = configs additionProblem 300 "0111+1=111100"
test4 = configs additionProblem 300 "111+1001=00001"
test5 = configs additionProblem 300 "1110+1110=0111"
test6 = configs additionProblem 300 "11+0010=1100"

module MonoTM where

import Data.List
import GvShow
import Util

data Direction = GoLeft | GoRight

instance Show Direction where
  show GoLeft = "L"
  show GoRight = "R"

type State = Integer
type Tape = Char
type Input = Char

data Trans = Trans State Tape Direction State Tape 

instance Show Trans where
  show (Trans s1 t1 d s2 t2) = show s1 ++ " === " ++ show t1 ++ " / " ++ show t2 ++ " " ++ show d ++ " ===> " ++ show s2

data TM = TM {
  states :: [State], -- Q: all states.  
  inputs :: [Input], -- Sigma: all possible inputs.
  tapesyms :: [Tape], -- Gamma: all possible stack symbols.
  blank :: Tape, -- blank symbol
  leftend :: Tape, -- left endmarker
  trans :: [Trans], -- R: transition relation
  start :: State, -- start state
  final :: [State]} -- final states

----------------------------------------------------------------------
-- createTM
----------------------------------------------------------------------
createTM ::
  [State] -> -- states
  [Input] -> -- inputs
  [Tape] -> -- tapesyms
  Tape -> -- blank
  Tape -> -- leftend
  [Trans] -> -- trans
  State -> -- start
  [State] -> -- final
  TM
createTM = TM

showListIndent :: Show a => [a] -> String
showListIndent xs = concat $ intersperse "\n" $ fmap (\ s -> "  " ++ show s) xs 

instance Show TM where
  show (TM states inputs tapesyms blank leftend trans start final) =
    "States: " ++ show states ++ "\n" ++
    "Alphabet: " ++ show inputs ++ "\n" ++
    "Tape symbols: " ++ show tapesyms ++ "\n" ++    
    "Blank: " ++ show blank ++ "\n" ++
    "Leftend: " ++ show leftend ++ "\n" ++
    "Transitions:\n" ++ showListIndent trans ++ "\n" ++
    "Start state: " ++ show start ++ "\n" ++
    "Final states: " ++ show final

----------------------------------------------------------------------
-- showTM
----------------------------------------------------------------------
showTM :: TM -> String
showTM = show

data Config = Config State Tape {- the blank -} [Tape] [Tape]
  deriving Eq
type Configs = [Config]
newtype History = History { openHistory :: [Configs] }

instance Show Config where
  show (Config st blank leftrev right) =
    "[" ++ show st ++ ": " ++ (show $ reverse leftrev) ++ " " ++ (show $ takeWhile (/= blank) right) ++ "]"

----------------------------------------------------------------------
-- showConfig
----------------------------------------------------------------------
showConfig :: Config -> String
showConfig = show

instance Show History where
  show (History css) = 
    foldr (\ cs str -> show cs ++ "\n" ++ str) "" css

----------------------------------------------------------------------
-- showHistory
----------------------------------------------------------------------
showHistory :: History -> String
showHistory = show

shiftConfig :: Config -> Direction -> Config 
shiftConfig (Config st blank leftrev (r:right)) GoRight = Config st blank (r:leftrev) right
shiftConfig (Config st blank (l:leftrev) (right)) GoLeft = Config st blank leftrev (l:right)

updateConfig :: Config -> State -> Tape -> Direction -> Config  
updateConfig (Config st blank (l:leftrev) right) st' g = shiftConfig (Config st' blank (g:leftrev) right)

newConfigs :: TM -> Config -> Configs
newConfigs tm c@(Config st _ (l:leftrev) (right)) =
  let nexts = [ (st'',g'',d) | Trans st' g' d st'' g'' <- (trans tm) , st == st' , l == g' ] in
    let newConfig (st,g,d) = updateConfig c st g d in
      map newConfig nexts

----------------------------------------------------------------------
-- initialConfig
----------------------------------------------------------------------
initialConfig :: TM -> [Input] -> Config 
initialConfig tm xs =
  let t = xs ++ repeat (blank tm) in
    Config (start tm) (blank tm) [head t , leftend tm] (tail t)

configsLazy :: TM -> [Input] -> History
configsLazy tm xs = History (iterate addNew [initialConfig tm xs])
  where addNew [] = []
        addNew (c : cs) = cs ++ newConfigs tm c

----------------------------------------------------------------------
-- configs
----------------------------------------------------------------------
configs :: TM -> Int -> [Input] -> History
configs tm n xs = History $ take n $ openHistory $ configsLazy tm xs

----------------------------------------------------------------------
-- accepting
----------------------------------------------------------------------
accepting :: TM -> [Input] -> Maybe Config
accepting tm xs = head <$> find acceptingConfig (takeWhile (/= []) $ openHistory $ configsLazy tm xs)
  where acceptingConfig (c@(Config st _ _ _) : cs) = elem st (final tm) 
        acceptingConfig _ = False
        
----------------------------------------------------------------------
-- accepts
----------------------------------------------------------------------
accepts :: TM -> [Input] -> Bool
accepts tm xs =
  case accepting tm xs of
    Just _ -> True
    Nothing -> False

-- transition from p to q with no change to the tape or readhead position
epsEdge :: State -> State -> State -> [Tape] -> [Trans]
epsEdge p intermediate q ts =
  concat (do
             t <- ts
             return [Trans p t GoRight intermediate t, Trans intermediate t GoLeft q t])

goRight :: State -> Tape -> Tape -> State -> [Trans]
goRight st g g' st' = [Trans st g GoRight st' g']

goLeft :: State -> Tape -> Tape -> State -> [Trans]
goLeft st g g' st' = [Trans st g GoLeft st' g']

-- if the current cell has the given value g, then transition from state p to q, moving readhead right
checkRight :: State -> Tape -> State -> [Trans]
checkRight p g q = goRight p g g q 

-- similar to checkRight, but move readhead left
checkLeft :: State -> Tape -> State -> [Trans]
checkLeft p g q = goLeft p g g q 

loop :: Direction -> State -> [Tape] -> [Trans]
loop d st = map (\ g -> Trans st g d st g) 

loopRight :: State -> [Tape] -> [Trans]
loopRight = loop GoRight

loopLeft :: State -> [Tape] -> [Trans]
loopLeft = loop GoLeft

toGraphViz :: Bool -> TM -> String
toGraphViz printNodeNames (TM states inputs _ _ _ trans start finals) =
    "digraph pda {\n" ++
    "graph [pad=\"1\", nodesep=\".5\", ranksep=\"1\"];\n" ++
    "rankdir = LR;\n" ++
    "hidden [shape = plaintext, label = \"\"];\n" ++
    "node [shape = doublecircle];\n" ++
    (foldrGlue (\ f str -> 
                   gvshow f ++ (if printNodeNames then "" else " [label = \"\"]") ++ ";\n" ++ str)
               finals
       ("node [shape = "  ++ (if printNodeNames then "circle" else "point") ++ "];\n" ++
        "hidden -> " ++ gvshow start ++ ";\n" ++ 
       -- loop over transitions st
       (foldrGlue (\ (Trans st g d st' g') str ->
                     gvshow st ++ " -> " ++ gvshow st' ++ " [label = \"" ++ gvshow g ++ (case d of { GoRight -> "/" ; GoLeft -> "\\\\"})
                     ++ gvshow g' ++ "\"];\n" ++ str)
         trans
         "}\n")))

-- write the given NFA in GraphViz format to the file named filename.
writeGraphViz :: String -> TM -> IO ()
writeGraphViz filename d =
  writeFile filename (toGraphViz False d)

-- same, but write node names
writeGraphVizN :: String -> TM -> IO ()
writeGraphVizN filename d =
  writeFile filename (toGraphViz True d)

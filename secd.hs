import qualified Data.Map as Map

data Term = HALT
            | LDC Int
            | LD Int
            | ADD
            | SUB
            | MUL
            | SEL [Term] [Term]
            | JOIN
            | LDF [Term]
            | LDRF [Term]
            | AP
            | RTN
             deriving Show

-- a code block (list of instructions)
type Code = [Term]

-- closure: pairs of code, environment
type Closure = (Code, Env)

-- closure addresses
type Addr = Int

-- memory for closures
type Memory = Map.Map Addr Closure

-- get the next available address
next :: Memory -> Addr
next memory = 1 + Map.size memory

-- a value of the SECD machine is either
-- a primitive integer or the address of a closure
data Value = I Int
            | A Addr
             deriving (Eq, Show)

-- the SECD machine components
type Stack = [Value]
type Env   = [Value]
type Dump  = [(Stack, Env, Code)]

-- the SECD machine configuration
type Conf  = (Stack, Env, Code, Dump, Memory)

-- Interpreter
eval :: Conf -> Conf

eval (s, e, LDC n:c, d, m)
    = (I n:s, e, c, d, m)

eval (I e2:I e1:s, e, ADD:c, d, m)
    = (I (e1 + e2):s, e, c, d, m)

eval (I e2:I e1:s, e, SUB:c, d, m)
    = (I (e1 - e2):s, e, c, d, m)

eval (I e2:I e1:s, e, MUL:c, d, m)
    = (I (e1 * e2):s, e, c, d, m)

eval (s, e, LD i:c, d, m)
    = let v = e!!i
    in (v:s, e, c, d, m)

eval (s, e, LDF c':c, d, m)
    = let a = next m
          m' = Map.insert a (c',e) m
    in (A a:s, e, c, d, m')

eval (s, e, LDRF c':c, d, m)
    = let a = next m
          m' = Map.insert a (c', A a:e) m
    in (A a:s, e, c, d, m')

eval (v:A a:s, e, AP:c, d, m)
    = let Just (c', e') = Map.lookup a m
    in ([], v:e', c', (s, e, c):d, m)

eval (v:s, e, RTN:c, (s', e', c'):d, m)
    = (v:s', e', c', d, m)

eval (I n:s, e, (SEL c1 c2):c, d, m)
    | n == 0 = (s, e, c1, ([], [], c):d, m)
    | otherwise = (s, e, c2, ([], [], c):d, m)

eval (s, e, JOIN:c, (_, _, c'):d, m)
    = (s, e, c', d, m)

eval (s, e, HALT:c, d, m)
    = (s, e, [], d, m)

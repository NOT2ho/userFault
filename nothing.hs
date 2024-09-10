data Exp = UNIT
    | TRUE
    | FALSE
    | CONST Int
    | VAR Var
    | ADD Exp Exp
    | SUB Exp Exp
    | MUL Exp Exp
    | DIV Exp Exp
    | EQUAL Exp Exp
    | LESS Exp Exp
    | NOT Exp
    | NULL
    | CONS Exp Exp
    | APPEND Exp Exp
    | HEAD Exp
    | TAIL Exp
    | ISZERO Exp
    | ISNULL Exp
    | IF Exp Exp Exp
    | LET Var Exp Exp
    | LETREC Var Var Exp Exp
    | LETMREC (Var, Var, Exp) (Var, Var, Exp) Exp
    | PROC Var Exp
    | CALL Exp Exp
    | PRINT Exp
    | SEQ Exp Exp

type Var = String

data Value = Unit
    | INT Int
    | BOOL Bool
    | LIST [Value]
    | Procedure Var Exp Env
    | RecProcedure Var Var Exp Env
    | MRecProcedure (Var, Var, Exp) (Var, Var, Exp) Env

type Env = [(Var, Value)]

extEnv :: Env -> Env -> Env
extEnv e0 e1
    | length e0 == 1 = head e0 : e1
    | otherwise = error "UserFault"

appEnv :: Var -> Env -> Value
appEnv _ [] = error "UserFault"
appEnv v (e:es) =
    let (var, value) = e in
        if var == v then value else appEnv v es

eval :: Exp -> Env -> Value
eval (CONST n) _ = INT n
eval (VAR x) env = appEnv x env
eval (ADD e0 e1) env=
    let v0 = eval e0 env in
    let v1 = eval e1 env in
            case (v0, v1) of
                (INT n0, INT n1) -> INT $ n0+n1
                _ -> error "UserFault"
eval (SUB e0 e1) env=
    let v0 = eval e0 env in
    let v1 = eval e1 env in
            case (v0, v1) of
                (INT n0, INT n1) -> INT $ n0-n1
                _ -> error "UserFault"
eval (MUL e0 e1) env=  
    let v0 = eval e0 env in
    let v1 = eval e1 env in
            case (v0, v1) of
                (INT n0, INT n1) -> INT $ n0*n1
                _ -> error "UserFault"
eval (DIV e0 e1) env=
    let v0 = eval e0 env in
    let v1 = eval e1 env in
            case (v0, v1) of
                (INT n0, INT n1) -> INT $ div n0 n1
                _ -> error "UserFault"
{-      
eval (EQUAL e0 e1) env =
        case (eval e0 env, eval e1 env) of
            (INT e0, INT e1) -> if e0==e1 then BOOL True else BOOL False
            _ -> error "UserFault"
-}

eval (NOT e) _ =
    case e of
        TRUE -> BOOL False
        FALSE -> BOOL True
        _ -> error "UserFault"


eval (ISZERO e) env =
    case eval e env of
        INT n 
            | n == 0 -> BOOL True
            | n /= 0 -> BOOL False
        _ -> error "UserFault"

eval (IF e0 e1 e2) env =
   case eval e0 env of
        BOOL True -> eval e1 env
        BOOL False -> eval e2 env
        _ -> error "UserFault"

eval (LET x e0 e1) env =
    let v = eval e0 env in
        eval e1 (extEnv [(x,v)] env)

eval (ISNULL e) env = eval (EQUAL e NULL) env

eval TRUE _ = BOOL True
eval FALSE _ = BOOL False
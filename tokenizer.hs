import Data.Char
data TokenType = LEFTPARAN
                | RIGHTPARAN
                | LEFTBRACE
                | RIGHTBRACE
                | COLON
                | PLUS
                | MINUS
                | STAR
                | SLASH
                | SEMICOLON
                | SHARP
                | WAVEDASH
                | DASH
                | GREATERTHEN
                | NUM
                | UPPERCASE
                | LOWERCASE
                | EQUAL
                | AT
            deriving (Show)

type Token = ([Char], TokenType)

tokenizer :: String -> [Token]
tokenizer (s:ss)  =
    case s of
        '(' -> ([s], LEFTPARAN) : tokenizer ss
        ')' -> ([s], RIGHTPARAN) : tokenizer ss
        '{' -> ([s], LEFTBRACE) : tokenizer ss
        '}' -> ([s], RIGHTBRACE) : tokenizer ss
        ':' -> ([s], COLON) : tokenizer ss
        ';' -> ([s], SEMICOLON) : tokenizer ss
        '+' -> ([s], PLUS) : tokenizer ss
        '-' -> ([s], MINUS) : tokenizer ss
        '*' -> ([s], STAR) : tokenizer ss
        '/' -> ([s], SLASH) : tokenizer ss
        '#' -> ([s], SHARP) : tokenizer ss
        '~' -> ([s], WAVEDASH) : tokenizer ss
        '>' -> ([s], GREATERTHEN) : tokenizer ss
        '=' -> ([s], EQUAL) : tokenizer ss
        '@' -> ([s], AT) : tokenizer ss
        ' ' -> tokenizer ss
        _ -> if isDigit s 
                then (takeWhile isDigit (s:ss), NUM) 
                    : tokenizer (dropWhile isDigit ss)
                else if isUpper s 
                    then (takeWhile isAlphaNum (s:ss), UPPERCASE) 
                        : tokenizer (dropWhile isAlphaNum ss)
                    else if isLower s 
                        then (takeWhile ((||) <$> isDigit <*> isLower) (s:ss), LOWERCASE) 
                            : tokenizer (dropWhile ((||) <$> isDigit <*> isLower) ss)
                        else error "UserFault"
tokenizer [] = []
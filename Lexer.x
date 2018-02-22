{
module Lexer where
}

%wrapper "basic"

$digit = 0-9      -- digits
$alpha = [a-zA-Z]   -- alphabetic characters

tokens :-

  $white+         ;
  "--".*          ;
  $digit+           { \s -> IntTok (read s) }
  [\*]              { \s -> MulTok}
  [\+]              { \s -> AddTok}
  [\^]              { \s -> ExpTok}
  [\-]              { \s -> SubTok}
  [\/]              { \s -> Par (slash (s))}
  $digit*\.$digit+  { \s -> RealTok (read s)}
  life              { \s -> IntTok (42)}
  pi                { \s -> RealTok (3.142)}
  fogarte           { \s -> RealTok (2.718)}
  tau               { \s -> RealTok (6.283)}
  [\~]              { \s -> RoundTok}
  [\%]              { \s -> ModTok}
  \(                { \s -> LPTok }
  \)                { \s -> RPTok }
  ifz               { \s -> IfzTok }
  then              { \s -> ThenTok }
  else              { \s -> ElseTok }
  ms                { \s -> MSTok }
  mr                { \s -> MRTok }

{
-- Each action has type :: String -> Token

data Par = IntDivTok| DivTok deriving Show

slash :: String -> Par
slash s = case s of
                  "/":"/":xs -> IntDivTok
                  "/":xs     -> DivTok

-- The token type:
data Token =
  IntTok Int|
  MultTok|
  AddTok|
  ExpTok|
  SubTok|
  ParTok Par|
  RealTok Double|
  RoundTok|
  ModTok|
  LeftTok|
  RightTok|
  IfzTok|
  ThenTok|
  ElseTok|
  MSTok|
  MRTok|
  deriving (Eq,Show)
scanTokens :: String -> Maybe [Token]
scanTokens str = go ('\n',[],str)
  where go inp@(_,_bs,str) =
          case alexScan inp 0 of
                AlexEOF -> Just []
                AlexError _ -> Nothing
                AlexSkip  inp' len     -> go inp'
                AlexToken inp' len act -> fmap ((act (take len str)):) (go inp')

}



{
module Main (main) where
}

%wrapper "posn"

$digit = [0-9]			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

$u = [. \n]         --universal set

@id = $alpha ($alpha | $digit | \_)*

@octnum = \\ [0-3] [0-7] [0-7]
@hexnum = \\x[0-9a-fA-F][0-9a-fA-F]
@otherEscape = \\a | \\b | \\f | \\n | \\r | \\t | \\v | \\\\ | \\\"

tokens :-

  $white+						;
  "//".*						                              ; --single line comment
  "/*" ([$u # \*] | \* [$u # \/])* ("*")* "*/"     ; --multiline comment
  --keywords
  array							{ \p s -> Array p }
  if								{ \p s -> If p }
  then							{ \p s -> Then p }
  else							{ \p s -> Else p }
  while							{ \p s -> While p }
  for								{ \p s -> For p }
  to								{ \p s -> To p }
  do								{ \p s -> Do p }
  let								{ \p s -> Let p }
  in								{ \p s -> In p }
  end								{ \p s -> End p }
  of								{ \p s -> Of p }
  break							{ \p s -> Break p }
  nil								{ \p s -> Nil p }
  function					{ \p s -> Function p }
  var								{ \p s -> Var p }
  type							{ \p s -> Type p }
  import						{ \p s -> Import p }
  primitive					{ \p s -> Primitive p }
  --operators
  ","								{ \p s -> Coma p }
  ":"								{ \p s -> Colon p }
  ";"								{ \p s -> Semicolon p }
  "("								{ \p s -> LRbrace p }
  ")"								{ \p s -> RRbrace p }
  "["								{ \p s -> LSbrace p }
  "]"								{ \p s -> RSbrace p }
  "{"								{ \p s -> LCbrace p }
  "}"								{ \p s -> RCbrace p }
  "."								{ \p s -> Dot p }
  "+"								{ \p s -> Plus p }
  "-"								{ \p s -> Minus p }
  "*"								{ \p s -> Multiply p }
  "/"								{ \p s -> Divide p }
  "="								{ \p s -> Eq p }
  "<>"							{ \p s -> NEq p }
  "<"								{ \p s -> Less p }
  "<="							{ \p s -> LessEq p }
  ">"								{ \p s -> Gr p }
  ">="							{ \p s -> GrEq p }
  "&"								{ \p s -> And p }
  "|"								{ \p s -> Or p }
  ":="						  { \p s -> Assign p }

  \" ([. # \\]* (@octnum | @hexnum | @otherEscape)* )* \"   { \p s -> String (unquot s) p }

  $digit+                 { \p s -> Number (read s) p }
  @id                 		{ \p s -> Identifier s p }
  _main								    { \p s -> Identifier s p }


{
-- Each action has type :: String -> Token

-- The token type:
data Token =
  Array AlexPosn      |
  If AlexPosn         |
  Then AlexPosn       |
  Else AlexPosn       |
  While AlexPosn      |
  For AlexPosn        |
  To AlexPosn         |
  Do AlexPosn         |
  Let AlexPosn        |
  In AlexPosn         |
  End AlexPosn        |
  Of AlexPosn         |
  Break AlexPosn      |
  Nil AlexPosn        |
  Function AlexPosn   |
  Var AlexPosn        |
  Type AlexPosn       |
  Import AlexPosn     |
  Primitive AlexPosn  |
  Coma AlexPosn       |
  Colon AlexPosn      |
  Semicolon AlexPosn  |
  LRbrace AlexPosn    |
  RRbrace AlexPosn    |
  LSbrace AlexPosn    |
  RSbrace AlexPosn    |
  LCbrace AlexPosn    |
  RCbrace AlexPosn    |
  Dot AlexPosn        |
  Plus AlexPosn       |
  Minus AlexPosn      |
  Multiply AlexPosn   |
  Divide AlexPosn     |
  Eq AlexPosn         |
  NEq AlexPosn        |
  Less AlexPosn       |
  LessEq AlexPosn     |
  Gr AlexPosn         |
  GrEq AlexPosn       |
  And AlexPosn        |
  Or AlexPosn         |
  Assign AlexPosn     |

  String String AlexPosn |
  Number Int AlexPosn    |
  Identifier String AlexPosn

  deriving (Eq,Show)


unquot (x:xs) = init xs

printElements :: [Token] -> IO()
printElements [] = return ()
printElements (x:xs) = do print(x)
                          printElements xs

main = do
  s <- getContents
  let alltokens = alexScanTokens s
  printElements alltokens
--  print (alltokens)
}
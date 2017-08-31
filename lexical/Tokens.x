{
module Main (main) where
}

%wrapper "basic"

$digit = [0-9]			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-

  $white+							;
  "//".*							;
  "/*"[. \n]*"*/"					;
  array								{ \s -> Array }
  if								{ \s -> If }
  then								{ \s -> Then }
  else								{ \s -> Else }
  while								{ \s -> While }
  for								{ \s -> For }
  to								{ \s -> To }
  do								{ \s -> Do }
  let								{ \s -> Let }
  in								{ \s -> In }
  end								{ \s -> end }
  of								{ \s -> Of }
  break								{ \s -> Break }
  nil								{ \s -> Nil }
  function							{ \s -> Function }
  var								{ \s -> Var }
  type								{ \s -> Type }
  import							{ \s -> Import }
  primitive							{ \s -> Primitive }
  ","								{ \s -> Coma }
  ":"								{ \s -> Colon }
  ";"								{ \s -> Semicolon }
  "("								{ \s -> LRbrace }
  ")"								{ \s -> RRbrace }
  "["								{ \s -> LSbrace }
  "]"								{ \s -> RSbrace }
  "{"								{ \s -> LCbrace }
  "}"								{ \s -> RCbrace }
  "."								{ \s -> Dot }
  "+"								{ \s -> Plus }
  "-"								{ \s -> Minus }
  "*"								{ \s -> Multiply }
  "/"								{ \s -> Divide }
  "="								{ \s -> Eq }
  "<>"								{ \s -> NEq }
  "<"								{ \s -> LT }
  "<="								{ \s -> LE }
  ">"								{ \s -> GT }
  ">="								{ \s -> GE }
  "&"								{ \s -> And }
  "|"								{ \s -> Or }
  ":="								{ \s -> Assign }

  $digit+							{ \s -> Int (read s) }
  [\=\+\-\*\/\(\)]					{ \s -> Sym (head s) }
  $alpha [$alpha $digit \_ \']*		{ \s -> Id s }
  _main								{ \s -> Id s }


{
-- Each action has type :: String -> Token

-- The token type:
data Token =
	Let 		|
	In  		|
	Sym Char	|
	Id String	|
	Int Int
	deriving (Eq,Show)

main = do
  s <- getContents
  print (alexScanTokens s)
}
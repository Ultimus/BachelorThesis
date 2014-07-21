{
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module PrologLexer (main, lexString) where

import Token
}

%wrapper "basic"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters
$atom = [a-z]                   --lower case letter for atoms


tokens :-

  $white+				;
  \' \$ bind \_ var\'                   ;
  \[ \]                                 {\s -> Bin 0}
  [ \( \) \[ \] \, \_ \- \>]            ;
  packed \_ visited \_ expression       ;
  transition				;
  const \_ and \_ vars                  ;
  \' \$ avl \_ exp \'                   ;
  \' \$ avl \_ bv \'                    { \s -> Sep '-'}
  \' \$ fd \_ $alpha+ $digit+ \'        { \s -> FDInt s}
  \' \$ $alpha+ \_ $alpha+ \'           ;
  \.                                    { \s -> Sep '|' }
  \' $alpha+ \'                         { \s -> S s }
  pred \_ false                         { \s -> B False }
  pred \_ true                          { \s -> B True }
  $atom \_* $alpha*                     { \s -> Atom s } 
  \' \$ alpha+ \_ $alpha+ $digit+ \'    { \s -> S s } -- I just need the digit
  $digit+				{ \s -> Int (read s) }
  \' $alpha \_ $alpha* \'               ; 



{
-- Each action has type :: String -> Token


lexString = alexScanTokens

main = do
  s <- getContents
  print (alexScanTokens s)
}


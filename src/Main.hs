{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes      #-}
module Main where

import qualified Data.Map              as Map

import           Data.Function         ((&))
import           Data.List             (isInfixOf, intersperse)
import           Text.Pandoc.JSON
import           Text.Read
import           Text.Regex.PCRE.Heavy
import           Debug.Trace

type IsEscaped = Bool

encloseInListingEscape :: IsEscaped -> String -> String
encloseInListingEscape True s  = s
encloseInListingEscape False s = "@" ++ s ++ "@"

escapeForLatex :: IsEscaped -> String -> String
escapeForLatex isEscaped = concatMap escape
  where
  escape '$' = if isEscaped then "\\$" else "$"
  escape c   = [c]

replaceDashWithLatex :: IsEscaped -> String -> String
replaceDashWithLatex isEscaped = gsub ([re|(\--)|]) toLatex
  where
  toLatex ("--":_) = encloseInListingEscape isEscaped "-{}-"
  toLatex _        = ""

replaceTagWithLatex :: IsEscaped -> String -> String
replaceTagWithLatex isEscaped s = foldl replaceWith s replacements
  where
  replacements = [ ([re|<strong>(.*?)</strong>|], "\\texttt{\\textbf{", "}}")
                 , ([re|<em>(.*?)</em>|], "\\texttt{\\textit{", "}}")
                 , ([re|<sub>(.*?)</sub>|], "\\textsubscript{", "}")
                 ]
  replaceWith s' (r, pre, post) = gsub r (toLatex pre post) s'
  toLatex pre post (contents:_) =
    let replacedContents = replaceWithLatex True contents
        command = pre ++ replacedContents ++ post
    in encloseInListingEscape isEscaped command
  toLatex _ _ [] = ""

replaceWithLatex :: Bool -> String -> String
replaceWithLatex isEscaped =
  replaceDashWithLatex isEscaped . replaceTagWithLatex isEscaped . escapeForLatex isEscaped

postProcess :: Format -> String -> String
postProcess fmt contents
  | fmt == Format "latex" = unlines $ map (replaceWithLatex False) $ lines contents
  | otherwise = contents

getRange :: Map.Map String String
         -> Maybe (Int, Int)
getRange attrs = do
  start <- Map.lookup "startLine" attrs >>= readMaybe
  end <- Map.lookup "endLine" attrs >>= readMaybe
  if start <= end
     then return (start, end)
     else Nothing

withinLines :: Maybe (Int, Int) -> String -> String
withinLines range =
  case range of
    Just (start, end) ->
      unlines . take (end - startIndex) . drop startIndex . lines
      where startIndex = pred start
    Nothing           ->
      id

onlySnippet :: Map.Map String String -> String -> String
onlySnippet attrs =
  case Map.lookup "snippet" attrs of
    Just name ->
      unlines . takeWhile (not . isSnippetEnd) . drop 1 . dropWhile (not . isSnippetStart) . lines
      where
        isSnippetTag tag line = (tag ++ " snippet " ++ name) `isInfixOf` line
        isSnippetStart = isSnippetTag "start"
        isSnippetEnd = isSnippetTag "end"
    Nothing ->
      id

includeCode :: Maybe Format -> Block -> IO Block
includeCode _ cb@(CodeBlock (id', classes, attrs) _) = do
  let attrs' = Map.fromList attrs
  case Map.lookup "include" attrs' of
    Just f -> do
      fileContents <- readFile f
      let filteredStr = removeSpaces $ fileContents
                          & withinLines (getRange attrs')
                          & onlySnippet attrs'
          paramNames = ["include", "formatted", "startLine", "endLine", "snippet"]
          filteredAttrs = foldl (flip Map.delete) attrs' paramNames
          classes' = unwords classes
      return $ CodeBlock (id', classes, Map.toList filteredAttrs) filteredStr
    Nothing -> return cb
includeCode _ x = return x


--------------------------------------------------------------------------------
trim :: Int -> String -> String
trim i s@(' ':xs) = if i > 0 then trim (i - 1) xs else s
trim _ x = x

removeSpaces :: String -> String
removeSpaces str =
  concat $ intersperse "\n" $ fmap (trim $ codeIdentation 99 ls) ls
  where ls = lines str

codeIdentation :: Int -> [String] -> Int
codeIdentation init (a:as) = codeIdentation newInit as
  where
    currentIdent = lineIdentation a
    newInit = if currentIdent < init then currentIdent else init
codeIdentation x [] = x

lineIdentation :: String -> Int
lineIdentation []       = 99
lineIdentation (' ':cx) = 1 + (lineIdentation cx)
lineIdentation _        = 0


--------------------------------------------------------------------------------
main :: IO ()
main = toJSONFilter includeCode

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where
import           Text.Pandoc.JSON
import           Text.Pandoc.Walk
import qualified Data.Text                     as T

main :: IO ()
main = toJSONFilter runAll

runAll :: [Block] -> [Block]
--runAll b = b
runAll = map (sections . cleanBlock)

emptyAttrs :: (T.Text, [T.Text], [(T.Text, T.Text)])
emptyAttrs = ("", [], [])

-- cleanBlock removes things like the sidebar, and calls cleanInlines to remove whitespace and more
cleanBlock :: Block -> Block
cleanBlock block = case block of
  (Div (_, classname : _, _) _)
    | classname
      == "shortcuts"
      || classname
      == "sidebar-elems"
      || classname
      == "theme-picker"
      || classname
      == "infos"
      || classname
      == "search-container"
      || classname
      == "sidebar-menu"
      || classname
      == "logo-container"
      || classname
      == "toggle-wrapper"
    -> Null

  (Div (_, [_, section], _) blocks) | section == "small-section-header" -> Div emptyAttrs $ map cleanBlock blocks

  (Para [Link _ [] _]) -> Null

  (Plain ((Link (_, [name], _) _ _) : _))
    | name == "sidebar-title" || name == "test-arrow" -> Null

  (Div (tag, _, _) bs)
    | tag
      == "main"
      || tag
      == "search"
      || tag
      == "imlementation-list"
      || tag
      == "synthetic-implementations-list"
      || tag
      == "blanket-implementations-list"
    -> Div emptyAttrs $ map cleanBlock bs

  (Plain [Span _ [Str panics]]) | panics == "Panics" -> Null
  (Plain inlines) -> Plain $ cleanInlines $ filter
    (\case
      (Link (_, [classname], _) _ _)
        | classname == "srclink" || classname == "collapse" -> False

      _ -> True
    )
    inlines

  (Header 1 (panics, _, _) ins) | T.isPrefixOf "panics" panics ->
    Plain $ cleanInlines ins

  (CodeBlock (_, [linenumbers], _) _) | linenumbers == "line-numbers" -> Null
  (CodeBlock _ t) -> Para [Str "#+BEGIN_SRC rust \n", Str t, Str "\n#+END_SRC"] -- This lets us use syntax highlighting
  (Header 1 _ [Str panics]) | panics == "Panics" -> Null
  (Para ins                ) -> Para $ cleanInlines ins
  (Header 4 _ [Code _ _]) -> Null
  (Header _ _    []        ) -> Null
  (Div _ [Header 4 _ [], Plain [Span _ []]]) -> Null
  (Header 1 _ [Link _ [Str examples] _]) | examples == "Examples" -> Null
  (Header a attr ins    )    -> Header a attr (cleanInlines ins)

  (Div attr blocks      )    -> Div attr (map cleanBlock blocks)
  (BlockQuote blocks    )    -> BlockQuote (map cleanBlock blocks)
  (BulletList blocklists)    -> BulletList blocklists

  _                          -> block

-- Amongst other things, removes some causes of unwanted linebreaks, for example space leads to linebreaks sometimes so it's replaced with Str " "
cleanInlines :: [Inline] -> [Inline]
cleanInlines = foldr
  (\x acc -> case x of
    Str src | src == "[src]" -> acc
    Space              -> Str " " : acc
    LineBreak          -> Str " " : acc
    SoftBreak          -> Str " " : acc
    (Link _ _ (url, _)) | url == "#required-sections" -> acc
    (Link _ (_ : (Span (_, [inner], _) _) : _) _) | inner == "inner" -> acc
    (Link _ is _) -> Span emptyAttrs (cleanInlines is) : acc
    (Span _ []       ) -> acc
    (Span (_, [classname], _) _)
      | classname == "since" || classname == "emoji" -> acc
    (Span attr is ) -> Span attr (cleanInlines is) : acc
    (Strong    ins) -> Strong (cleanInlines ins) : acc
    (Emph      ins) -> Emph (cleanInlines ins) : acc
    (Strikeout ins) -> Strikeout (cleanInlines ins) : acc
    _               -> x : acc
  )
  []

-- This makes [inline] of This is a nightly-only experimental API, so that it doesn't become a header.
mkNotice :: [Block] -> [Inline]
mkNotice = foldr
  (\x acc -> case x of
    (Plain ins     ) -> ins ++ acc
    (Para  ins     ) -> ins ++ acc
    (Div _ bs      ) -> mkNotice bs ++ acc
    (Header _ _ ins) -> ins ++ acc
    _                -> acc
  )
  []


sections :: Block -> Block
sections b = case b of
  (Plain [Link _ _ (_, _), Code (_, _, _) code]) ->
    Header 3 emptyAttrs [Code emptyAttrs code]
  (Div (_, classnames, _) blocks) | "stability" `elem` classnames ->
    Plain $ mkNotice blocks

  (Header 3 (_, [classname], _) (code : (Link _ _ (url, _)) : _))
    | classname == "impl" -> Header 2
                                    emptyAttrs
                                    [Link emptyAttrs [code] (url, "")]
  (Header 1 _ ins) -> Header 1 emptyAttrs $ foldInlines ins

  (Header l _ ins) -> Header (l - 1) emptyAttrs $ foldInlines ins

  (Div (_, [docblock], _) docs) | docblock == "docblock" -> Div emptyAttrs docs
  _ -> b

 where
  foldInlines = foldr
    (\x acc -> case x of
      (Span _ [Str src]) | src == "[src]" -> acc
      (Code _ desc) | T.isPrefixOf "#[must" desc ->
        let descNoMustUse = T.drop 1 (T.dropWhile (/= ']') desc)
            mustUse =
                T.takeWhile (/= ']') (T.drop 1 (T.dropWhile (/= '=') desc))
        in  Code emptyAttrs descNoMustUse : Str " " : if T.length mustUse > 3
              then Note [Plain [Str mustUse]] : acc
              else acc
      (Span attr ins) -> Span attr ins : acc
      x               -> x : acc
    )
    []

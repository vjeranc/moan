moan
===================

This package provides a morphosyntactic analyzer that is language-agnostic in a sense that if language uses positional tags (ex. [Multext East][MultextEast], [Polish tagset][nkjp-tagset] etc.) this analyzer can be used. It is also possible to use it for part-of-speech tags only.

It solves a problem of providing a set of possible tags for a given word. Instead of just matching on the word-set pair, one can assume that suffixes of an unknown word also hold some information about the set.

For detailed documentation of the internals checkout the package on [Hackage](http://hackage.haskell.org/package/moan).

Usage
===================

This library provides the functionality of that kind of tags-per-word analysis. One example of where this might be useful is [concraft] tagging library. The analyzer is used in the [concraft-hr] MSD-tagger. Before the POS-tagging one needs to have a set of possible tags for a word from which the correct one is disambiguated. Although, it is assumed that the sentences are separated and tokenized.

For a sufficiently large construction corpus this analyzer might only benefit from additional regular expressions for easily matched tokens or matchers for punctuation and numbers. There is a possibility of returning a set of possible tags that isn't complete - the set doesn't contain a correct tag. If construction corpus isn't sufficiently large, there might be a fair amount of incomplete sets on unseen named entities (person names, corporation names etc.).

If one needs the analyzer to be less aggressive, it is recommended to extend the functionality and remove the sets of possible tags from words which might be named (ex. capitalized words in the middle of a sentence, this can be added in form of auxiliary functions in the future). This is present mostly in use cases where part-of-speech tags of a language contain information whether a word represents a named entity or not, so if this is not a case, there will be no need to extend the current functionality.

A simple example of using `GHCi` for construction:

```Haskell
:set -XOverloadedStrings
import NLP.Morphosyntax.Analyzer
import qualified Data.Text.IO as T
import qualified Data.Tagset.Positional as P
f <- readFile "tagset.cfg"
let tset = P.parseTagset "tagset-name" f
f <- T.readFile "construction-corpus.txt"
let train = map (\(word:tags) -> (word, map (P.parseTag tset) tags)) . map T.words . filter (not . T.null) . T.lines $ f
let conf = AConf 3 [(Punct, S.fromList [P.parseTag tset "Z"])] M.empty
--   ^^ "Z" is a Croatian tag for punctuation
let an = create tset conf train
save "analyzer.gz" an
```
It is assumed that tag attributes are separated with `:` for `P.parseTag`. One could write a different parsing function since `P.Tag` is just a `Data.Map`.

[concraft]: https://github.com/kawu/concraft
[concraft-hr]: https://github.com/vjeranc/concraft-hr
[nkjp-tagset]: http://nkjp.pl/poliqarp/help/ense2.html
[ru-tagset]: http://ufal.mff.cuni.cz/~hana/morph/rutags.html
[MultextEast]: http://nl.ijs.si/ME/ "Multext East"
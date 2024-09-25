# Glimpse

Glimpse is a small CNL for validating the contents of key-value maps.

Its syntax is meant to look like natural English, while evaluating unambiguously and maintaining a tiny "surface area" to facilitate learning.

Glimpse is formatting-sensitive. **Bold text** indicates value access. _Italic text_ is a comment, and will be ignored by the compiler.

The EBNF grammar is located in `src/core.clj`.

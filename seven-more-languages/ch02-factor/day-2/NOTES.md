## Defining Words

* Words to Factor is like functions / procedures to many other programming languages.
* To declare a word, one should write down its stack effect and give the code for the word.
* Stack effects can take some arbitrary names, but they are not used in the code of the word.
It is preferred to follow some convensions of naming (e.g. `str`, `obj`, `seq` ... ).

## Vocabularies

* Use `IN: <vname>` to define a vocabulary. Vocabularires work like modules or packages in
other programming languages.

* `USE: <vname>` can be used to import vocabularies one at a time
* `USING: <vname1> <vname2> ... ;` imports multiple vocabularies.

## Scripting

* To begin scripting in factor, one should setup `.factor-roots` file in his home directory.
  The file contains a list of loading paths.
  To run scripts in this repo, you should append one line to your `.factor-roots` file that
  looks like: `/path/to/thinking-dumps/seven-more-languages/ch02-factor`

* Note that `MAIN: <word>` accepts a word that has stack effect `( -- )`,
  and use it as an entry point to the program. If you do `MAIN: word1 word2`,
  it actually means `MAIN: word1` and then `word2`.
  Therefore in this case `word2` is a top level code, which executes before entering entry
  point. (This could be confusing)
* For now I think the easiest way to script in Factor is to just define and use words on top level.
  As defining a new word to serve as `MAIN` just adds up boilerplate and does nothing really useful.

This is a simple Common Lisp program which implements the technique
for categorizing texts described in

Cavnar, W. B. and J. M. Trenkle, "N-Gram-Based Text Categorization" In
Proceedings of Third Annual Symposium on Document Analysis and
Information Retrieval, Las Vegas, NV, UNLV Publications/Reprographics,
pp. 161-175, 11-13 April 1994.

It is a straightforward port of the Perl version, which is available
under the LGPL at http://www.let.rug.nl/~vannoord/TextCat/.


CL-TEXTCAT provides a single function, TEXTCAT:CLASSIFY. It takes an
input text and returns, as multiple values, its best guesses for the
language of the text:

     (classify "how now brown cow")
     => :EN

It returns ISO639 alpha-2 (en, es) codes for languages that have them, and
alpha-3 codes (eng, spa) for languages that do not.


(The language models included with the original Perl version, although
they have been widely taken as gospel, were never intended for
production use. The language models in this repository were computed
using the corpus of one hundred Bible translations at
http://homepages.inf.ed.ac.uk/s0787820/bible/. The corpus is not
included, but the code used to generate the language models (and the
alpha-3 to alpha-2 translation table) is in `parser.lisp'.)

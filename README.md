# futile

### Resistance is futile! An opinionated functional utilities library for Common Lisp - powered by FSet 

### Q&A

*What makes this library opinionated?*
Heavy and liberal use of the FSet library and total disregard for any kind of imperative nonsense. This package assumes you use FSet heavily also.
The main purpose of this library is to expand upon that library and add utilities that were missing from FSet, and to make shorthand conventions for things that are quite verbose. I also wanted to bring in a lot of things that I liked from Clojure into Common Lisp (such as condp, ->, and arity overloading)

*What, no pattern matching?*
Common lisp already has great pattern matching libraries (trivia) and I haven't got enough knowledge of them to offer any expansions. Personally I don't use pattern matching. I just wanted to offer some new kinds of polymorphism.

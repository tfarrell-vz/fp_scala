## Handling errors without exceptions

### What's the big deal?

Our path so far has been a tale of immutability. One of the reasons for studying and implementing immutable data structures and algorithms has been to enable referential transparency. That is, to allow us to reason about a program without having to worry about the contextual dependence of expressions.

The glaring fault of exceptions, while traditionally quite useful, is that they break referential transparency.


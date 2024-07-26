# fino

this is being rewritten in rust here: https://github.com/05st/fino
i would love to use haskell for it but unfortunately the state of llvm libraries for haskell is completely atrocious

a functional programming language.

todo:
- [X] Syntax, tokenization, parsing
- [X] Basic module system
- [X] Module verification (topological sort, detect cycles)
- [X] Name resolution
- [X] User-defined prefix, infix, and postfix operators
- [X] Function call analysis (find strongly connected components and topological sort)
- [X] Hindley-Milner type inference
- [X] Algebraic data types
- [X] Records with scoped labels
- [ ] Traits (i.e. Haskell typeclasses)
- [ ] Monomorphization
- [ ] Closure conversion / lambda lifting
- [ ] Code generation (C probably)
- [ ] ...

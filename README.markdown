# 🦠 algaett’s not algaeff

This development is an experiment with the following goals:
1. Adopt smalltt and related techniques into the cubical world.
2. Show how various OCaml packages of ours fit together.
3. Write natural grammars without neccesarily conforming to LR(k).
4. Use lots of Unicode emojis.

## Smalltt

The core NbE algorithm closely follows [András Kovács’s smalltt.](https://github.com/AndrasKovacs/smalltt)
Here are some notable differences:

1. We intentionally do not implement unification.
2. The universe itself (as a term) is not inferable, which means that the checking might have to be redone with the type unfolded.
   ```
   📌 😄 : 🌌 🆙 2️⃣ 👉 🌌 🆙 1️⃣
   📌 _ 👉 🌌 : 😄
   ```
   The type inference from the universe 🌌 will fail, and then the type checking will be redone with 😄 unfolded to 🌌 🆙 1️⃣.
3. The conversion checker is generalized to handle subtyping generated by cumulativity.

## Our OCaml packages

- [algaeff](https://redprl.org/algaeff/algaeff/Algaeff): reusable effect-based components
- [bantorra](https://redprl.org/bantorra/bantorra/Bantorra): unit resolution _(not used yet)_
- [bwd](https://redprl.org/bwd/bwd/Bwd): backward lists
- [mugen](https://redprl.org/mugen/mugen/Mugen): universe levels
- [yuujinchou](https://redprl.org/yuujinchou/yuujinchou/Yuujinchou): namespaces

## Beyong LR

We are using the Earley’s parsing algorithm which can handle all context-free grammars.

## Documentation

[Here is the API documentation.](https://redprl.org/algaett/algaett/)

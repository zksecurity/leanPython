# Lython

**Warning: This codebase was vibe-coded in a few hours with Claude. Do not use this for anything of value.**

No theorem proving is involved yet. This is plain Lean4 code that happens to be written in a language with a proof assistant — we're not using that capability at this stage.

Lython is a tree-walking Python 3.12 interpreter written in Lean4. The end goal is to interpret the [leanSpec](https://github.com/leanEthereum/leanSpec) Ethereum consensus specification, making Python specs usable from within Lean4. Right now it can run basic Python programs with arithmetic, control flow, functions, lists, dicts, comprehensions, and a handful of built-in functions.

## Running hello world

Install [elan](https://github.com/leanprover/elan) (the Lean version manager):

```bash
curl https://elan.lean-lang.org/elan-init.sh -sSf | sh
```

Clone and build:

```bash
git clone https://github.com/zksecurity/Lython.git
cd Lython
lake build
```

The first build downloads the Lean toolchain and compiles everything — this takes a few minutes.

Write a Python file and run it:

```bash
echo 'print("Hello, World!")' > hello.py
lake exe lython hello.py
```

Run the test suite:

```bash
lake test
```

## Contributing

The codebase is still rapidly changing and the author is pushing to main directly. Please wait for things to stabilize before opening PRs.

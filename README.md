# Stimsym [![build status](https://travis-ci.org/c-cube/stimsym.svg?branch=master)](https://travis-ci.org/c-cube/stimsym)

Rewrite system designed for symbolic manipulations and maximal expressiveness.

Why?! Well, I see that Mathematica as a symbolic language has a lot of
nice features, and is rooted in term rewriting. However, because of
its price and closedness, I started this small project. The goal is certainly
not to reimplement all the complex mathematical primitives of Mathematica;
instead, I just want a terse, powerful language in a sandbox (here, a Jupyter
notebook or a CLI interface).

I drew a lot of inspiration from https://github.com/jyh1/mmaclone/, which
was also my reference for operator priorities for the expression parser.
Stimsym is not yet feature complete (it lacks AC matching, among others,
for now), but it has a Jupyter interface and I believe the architecture might
scale a bit better if lots of primitives were added. Most of the code
for the Jupyter frontend is adapted from
[IOCaml](https://github.com/andrewray/iocaml)
so as to use `lwt-zmq` and work with Jupyter 4.

## Build

I recommend using [opam](https://opam.ocaml.org). The following will
clone this repository, and use opam to install the dependencies and
the program itself

```
git clone https://github.com/c-cube/stimsym.git
cd stimsym
opam pin add -k git -n stimsym
opam install stimsym
```

For the jupyter frontend, some more dependencies are needed (see 'opam' to
see which one exactly, but it should be something along:
`opam install zmq atdgen yojson uuidm lwt lwt-zmq`).

## Usage

Once installed, you can either use the simple command line interface:

```mathematica
$ stimsym_cli
> 1+1
2
> f[1,2,g[3],4,5] //. f[l1__,l2__,g[x_],r__] :> h[r,x,l2,l1]
h[4,5,3,2,1]
> {f[a],f[b],f[c],f[d]} //. f[x_] /; ((x===a)||(x===c)) :> g[x]
List[g[a],f[b],g[c],f[d]]
```

### Some Examples

See [the list of commented examples](doc/examples.adoc) to
get an idea of how to use Stimsym.
The file 'tests/run_tests.ml' also contains a lot of small test cases.

Some notebooks can be found on
the [gallery branch](https://github.com/c-cube/stimsym/tree/gallery),
including [an improved version of the examples](https://github.com/c-cube/stimsym/blob/gallery/data/notebook_examples.ipynb).

### Jupyter Interface

Use the (experimental) library
[jupyter-kernel](https://github.com/ocaml-jupyter/jupyter-kernel).
Recommended way is
`jupyter kernelspec install data/ --name=stimsym --user --replace`
You can also copy manually 'data/kernel.json' into the directory
`~/.local/share/jupyter/kernels/stimsym`.

```
mkdir -p ~/.local/share/jupyter/kernels/stimsym
cp data/kernel.json ~/.local/share/jupyter/kernels/stimsym/

opam pin add -k git jupyter-kernel https://github.com/ocaml-jupyter/jupyter-kernel.git
```

Start the jupyter notebook with `jupyter-notebook`;
you can find a sample notebook in 'data/notebook1.ipynb'. The `make jupyter`
target will open the sample notebook.

## License

BSD license, you can modify as you like. Contributions are welcome.

## Hacking

Almost everything in the language itself can be found in `src/core/Expr.ml`,
`src/core/Eval.ml{,i}` and `src/core/Pattern.ml{,i}`;
the primitives are in `src/core/Builtins.ml`.
The rest is about parsing, CLI, jupyter frontend, etc. The code is relatively
naive and will certainly not perform well, and there is a lot of room for
algorithmic improvement.

If (who knows?) you are interested in hacking on this in any way, do not
hesitate to contact me or just say "hi" on IRC. I'd be interested in
discussing or helping.

## Why the name?

It is a reference to Dan Simmons' _Hyperion_ series of books, with the
suffix "sym" altered because, hey, we're doing _symbolic_ stuff around here!


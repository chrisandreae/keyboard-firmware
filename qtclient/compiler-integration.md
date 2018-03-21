**Compiler integration is unpolished.**

The `compiler` should be built with

    cabal configure
    cabal build

This will produce `libHSCompiler-0.0-mumble.dylib`.

In `qtclient`

    ./integrate-compiler.sh > compiler.pri
    /usr/local/opt/qt5/bin/qmake USE_COMPILER=1
    make -j5
    /usr/local/opt/qt5/bin/macdeployqt KeyboardClient.app

Where `integrate-compiler.sh` resolves headers, libraries, and
converts @rpath links to absolute paths for the benefit of
macdeployqt. It complains about an unexpected prefix `@loader_path`,
but still works.

If you find a tutorial that suggests you `hs_add_root(__stginit_Foo)`,
that hasn't been necessary since 2011, according to
[this commit](https://git.haskell.org/ghc.git/commitdiff/a52ff7619e8b7d74a9d933d922eeea49f580bca8). The
current `__stginit_Foo` symbols include the library hash. I don't know
if it's feasible to embed it.

### Future Work

 * Why does the cabal file have to include transitive dependencies?
   Removing them produced unresolved symbols for the `BlockIR`,
   `BasicTypes`, etc.

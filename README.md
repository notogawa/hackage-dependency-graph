

Build
=====

    $ sudo emerge -v closure-compiler-bin
    $ git clone git://github.com/notogawa/hackage-dependency-graph.git
    $ cd hackage-dependency-graph
    $ git submodule init
    $ git submodule update
    $ ln -s /opt/closure-compiler-bin-0/lib/closure-compiler-bin.jar ext/sigma.js/build/compiler.jar
    $ make -C ext/sigma.js
    $ cd js
    $ ln -s ../ext/sigma.js/build/*.js .
    $ ln -s ../ext/sigma.js/plugins/*.js .
    $ cd ..
    $ cabal update
    $ cabal install hakyll xml-conduit
    $ ghc updater
    $ ghc hakyll
    $ ./update.sh
    $ ./updater
    $ ./hakyll rebuild

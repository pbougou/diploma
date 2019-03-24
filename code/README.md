## Build using [Stack](http://docs.haskellstack.org/en/stable/install_and_upgrade) : 
Works (tested) with:
* Version 1.9.3
* Version 1.6.5
* Probably works with all stack versions.

To build and execute the project, run:
* `stack build`
* `stack exec code-exe < filename`
* To enable debug build with:
    * `stack build --ghc-options=-DDEBUG`
    * or uncomment `cpp-options=-DDEBUG` in package.yaml
        
#### Example syntax in [examples](./examples)

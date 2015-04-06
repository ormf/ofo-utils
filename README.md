# ofo-utils

Utilities for overtone (VUmeters, sample-players, misc.). Some parts
of the code contain modified code from overtone. References are
specified in the sources.

### Installation

```sh
    # Install lein2
    # https://github.com/technomancy/leiningen

    $ lein new ofo-utils-test

    # add the following dependencies to ofo-utils-test/project.clj
    # [org.clojure/clojure "1.6.0"]
    # [ofo-utils "0.1.0-SNAPSHOT"]

    $ cd ofo-utils-test
    $ lein repl
```

### Making sounds


```clj
    ;; boot the server
    user=> (use 'overtone.live)

    ;; load the utils
    user=> (use 'ofo-utils.core)

    ;; add a Stereo VUmeter
    user=> (vumeter)

    ;; add another 16 channel VUmeter, listening on supercollider
    ;; buses 0 through 15

    user=> (vumeter (range 16))

    ;; add a 4 channel VUmeter, listening on supercollider's first two
    ;; input and output buses

    user=> (vumeter [8 9 0 1])

    ;; listen to the joys of a simple sine wave and watch its output
    ;; in the vumeter(s)
    
    user=> (demo (sin-osc))

    ;; load a sampled-piano synth (this is an extended sampled piano
    ;; from standard overtone, enabling float values for microtonal
    ;; midi values and an optional duration argument. Beware this
    ;; takes a long time to load for the first time as the samples
    ;; have to get downloaded to the sample pool on the local
    ;; harddrive first.

    user=> (use 'ofo-utils.synth.sampled-piano)
    
    ;; play a midi c
    user=> (sampled-piano 60)

    ;; play a midi c plus one quartertone
    user=> (sampled-piano 60.5)

    ;; play a staccato note
    user=> (sampled-piano 60 :dur 0.1)




```



## License

Open Source Initiative OSI - The MIT License:Licensing

The MIT License

Copyright (c) 2012 <All contributors listed in the README.md file>.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
    

Copyright © 2015 Orm Finnendahl (and Sam Aaron for the copied parts)

Distributed under the MIT License (see LICENSE)

## Contributors

* Orm Finnendahl



# ada-irc

ada-irc is an MIT licensed Ada framework for IRC bot creation. See `examples/`
for a few examples of how to use the framework.

## building

Uses GNAT make.

* `make all` to create a release build.
* `make debug` for a debug build.
* `make test` to build the test bots in `examples/`. Placed into `examples/bin`.

## dependencies

ada-irc's only dependency is the GNAT runtime and an Ada 2005
compiler, as it makes heavy use of GNAT's socket and regular
expression libraries, and does a few things unique to Ada 2005
(`limited with`, etc.)

## license

(The MIT License)

Copyright (c) 2012 Erik Price

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


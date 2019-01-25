[![Build Status](https://img.shields.io/travis/netorcai/netorcai-client-fortran/master.svg?maxAge=600)](https://travis-ci.org/netorcai/netorcai-client-fortran)
[![Coverage Status](https://img.shields.io/codecov/c/github/netorcai/netorcai-client-fortran/master.svg?maxAge=600)](https://codecov.io/github/netorcai/netorcai-client-fortran)

netorcai-client-fortran
=================
FORTRAN 2003 version of the [netorcai] client library.

Installation
============
The library uses the [Meson] build system (and therefore uses [Ninja]).

``` bash
# Create build directory.
meson build # --prefix=/desired/installation/prefix

# Build the library (and the unit test executables) in the build directory.
ninja -C build

# Run the unit tests
ninja -C build test

# Install the library
ninja -C build install
```

[netorcai]: https://github.com/netorcai/
[Meson]: https://mesonbuild.com/
[Ninja]: https://ninja-build.org/

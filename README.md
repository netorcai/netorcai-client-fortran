netorcai-client-fortran
=================
FORTRAN version of the [netorcai] client library.

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

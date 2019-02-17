# Meson dependencies does not work with FORTRAN
# See https://github.com/mesonbuild/meson/issues/4223

.PHONY: hello-player

all: hello-player

hello-player: src/main.f90
	$(FC) $^ -o $@ $(shell pkg-config netorcai-client-fortran --cflags --libs)

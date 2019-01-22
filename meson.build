# Composition failure bug
# See: https://github.com/mesonbuild/meson/issues/1489

project('netorcai_fortran', 'fortran', version: '1.0.0')

sources = ['src/stdc.f90', 'src/socket.f90', 'src/client.f90', 'src/main.f90']
fson  = dependency('fson',  version: '>=1.0.0', fallback: ['fson', 'fson_dep'])
executable('netorcai_fortran', sources, dependencies: [fson], install: true)


# Composition failure bug
# See issue: https://github.com/mesonbuild/meson/issues/1489

project('netorcai_fortran', 'fortran', version: '1.0.0')

# Remove stupid complaints of GCC in FORTRAN
# (they MAY happend but never in practice and cannot be avoided)
if meson.get_compiler('fortran').get_id() == 'gcc'
    fArgs = ['-Wno-maybe-uninitialized']
else
    fArgs = []
endif

# Possible bug or feature of FORTRAN:
# The order seems to be important (files should be put after its dependencies)
sources = [
    'src/stdc.f90',
    'src/socket.f90',
    'src/process.f90',
    'src/utils.f90',
    'src/vector.f90',
    'src/json.f90',
    'src/proto_version.f90',
    'src/message.f90',
    'src/client.f90'
]
zofu = dependency('zofu',  version: '>=0.1.0', fallback: ['zofu', 'zofu_dep'])
netorcai_fortran = shared_library('netorcai_fortran', sources, fortran_args: fArgs, install: true)
netorcai_fortran_dep = declare_dependency(link_with: netorcai_fortran)


    # For the installation

pkg = import('pkgconfig')
pkg.generate(name: 'netorcai-client-fortran',
    libraries: netorcai_fortran,
    version: meson.project_version(),
    description: 'A netorcai client library written in object-oriented FORTRAN'
)

# Modules files are not installed properly
# See issues: https://github.com/mesonbuild/meson/issues/4708
prefix = get_option('prefix')
libdir = get_option('libdir')
includedir = get_option('includedir')
moduleInstallDir = join_paths(prefix, includedir)
objs = []
foreach srcFile: sources
    tmp = srcFile.split('src/')
    assert(tmp.length() == 2 and tmp[0] == '', 'bad path')
    objs += ['src_' + tmp[1] + '.o']
endforeach
install_subdir(join_paths('build', 'netorcai_fortran@sha'),
                install_dir: moduleInstallDir,
                strip_directory: true,
                exclude_files: objs + ['libnetorcai_fortran.so.symbols'])


    # For the unit-tests

testNames = ['utils', 'json', 'basic']

zofuDriver = find_program('zofu-driver', required: false)
if not zofuDriver.found()
    zofuDriver = subproject('zofu').get_variable('driver_exe')
endif

foreach testName: testNames
    testSrc = join_paths(meson.current_source_dir(), 'src', 'tests', testName + '.f90')
    testLib = static_library(
        testName + '_lib', [testSrc],
        fortran_args: fArgs,
        dependencies: [netorcai_fortran_dep, zofu])
    testDriverSrc = custom_target(
        testName + '_driver',
        input: testSrc,
        output: testName + '_driver.f90',
        command: [zofuDriver, '@INPUT@', '@OUTPUT@']
    )
    unitTest = executable(
        testName, [testDriverSrc],
        fortran_args: fArgs,
        link_with: [testLib],
        dependencies: [zofu]
    )
    test(testName, unitTest)
endforeach


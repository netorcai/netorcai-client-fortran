# Composition failure bug
# See: https://github.com/mesonbuild/meson/issues/1489

project('netorcai_fortran', 'fortran', version: '1.0.0')

# Possible bug or feature of FORTRAN:
# The order seems to be important (files should be put after its dependencies)
sources = [
    'src/stdc.f90',
    'src/socket.f90',
    'src/process.f90',
    'src/utils.f90',
    'src/json.f90',
    'src/message.f90',
    'src/client.f90'
]
fson = dependency('fson',  version: '>=1.0.0', fallback: ['fson', 'fson_dep'])
zofu = dependency('zofu',  version: '>=0.1.0', fallback: ['zofu', 'zofu_dep'])
netorcai_fortran = shared_library('netorcai_fortran', sources, dependencies: fson, install: true)
netorcai_fortran_dep = declare_dependency(link_with: netorcai_fortran)

    # For unit-testing

testNames = ['json', 'basic']

zofuDriver = find_program('zofu-driver', required: false)
if not zofuDriver.found()
    zofuDriver = subproject('zofu').get_variable('driver_exe')
endif

foreach testName: testNames
    testSrc = join_paths(meson.current_source_dir(), 'src', 'tests', testName + '.f90')
    testLib = static_library(
        testName + '_lib', [testSrc], 
        link_with: netorcai_fortran,
        dependencies: [fson, zofu])
    testDriverSrc = custom_target(
        testName + '_driver',
        input: testSrc,
        output: testName + '_driver.f90',
        command: [zofuDriver, '@INPUT@', '@OUTPUT@']
    )
    unitTest = executable(
        testName, [testDriverSrc, testSrc],
        link_with: [netorcai_fortran, testLib],
        dependencies: [fson, zofu]
    )
    test(testName, unitTest)
endforeach


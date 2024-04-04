set(unit-test-prefix "${unit-test-prefix}/math")

set(tests
  matrixops)

foreach(test IN LISTS tests)
  dftbp_preprocess("${FYPP}" "${FYPP_FLAGS}" "F90" "f90" "${test}.F90" resfile)
  add_executable(${test}_testapp ${resfile})
  target_link_libraries(${test}_testapp dftbplus Fortuno::fortuno_serial)
  add_test(NAME ${unit-test-prefix}/${test} COMMAND ${test}_testapp)
endforeach()

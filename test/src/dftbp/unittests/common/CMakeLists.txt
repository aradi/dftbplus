set(unit-test-prefix "${unit-test-prefix}/common")

set(tests
  memman
  file
  schedule)

foreach(test IN LISTS tests)
  fytest_fypp_preprocess("${FYPP_FLAGS}" ${test}.F90 sources-f90)
  add_executable(${test} ${sources-f90})
  target_link_libraries(${test} dftbplus FyTest::FyTest)
  add_test(NAME ${unit-test-prefix}/${test} COMMAND ${test})
endforeach()


set(tests
  atomicmass)

foreach(test IN LISTS tests)
  dftbp_preprocess("${FYPP}" "${FYPP_FLAGS}" "F90" "f90" "${test}.F90" resfile)
  add_executable(${test}_testapp ${resfile})
  target_link_libraries(${test}_testapp dftbplus Fortuno::fortuno_serial)
  add_test(NAME ${unit-test-prefix}/${test} COMMAND ${test}_testapp)
endforeach()

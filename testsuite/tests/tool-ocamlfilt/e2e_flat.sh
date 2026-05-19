#!/bin/sh
# End-to-end test: compile a comprehensive OCaml source with the
# flat name-mangling scheme and verify that ocamlfilt --format flat1
# can demangle every OCaml linker symbol that appears in the
# generated assembly.
set -e

OCAMLFILT="${ocamlsrcdir}/tools/ocamlfilt"
ASM="${test_build_directory}/e2e_ocaml.s"

if [ ! -f "${ASM}" ]; then
  echo "missing assembly file ${ASM}" > "${ocamltest_response}"
  exit "${TEST_FAIL}"
fi

SYMS=$(grep -oE '_?caml[A-Z][A-Za-z0-9_$.]*' "${ASM}" | sort -u)

if [ -z "${SYMS}" ]; then
  echo "no OCaml symbols found in ${ASM}" > "${ocamltest_response}"
  exit "${TEST_FAIL}"
fi

ERR_OUT="${test_build_directory}/e2e_ocaml.flat.demangle.err"
OK_OUT="${test_build_directory}/e2e_ocaml.flat.demangle.out"
if ! printf '%s\n' "${SYMS}" | \
       "${OCAMLFILT}" --format flat1 \
         > "${OK_OUT}" 2> "${ERR_OUT}"; then
  {
    echo "ocamlfilt failed to demangle one or more symbols:"
    cat "${ERR_OUT}"
  } > "${ocamltest_response}"
  exit "${TEST_FAIL}"
fi

if [ -s "${ERR_OUT}" ]; then
  {
    echo "ocamlfilt reported errors on stderr:"
    cat "${ERR_OUT}"
  } > "${ocamltest_response}"
  exit "${TEST_FAIL}"
fi

exit "${TEST_PASS}"

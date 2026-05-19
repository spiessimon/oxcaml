#!/bin/sh
# End-to-end test: compile a comprehensive OCaml source with the
# structured name-mangling scheme and verify that ocamlfilt can
# demangle every OCaml linker symbol that appears in the generated
# assembly, including symbols that exercise the hex-escaped encoding
# path (custom operators, indexing operators) and the class (O) path
# item.
set -e

OCAMLFILT="${ocamlsrcdir}/tools/ocamlfilt"
ASM="${test_build_directory}/e2e_ocaml.s"

if [ ! -f "${ASM}" ]; then
  echo "missing assembly file ${ASM}" > "${ocamltest_response}"
  exit "${TEST_FAIL}"
fi

# On macOS the assembler emits names prefixed by an additional
# underscore (__Caml...); on other platforms the names appear as-is
# (_Caml...). The structured demangler accepts both prefixes.
SYMS=$(grep -o '__\{0,1\}Caml[A-Za-z0-9_]*' "${ASM}" | sort -u)

if [ -z "${SYMS}" ]; then
  echo "no OCaml symbols found in ${ASM}" > "${ocamltest_response}"
  exit "${TEST_FAIL}"
fi

ERR_OUT="${test_build_directory}/e2e_ocaml.demangle.err"
OK_OUT="${test_build_directory}/e2e_ocaml.demangle.out"
if ! printf '%s\n' "${SYMS}" | \
       "${OCAMLFILT}" --format structured \
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

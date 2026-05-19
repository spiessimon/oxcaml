#!/bin/sh
# End-to-end test for OxCaml-specific constructs. Invoked twice:
#   - "plain":  structured mangling, no pack prefix
#   - "packed": same, plus -for-pack Ox_pack
# Both runs must demangle cleanly. The "packed" run additionally
# checks that the pack prefix (joined to the unit name with the [__]
# separator from the flat scheme) appears inside the U payload, which
# is the currently CR-marked path in [Structured_mangling].
set -e

mode=${1:?"missing mode argument (plain|packed)"}
OCAMLFILT="${ocamlsrcdir}/tools/ocamlfilt"
ASM="${test_build_directory}/e2e_oxcaml.s"

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

ERR_OUT="${test_build_directory}/e2e_oxcaml.${mode}.demangle.err"
OK_OUT="${test_build_directory}/e2e_oxcaml.${mode}.demangle.out"
if ! printf '%s\n' "${SYMS}" | \
       "${OCAMLFILT}" --format structured \
         > "${OK_OUT}" 2> "${ERR_OUT}"; then
  {
    echo "ocamlfilt failed to demangle one or more symbols (${mode}):"
    cat "${ERR_OUT}"
  } > "${ocamltest_response}"
  exit "${TEST_FAIL}"
fi

if [ -s "${ERR_OUT}" ]; then
  {
    echo "ocamlfilt reported errors on stderr (${mode}):"
    cat "${ERR_OUT}"
  } > "${ocamltest_response}"
  exit "${TEST_FAIL}"
fi

exit "${TEST_PASS}"

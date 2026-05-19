#!/bin/sh
# End-to-end test of [--format auto]. Invoked twice from
# [e2e_ocaml.ml]: once after a structured compile, once after a flat
# compile. In each run we extract the OCaml-mangled symbols from the
# assembly file, decode them once with the scheme-specific format and
# once with [--format auto], and require the outputs to match.
set -e

scheme=${1:?"missing scheme argument (structured|flat)"}
OCAMLFILT="${ocamlsrcdir}/tools/ocamlfilt"
ASM="${test_build_directory}/e2e_ocaml.s"

if [ ! -f "${ASM}" ]; then
  echo "missing assembly file ${ASM}" > "${ocamltest_response}"
  exit "${TEST_FAIL}"
fi

case "${scheme}" in
  structured)
    pinned_format=structured
    SYMS=$(grep -o '__\{0,1\}Caml[A-Za-z0-9_]*' "${ASM}" | sort -u)
    ;;
  flat)
    pinned_format=flat1
    # Flat-scheme linker names: [caml] + uppercase + ASCII (with [.]
    # and [$] inside escaped fragments). On macOS the assembler emits
    # them with a leading underscore (_caml...); ocamlfilt accepts
    # either form.
    SYMS=$(grep -oE '_?caml[A-Z][A-Za-z0-9_$.]*' "${ASM}" | sort -u)
    ;;
  *)
    echo "unknown scheme '${scheme}' (expected structured|flat)" \
      > "${ocamltest_response}"
    exit "${TEST_FAIL}"
    ;;
esac

if [ -z "${SYMS}" ]; then
  echo "no OCaml symbols found in ${ASM}" > "${ocamltest_response}"
  exit "${TEST_FAIL}"
fi

PINNED_OUT="${test_build_directory}/e2e_auto.${scheme}.pinned.out"
AUTO_OUT="${test_build_directory}/e2e_auto.${scheme}.auto.out"

if ! printf '%s\n' "${SYMS}" | \
       "${OCAMLFILT}" --format "${pinned_format}" \
         > "${PINNED_OUT}" 2>/dev/null; then
  {
    echo "ocamlfilt --format ${pinned_format} failed on the ${scheme} corpus"
  } > "${ocamltest_response}"
  exit "${TEST_FAIL}"
fi

if ! printf '%s\n' "${SYMS}" | \
       "${OCAMLFILT}" --format auto \
         > "${AUTO_OUT}" 2>/dev/null; then
  {
    echo "ocamlfilt --format auto failed on the ${scheme} corpus"
  } > "${ocamltest_response}"
  exit "${TEST_FAIL}"
fi

if ! diff -u "${PINNED_OUT}" "${AUTO_OUT}" > /dev/null; then
  {
    echo "--format auto disagrees with --format ${pinned_format}"
    echo "  (scheme=${scheme})"
    diff -u "${PINNED_OUT}" "${AUTO_OUT}"
  } > "${ocamltest_response}"
  exit "${TEST_FAIL}"
fi

exit "${TEST_PASS}"

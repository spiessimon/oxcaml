#!/bin/sh
# Demangle every OCaml-mangled symbol in an object file with each
# ocamlfilt format and print a column-aligned table to stdout.
#
# Usage: sh e2e_table.sh <obj-file>
#
# The table has four columns: the raw linker symbol followed by its
# demangling under [--format flat1], [--format structured], and
# [--format auto]. Cells where ocamlfilt refuses the symbol show
# [(error)]. Compiler-generated stamps ([_NN_NN_code], [_NN_code],
# trailing [_NN], [PmakeblockNN], [const_blockNN], [iarrNN]) are
# masked so the reference stays stable across builds.
set -e

OBJ=${1:?"missing object file argument"}
OCAMLFILT="${ocamlsrcdir}/tools/ocamlfilt"
TAB="$(printf '\t')"

ocamlfilt_or_error () {
  "$OCAMLFILT" --format "$1" "$2" 2>/dev/null || echo "(error)"
}

SYMS="${test_build_directory}/e2e_table.syms"

# Keep only OCaml-mangled symbols that this object defines:
# flat ([caml] + uppercase) or structured ([_Caml] + uppercase),
# each optionally with the macOS leading underscore. Undefined
# references (nm type [U] or weak [u]) are dropped because their
# names are owned by other units (stdlib, Camlinternal*, etc.),
# not by the test source -- including them would make this
# reference more fragile to unrelated changes in the compiler.
nm "$OBJ" | awk '
  $(NF - 1) != "U" && $(NF - 1) != "u" \
    && ($NF ~ /^_?caml[A-Z]/ || $NF ~ /^_?_Caml[A-Z]/) { print $NF }
' | sort -u > "$SYMS"

if [ ! -s "$SYMS" ]; then
  echo "no OCaml symbols found in $OBJ" > "$ocamltest_response"
  exit "$TEST_FAIL"
fi

for fmt in flat1 structured auto; do
  COL="${test_build_directory}/e2e_table.${fmt}.col"
  : > "$COL"
  while IFS= read -r sym; do
    ocamlfilt_or_error "$fmt" "$sym" >> "$COL"
  done < "$SYMS"
done

{
  printf 'SYMBOL\tFLAT1\tSTRUCTURED\tAUTO\n'
  paste "$SYMS" \
    "${test_build_directory}/e2e_table.flat1.col" \
    "${test_build_directory}/e2e_table.structured.col" \
    "${test_build_directory}/e2e_table.auto.col"
} | sed -E \
    -e 's/_[0-9]+_[0-9]+_code/_N_N_code/g' \
    -e 's/_[0-9]+_code/_N_code/g' \
    -e 's/Pmakeblock[0-9]+/PmakeblockN/g' \
    -e 's/const_block[0-9]+/const_blockN/g' \
    -e 's/iarr[0-9]+/iarrN/g' \
    -e "s/_[0-9]+(${TAB}|$)/_N\\1/g" \
  | column -t -s "${TAB}"

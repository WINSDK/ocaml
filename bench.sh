#!/bin/bash
set -e

DUNE_FILE="bench/dune"
MAIN_FILE="bench/main.ml"
BACKUP_FILE="/tmp/dune.backup"

setup_runner() {
    if [ -e "$MAIN_FILE" ]; then
        echo "Error: can't have a file in 'bench/' be called 'main.ml'. Aborting."
        exit 1
    fi

    {
        echo "(* Auto-generated file *)"
        echo "let () ="
        for file in bench/*.ml; do
            if [ "$file" = "$MAIN_FILE" ]; then
                continue
            fi

            filename=$(basename "$file" .ml)
            echo "  Inline_benchmarks_public.Runner.main ~libname:\"$filename\";"
        done
    } > "$MAIN_FILE"


    cp "$DUNE_FILE" "$BACKUP_FILE"

    libraries=()

    # Parse the library sexp from the input.
    while read -r line; do
        case "$line" in
            "(name"*)
                lib_name=$(echo "$line" | sed -E 's/\(name (.+)\)/\1/')
                libraries+=("$lib_name");;
        esac
    done < "$DUNE_FILE"

    # Generate the executable sexp.
    {
        echo "(executable"
        echo " (name main)"
        echo " (modules main)"
        echo -n " (libraries"
        for lib in "${libraries[@]}"; do
            echo -n " $lib"
        done
        echo " core_bench.inline_benchmarks))"
    } >> "$DUNE_FILE"
}

cleanup_runner() {
    rm -f "$MAIN_FILE"

    if grep -q "(executable" "$DUNE_FILE"; then
        sed -i '' -e "/(executable/,/core_bench.inline_benchmarks))/d" "$DUNE_FILE"
    fi
}

# Makes sure the -clear-columns flag is always set.
flag_present=false
for arg in "$@"; do
    if [[ "$arg" == "-clear-columns" ]]; then
        flag_present=true
        break
    fi
done
if [[ "$flag_present" = false ]]; then
    set -- "-clear-columns" "$@"
fi

trap cleanup_runner EXIT
setup_runner;

export BENCHMARKS_RUNNER=TRUE

# Filter out broken warning from stderr.
dune exec --release -- "./bench/main.exe" -run-without-cross-library-inlining "$@" \
    2>&1 | grep -v "Warning: X_LIBRARY_INLINING is not set to true"

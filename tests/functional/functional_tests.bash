#!/usr/bin/bash

BINARY="./glados"
REF="petite"
ROOT="$(pwd)/tests/functional/files"


display_failure() {
    printf "Test %s \033[31mfailed\033[0m\n" "$1"
    if [[ -x "$(command -v batcat)" ]]; then
        batcat /tmp/diff.test
    else
        cat /tmp/diff.test
    fi
}

display_success() {
    printf "Test %s \033[32msucceeded\033[0m.\n" "$1"
}

display_warning() {
    printf "\033[33;1mWARNING\033[0m: %s\n" "$1"
}

display_score() {
    printf "\nRan tests successfully.\n"
    printf "Scores:\n"
    printf "\tSimple tests: %s\n" "$1"
    printf "\tNormal tests: %s\n" "$2"
}

run_test() {
    BASENAME=$(basename "$1")
    if [[ -f "$1" ]]; then
        printf "\n================================> RUNNING ${2^^} TEST \033[33m$BASENAME\033[0m. <================================\n\n"
        $REF < "$1" &> /tmp/ref.test
        $BINARY < "$1" &> /tmp/bin.test
        diff /tmp/ref.test /tmp/bin.test &> /tmp/diff.test
        if [[ $? -ne 0 ]]; then
            display_failure "$BASENAME"
            return 0
        else
            display_success "$BASENAME"
            return 1
        fi
        rm -f /tmp/ref.test /tmp/bin.test
    else
        display_warning "Test $BASENAME does not exist. Skipping."
        return 0
    fi
}

if [[ ! -z "$BINARY" ]]; then
    make
fi

SIMPLE="$ROOT/simple"
NORMAL="$ROOT/normal"

SIMPLE_SCORE=0
SIMPLE_TOTAL=0
NORMAL_SCORE=0
NORMAL_TOTAL=0

for f in $SIMPLE/*; do
    run_test "$f" "simple"
    SIMPLE_SCORE=$(($SIMPLE_SCORE + $?))
    SIMPLE_TOTAL=$(($SIMPLE_TOTAL + 1))
done

for f in $NORMAL/*; do
    run_test "$f" "normal"
    NORMAL_SCORE=$(($NORMAL_SCORE + $?))
    NORMAL_TOTAL=$(($NORMAL_TOTAL + 1))
done

display_score "$SIMPLE_SCORE/$SIMPLE_TOTAL" "$NORMAL_SCORE/$NORMAL_TOTAL"
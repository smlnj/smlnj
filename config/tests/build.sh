#!/bin/sh
#
# COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
# All rights reserved.
#
# Exercise the build's driver scripts without compiling the runtime or LLVM.

set -eu

SOURCE_ROOT=$(CDPATH='' cd -- "$(dirname -- "$0")/../.." && pwd)
TEST_DIR=$(mktemp -d "${TMPDIR:-/tmp}/smlnj-build-test.XXXXXX")
trap 'rm -rf "$TEST_DIR"' 0
trap 'exit 1' 1 2 3 15

eval "$(sh "$SOURCE_ROOT/config/_arch-n-opsys")"
failed=0

for test_case in unset-home inherited-home separate-install existing-heap
do
    source_dir=$TEST_DIR/$test_case
    install_dir=$source_dir
    if [ "$test_case" = separate-install ]; then
        install_dir=$TEST_DIR/installed
    fi
    foreign_dir=$TEST_DIR/foreign
    mkdir -p "$source_dir/config" "$install_dir/bin/.run" \
        "$source_dir/sml.boot.$ARCH-unix/smlnj/basis/.cm" \
        "$foreign_dir/bin/.run"
    cp "$SOURCE_ROOT/build.sh" "$source_dir/build.sh"
    for file in _arch-n-opsys _link-sml _run-sml _ml-build \
        _ml-makedepend _heap2exec preloads version srcarchiveurl unpack
    do
        cp "$SOURCE_ROOT/config/$file" "$source_dir/config/$file"
    done
    printf '#!/bin/sh\nexit 0\n' > "$source_dir/config/chk-global-names.sh"
    chmod +x "$source_dir/config/chk-global-names.sh"

    # Stand in for the newly built runtime, recording which runtime was used.
    cat > "$install_dir/bin/.run/run.$ARCH-$OPSYS" <<'EOF'
#!/bin/sh
printf 'local\n' >> "$TEST_RUNTIME_LOG"
boot=no
redump=no
heap=
for arg in "$@"
do
    if [ "$redump" = yes ]; then
        heap=$arg
        redump=no
        boot=yes
    fi
    case "$arg" in
        @SMLboot=*) boot=yes ;;
        @SMLheap=*) heap=${arg#@SMLheap=} ;;
        @CMredump) redump=yes ;;
    esac
done
if [ "$boot" = yes ]; then
    : > "$heap.$TEST_HEAP_SUFFIX"
fi
EOF
    chmod +x "$install_dir/bin/.run/run.$ARCH-$OPSYS"

    # An installed runtime must not be used with the new system's boot files.
    sed '1s|.*|#!/bin/sh|' "$SOURCE_ROOT/config/_arch-n-opsys" \
        > "$foreign_dir/bin/.arch-n-opsys"
    cat > "$foreign_dir/bin/.run/run.$ARCH-$OPSYS" <<'EOF'
#!/bin/sh
printf 'foreign\n' >> "$TEST_RUNTIME_LOG"
echo 'Installed runtime was used during the build' >&2
exit 1
EOF
    chmod +x "$foreign_dir/bin/.arch-n-opsys" \
        "$foreign_dir/bin/.run/run.$ARCH-$OPSYS"

    if [ "$test_case" = existing-heap ]; then
        mkdir -p "$install_dir/bin/.heap" "$install_dir/lib/smlnj/basis/.cm"
        : > "$install_dir/bin/.heap/sml.$HEAP_SUFFIX"
        ln -s .run-sml "$install_dir/bin/sml"
    fi

    if (
        unset SMLNJ_HOME CM_PATHCONFIG CM_DIR_ARC
        if [ "$test_case" != unset-home ]; then
            SMLNJ_HOME=$foreign_dir
            export SMLNJ_HOME
        fi
        TEST_RUNTIME_LOG=$source_dir/runtime.log
        TEST_HEAP_SUFFIX=$HEAP_SUFFIX
        export TEST_RUNTIME_LOG TEST_HEAP_SUFFIX
        cd "$source_dir"
        sh ./build.sh -install "$install_dir"
    ) > "$source_dir/build.log" 2>&1
    then
        if [ "$(grep -c '^local$' "$source_dir/runtime.log")" -eq 2 ] \
            && ! grep -q '^foreign$' "$source_dir/runtime.log" \
            && [ -r "$install_dir/bin/.heap/sml.$HEAP_SUFFIX" ]
        then
            echo "PASS $test_case"
        else
            echo "FAIL $test_case: unexpected runtime calls or missing heap"
            failed=1
        fi
    else
        echo "FAIL $test_case: build failed"
        tail -n 4 "$source_dir/build.log"
        failed=1
    fi
done

exit "$failed"

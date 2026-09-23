# Mixed Records

This branch is for introducing a new "mixed" record representation
to represent closures over variables that are floats or native ints

## Removing the `raw64` Object Tag

The following steps were taken to remove the (now redundant) `raw64` object
tag from the system.

* [**DONE**] Remove the `tag_raw64` tag value from the `ObjectDesc :> OBJECT_DESC`
    structure and the `RK_RAW64BLOCK` constructor from the `CPS.record_kind`
    datatype.  Any occurrence of the tag `tag_raw64` is replaced by `tag_raw`
    and `RK_RAW64BLOCK` is replaced by `RK_RAWBLOCK`.

    Note that the runtime treats the two tags identically, since we are no
    longer supporting 32-bit architectures.

    We can then create a new version of the compiler that does not generate
    `tag_raw64` descriptors.
    ``` console
    cd system
    cmb-make
    makeml
    installml -clean -boot
    cd ..
    build.sh -clean
    ```

* [**DONE**] tag this check point
    ``` console
    git tag -a rm-raw64-tag -m "tag_raw64 removed from compiler"
    git push origin tag rm-raw64-tag
    ```

* [**DONE**] Remove the `DTAG_raw64` descriptor tag from the runtime system.  This change
  can only be done once we have binfiles that do not use that descriptor tag.

* [**DONE**] tag this check point
    ``` console
    git tag -a rm-runtime-raw64-tag -m "tag_raw64 removed from runtime system"
    git push origin tag rm-runtime-raw64-tag
    ```

Now we are ready to add support for mixed records to the runtime system.

## Adding Mixed-Record Support to the Runtime

Issues:
* partition the length field of the descriptor into total length and pointer length
* add a new arena for mixed records, since (unlike for records), scanning them requires
  examining the object descriptors

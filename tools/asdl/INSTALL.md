# Installation guide for asdlgen

**asdlgen** is written in *Standard ML* (SML); it can be compiled using either
the SML/NJ or MLton implementations of SML.

The installation process involves first running the **configure** script
and then compiling and installing the program.

```sh
  ./configure
  make build
  make install
```

## Configuration using [SML/NJ](smlnj.org)

```sh
  ./configure
```

```sh
  SMLNJ_CMD=<path> ./configure
```

## Configuration using [MLton](mlton.org)

```sh
  ./configure --with-mlton
```

```sh
  ./configure --with-mlton=<path>
```

## Cleanup

To remove the intermediate files from the build:

```sh
  make clean
```
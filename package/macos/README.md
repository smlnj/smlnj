## SML/NJ Installer for macOS

This directory contains the infrastructure for building a macOS
installer for SML/NJ on Intel hardware.  The basic command

	./build-pkg.sh $VERSION

will build a signed installer package for the specified version (e.g.,
2022.1). This script assumes that the boot files have already been
uploaded to the standard distribution site

	http://smlnj.cs.uchicago.edu/dist/working/$VERSION/

and that the targets file is initialized correctly.

The signing of the package depends on the user having the correct
developer certificate in his/her keychain.  The script currently knows
about John Reppy's certificate, but the script can be generalized
by adding other user IDs.

In addition to the build script, the other important file is

	components/distribution_xml.in

which is used to generate the distribution.xml file that controls the
installer. See Apple's documentation for more details.

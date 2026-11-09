## SML/NJ Installer for macOS

This directory contains the infrastructure for building a macOS
installer for SML/NJ.  The basic command

	./build-pkg [ options ] $VERSION

will build a signed installer package for the specified version (e.g.,
2026.2). This script assumes that the boot files have already been
uploaded to the standard distribution site

	http://smlnj.cs.uchicago.edu/dist/working/$VERSION/

and that the targets file is initialized correctly.

The optional arguments are
```
  -h, --help       show this help message and exit
  -v, --verbose    output information as the build proceeds
  --info <file>    specify the package info JSON file
  --no-clean       do not remove the intermediate files after packaging
  --build-only     build the distribution in the 'smlnj' directory, but do
                   not sign, package, or notarize it.
  -s, --sign       sign, package, and notarize the distribution in the
                   'smlnj' directory.
```

In addition to the build script, the other important file is

	components/distribution_xml.in

which is used to generate the distribution.xml file that controls the
installer. See Apple's documentation for more details.

### The `package-info.json` file

The signing and notarization of the installation package requires an Apple
developer account and other identity information.  Rather than include this
information in the `build-pkg.py` script, we load it from a **JSON** file.
By default, this file is `~/Library/SMLNJ/package-info.json`, but it can also
be specified using the `--info` option.

The file should contain a single **JSON** object with the following fields:

  * `application_id` -- this string field holds the name of the application
    certificate used to sign the executables in the distribution.  It typically
    has the form `"Developer ID Application: <name> (<team id>)"`.  This
    certificate can be created in **Xcode**.

  * `installer_id` -- this string field holds the name of the installer
    certificate used to sign the package in the distribution.  It typically
    has the form `"Developer ID Installer:  <name> (<team id>)"`.  This
    certificate can be created in **Xcode**.

  * `keychain_profile` -- this string field specifies the name of an application-specific
    password stored in your keychain.  The purpose of this password is to give the
    `notarytool` program access to your developer account.  Instructions for creating
    an application-specific password can be found [here](https://support.apple.com/en-us/HT204397).

    Once you have created an application-specific password for your developer account,
    you can add it to your Keychain using the **Keychain Access** application (which
    can be found in `/System/Library/CoreServices/Applications/`).

The certificate IDs can be found using the command
`security find-identity -p basic -v`

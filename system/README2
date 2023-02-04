Compiler Hacker's Guide to the new CM...
========================================

Last change: 12/5/2000

With added comments and questions by DBM: 2/3/2023
(and suggestions for updating)

* Libraries
-----------

The new way of building the compiler is heavily library-oriented.
Aside from a tiny portion of code that is responsible for defining the
pervasive environment, _everything_ lives in libraries. Building the
compiler means compiling and stabilizing these libraries first. Some
of the libraries exist just for reasons of organizing the code, the
other ones are potentially useful in their own right. Therefore, as a
beneficial side-effect of compiling the compiler, you will end up with
stable versions of these libraries.

At the moment, the following libraries are constructed when compiling
the compiler ("*" means that I consider the library potentially useful
in its own right):

  * $/basis.cm				SML'97 Basis Library (pre-loaded)
  * $/smlnj-lib.cm			SML/NJ Utility Library
  * $/html-lib.cm			SML/NJ HTML Library
  * $/pp-lib.cm				SML/NJ Pretty-print Library

  * $/ml-yacc-lib.cm			SML/NJ ML-Yacc runtime library

  * $smlnj/compiler/{alpha32,hppa,ppc,sparc,x86}.cm
					cross-compiler libraries, exporting
					structure <Arch>Compiler
  * $smlnj/compiler/current.cm		structure Compiler (current arch)
  * $smlnj/compiler/all.cm		all cross-compilers and all cross-CMBs

  * $smlnj/cm/full.cm			structure CM (see manual)
  * $smlnj/cm/tools.cm			CM tools library

  * $smlnj/cmb/{alpha32,hppa,ppc,sparc,x86}-unix.cm
					cross-bootstrap-compilers for Unix
					(structure <Arch>UnixCMB)
  * $smlnj/cmb/ppc-macos.cm		...for Mac (structure PPCMacosCMB)
  * $smlnj/cmb/x86-win32.cm		...for Windoze (structure X86Win32CMB)
  * $smlnj/cmb/current.cm		structure CMB (current arch/os)

  * $smlnj/compiler.cm			abbrev. for $smlnj/compiler/current.cm
  * $smlnj/cm.cm			abbrev. for $smlnj/cm/full.cm
  * $smlnj/cmb.cm			abbrev. for $smlnj/cmb/current.cm

  * $/comp-lib.cm			Utility library for compiler

  - $smlnj/viscomp/core.cm		Compiler core functionality
  - $smlnj/viscomp/{alpha32,hppa,ppc,sparc,x86}.cm
					Machine-specific parts of compiler

  - $smlnj/internal/{intsys,cm-lib,host-compiler-0}.cm
					Glue that holds the interactive system
					together

  * $MLRISC/{MLRISC,Control,Lib,ALPHA,HPPA,PPC,SPARC,IA32}.cm
					Various MLRISC bits
					(Other MLRISC libraries such as
					 Graph, Visual, etc. do not currently
					 take part in the SML/NJ build.)

  * $/{mlyacc,mllex,mlburg}-tool.cm	CM plug-in libraries for common tools
  * $/{grm,lex,burg}-ext.cm		CM plug-in libraries for common file
					extensions

Paths of the form $/foo/<more> are shorthands for $foo/foo/<more>.

A more complete explanation of the $-notation can be found later in
this document or in the CM manual.

To learn about the definitions of the $-anchors (and, thus, where in
the source tree the above libraries are defined), consult the
"pathconfig" file here in this directory.

[DBM:
 * smlnj-lib.cm is missnamed. It should probably be smlnj-lib-util.cm or
   smlnj-util-lib.cm or util-lib.cm.

 * The HTML library (html-lib.cm) should be updated to HTML 5. What does that
   involve?
 
 * $/prettyprint.cm, the new prettyprint library, should be added.
 
 * Does one need to consult just system/pathconfig, or also config/extrapathconfig?
   Should these two versions of pathconfig "definitions" be merged? Or why are there
   two (non-generated) pathconfig files.
   How do they relate to the lib/pathconfig file that is generated when runing
   $SMLNJ/build.sh (actually by (sml @CMbuild, which calls mlbuild in cm-boot.sml))?

 * $smlnj/compiler/{alpha32,hppa,ppc,sparc,x86}-unix.cm: the architecture list
   should be updated to {amd64, arm64}.

 * $smlnj/cmb/ppc-macos.cm -> $smlnj/cmb/amd64-unix.cm
   $smlnj/cmb/x86-win32.cm is currently missing: should be replaced by $smlnj/cmb/amd64-win64.cm
   $smlnj/cmb/arm64-unix.cm should be added
   
 * $smlnj/viscomp/{archs}.cm: {archs} should be replaced by {amd64, arm64}

 * $MLRISC/{...}.cm: {...} should be replaced by ?  Does llvm replace MLRISC somehow?
]


* Before you can use the bootstrap compiler (CMB)...
----------------------------------------------------

To be able to use CMB at all, you must first say

	CM.autoload "$smlnj/cmb.cm";

after you start sml.  Alternatively -- and perhaps more conveniently --
you can provide "$smlnj/cmb.cm" as a command-line argument to sml:

      $ sml '$smlnj/cmb.cm'

(Be sure to protect the dollar symbol which usually has its own
special meaning to the shell.)

* Compiling the compiler
------------------------

We are now back to the old scheme where a call to CMB.make() suffices to
build a bootable set of files (libraries in our case).  CMB.make maintains
two parallel hierarchies of derived files:

  1. the binfile hierarchy ("binfiles"), containing compiled objects for
     each individual ML source file; this hierarchy is rooted at
      <prefix>.bin.<arch>-<opsys>
  2. the stable library hierarchy ("bootfiles"), containing library files
     for each library that participates in building SML/NJ; this hierarchy
     is rooted at
      <prefix>.boot.<arch>-<opsys>

The default for <prefix> is "sml".  It can be changed by using
CMB.make' with the new <prefix> as the optional string argument.

CMB.make reuses existing bootfiles after it has verified that they are
consistent with their corresponding binfiles. Bootfiles do not need
to be deleted in order for CMB.make to work correctly.

To bootstrap a new system (using the runtime system boot loader), the
bootfiles _must_ be present, the binfiles need not be present (but
their presence does not hurt either).

You can reduce the number of extra files compiled and stabilized
during CMB.make at the expense of not building any cross-compilers.
For that, say
	#set (CMB.symval "LIGHT") (SOME 1);
before running CMB.make.

[DBM: What does "say" mean here? Where should this "#set" command be given?
  If it is entered in the shell, it looks like a comment line!]

* Making the heap image
-----------------------

The heap image is made by running the "makeml" script that you find
here in this directory.  By default it will try to refer to the
sml.boot.<arch>-<os> directory.  You can change this using the -boot
argument (which takes the full name of the boot directory to be used).

The "feel" of using makeml should be mostly as it used to be. However,
internally, there are some changes that you should be aware of:

1. The script will make a heap image and build a separate library directory
   that contains (hard) links to the library files in the bootfile directory.

2. There is no "-full" option anymore. This functionality should
   eventually be provided by a library with a sufficiently rich export
   interface.

3. No image will be generated if you use the -rebuild option.
   Instead, the script quits after making new bin and new boot
   directories. You must re-invoke makeml with a suitable "-boot"
   option to actually make the image. The argument to "-rebuild"
   is the <prefix> for the new bin and boot directories (see above).

   [Note: When the -rebuild option is specified, then the boot procedure
    will not read static environments from the boot directory.  Instead,
    after the ML code has been loaded and linked, the system will invoke
    CMB.make' with the argument that was given to -rebuild.  After
    CMB.make' is done, the system quits.  In essence, makeml with -rebuild
    acts as a bootstrap compiler that is not dependent on any usable
    static environments.]

Makeml will not destroy the bootfile directory.

* Testing a newly generated heap image
--------------------------------------

If you use a new heap image by saying "sml @SMLload=..." then things
will not go as you may expect because along with the new heap image
should go those new stable libraries, but unless you do something
about it, the newly booted system will look for its stable libraries
in places where you stored your _old_ stable libraries.  (After just
having done "makeml", these "places" would be within the boot file
hierarchy under <prefix>.boot.<arch>-<os>.)

After you have made the new heap image, the new libraries are in a
separate directory whose name is derived from the name of the heap
image.  (Actually, only the directory hierachy is separate, the
library files themselves are hard links.)  The "testml" script that
you also find here will run the heap image and instruct it to look for
its libraries in that new library directory by setting the
CM_PATHCONFIG environment variable to point to a different pathconfig
file under <prefix>.lib.

"testml" takes the <prefix> of the heap image as its first
argument. All other arguments are passed verbatim to the ML process.

The <prefix> is the same as the one used when you did "makeml".  If
you run "testml" without arguments, <prefix> defaults to "sml".
Thus, if you just said "makeml" without arguments you can also say
"testml" without arguments.  (Note that you _must_ supply the <prefix>
argument if you intend to pass any additional arguments.)

* Installing a heap image for more permanent use
------------------------------------------------

You can "install" a newly generated heap image by replacing the old
image with the new one AND AT THE SAME TIME replacing the old stable
libaries with the new ones.  To do this, run the "installml" script.

Like "testml", "installml" also expects the <prefix> as its first
argument.  <prefix> defaults to "sml" if no argument is specified.

"installml" patches the ../../lib/pathconfig [DBM: ../lib/pathconfig] file to reflect any
changes or additions to the path name mapping. (I say "patches"
because entries unrelated to the SML/NJ build process are retained in
their original form.)  If you want to use a destination directory that
is different from ../../lib [DBM: ../lib/], then you must do this by hand (i.e.,
installml does not have an option for that).

Thus, after a successful CMB.make, you should say

	./makeml

to make the new heap image + libraries, then

	./testml

to make sure everything works, and finally

	./installml

to replace your existing compiler with the one you just built and tested.

[DBM: Document the "-clean" and "-boot" options for installml.]


* Cross-compiling
-----------------

All cross-compilers live in the "$smlnj/compiler/all.cm" library.
(The source tree for the "$smlnj" anchor -- see "pathconfig" -- is
src/system/smlnj [DBM: system/smlnj], but this should normally not concern you.)
You must first say

	CM.autoload "$smlnj/compiler/all.cm";

before you can access them. (This step corresponds to the old
CMB.retarget call.) After that, _all_ cross-compilers are available
at the same time. However, the ones that you are not using don't take
up any undue space because they only get loaded once you actually
mention them at top level. The names of the structures currently
exported by $smlnj/compiler/all.cm are:

	structure Alpha32UnixCMB
	structure HppaUnixCMB
	structure PPCMacOSCMB
	structure PPCUnixCMB
	structure SparcUnixCMB
	structure X86UnixCMB
	structure X86Win32CMB

	structure Alpha32Compiler
	structure HppaCompiler
	structure PPCCompiler
	structure SparcCompiler
	structure X86Compiler

[DBM: Out of date. Currently there is no $smlnj/compiler/all.cm.
 When restored, it should trim the architectures to amd64 and arm64. Currently the
 files compiler/amd64.sml and compiler/arm64.sml both define a structure "Backend"
 (defined in terms of Amd64Backend and Arm64Backend, respectively). Also, implicitly,
 only the unix OS option is supported.]

(PPCMacOSCMB is not very useful at the moment because there is no
implementation of the basis library for the MacOS.)
[DBM: Out of date; remove this paragraph.]

Alternatively, you can select just the one single structure that you
are interested in by auto-loading $smlnj/compiler/<arch>.cm or
$smlnj/cmb/<arch>-<os>.cm.
<arch> currently ranges over "alpha32", "hppa", "ppc", "sparc", and "x86.
<os> can be either "unix" or "macos" or "win32".
(Obviously, not all combinations are valid.)

[DBM: Above, architectures and oses need to be updated: amd64, arm64; unix, win64.
 But does unix always cover both macOS (darwin) and linux? When, if ever, might it
 matter whether we have OS=unix vs OS=darwin?
 Note that the new cm files, amd64.cm and arm64.cm currently specify only architecture.
  Should we rename these files amd64-unix.cm and arm64-unix.cm?
 Will this change once win64 is supported, yielding amd64-unix.cm and amd64-win64.cm?]

Again, as with $smlnj/cmb.cm, you can specify the .cm file as an
argument to the sml command:

    $ sml '$smlnj/compiler/all.cm'  [DMB: currently doesn't exist.]

or

    $ sml '$smlnj/cmb/alpha32-unix.cm' [DBM: use "$smlnj/cmb/arm64.cm"?]

[Note: The command line for the "sml" command accepts configuration
parameters of the form "@SMLxxx...", mode switches of the form "-m"
and "-a", names of ML files -- which are passed to "use" -- and
arguments suitable for CM.make or CM.autoload.  CM.autoload is the
default; the "-m" and "-a" mode switches can be used to change the
default -- even several times within the same command line.
A single argument "@CMslave" is also accepted, but it should not be
used directly as it is intended for use by the parallel compilation
facility within CM.]

[DBM: Make this note consistent with the current SML man page.
@CMslave is not mentioned in the man page. Nor are some CM options like @CMredump
and @CMbuild. These special CM options (and others?) should be documented somewhere!]


* Path configuration
--------------------

+ Basics:

[DBM: terminology question -- is there a distinction betweeh the terms
"path" and "path name" (or "pathname") or are these equivalent? If they
are equivalent, we should use only one or the other consistently. "pathname"
also sometimes seems to refer to a native (Unix) pathname.
And, in addition, there is the possibility of confusion with the Unix PATH 
variable, which is a sequence of directory names. The CM Manual has the same
confusion: "path names" and "pathnames", but mostly "pathnames". One possible
clarification would be to use the term "pathname" only for CM pathnames, and
always use another term, say "filepath" for Unix (or OS) pathnames.]

One of the new features of CM is its handling of pathnames [DBM].  In the
old CM, one particular point of trouble was the autoloader.  It
analyzes a group or library and remembers the locations of associated
files.  Later, when the necessity arises, those files will be read.
Therefore, one was asking for trouble if the current working directory
was changed between analysis- and load-time, or, worse, if files
actually moved about (as is often the case if build- and
installation-directories are different, or, to put it more generally,
if CM's state is frozen into a heap image and used in a different
environment).

Maybe it would have been possible to work around most of these
problems by fixing the path-lookup mechanism in the old CM and using
it extensively.  But path-lookup (as in the Unix-shell's "PATH") is
inherently dangerous because one can never be too sure what it will be
that is found on the path.  A new file in one of the directories early
in the path can upset the program that hopes to find something under
the same name later on the path.  Even when ignoring security-issues
like trojan horses and such, this definitely opens the door for
various unpleasant surprises.  (Who has never named a test version
of a program "test" an found that it acts strangely only to discover
later that /bin/test was run instead?)

Thus, the new scheme used by CM is a fixed mapping of what I call
"configuration anchors" to corresponding directories.  The mapping can
be changed, but one must do so explicitly.  In effect, it does not
depend on the contents of the file system.  Here is how it works:

[DBM: The "fixed mapping" is global and "stateful" in that it can be modified. It is a
nameless environment that maps anchors (a kind of name or symbol) to "directory names"
which are OS-specific "pathnames" that use a very similar notation (in the case of
Unix). However, there are locally-scoped anchor bindings that are can be specified in CM
description files using the "bind" declaration.  (How these are represented and how they
are related to the underlying global anchor environment is unclear.). Lets use the
identifier "AE" to refer to this nameless, global anchor environment.]

What are the rules governing what is a valid anchor name? One might think that they would
be the same as for CM "variables" (which seem to be the same as the lexical rules for SML
variables), but we see examples like "$basis.cm", which leads me to assume that anchor
names follow the same rules as file names, (but in what OS?). I have been assumeing that
anchor names can take the same form as Unix file names. Similarly, the lexical conventions
for arc names are not specified, and seem to default to the convention for OS filenames,
specifically Unix file names.

Looking at documentation on the bash shell or Unix/Linux file names, they also use
"pathnames" for absolute and relative naming of files. And the notations for CM and Unix
pathnames are almost identical, except for the leading "$" in CM pathnames.  So if we want
to be precise and unambiguous, we probably have to specify "CM pathname" and "Unix
pathname".  Here is a proposal for more precise, and less ambiguous terminology:

   "path"  -- not used
   "pathname" -- a CM pathname, and anchor followed by "/" followed by a sequence of
                 "arcs" (filenames) separated by "/"
   "filepath" -- an OS's native form of pathname for locating files in the file system, and
   "dirpath"  -- an OS's native form of pathname denoting a directory in the file system

The (native) notation used for filepaths and dirpaths depends on the OS. In Unix, the notation
for filepaths is essentially the same as for the "tail" of a CM pathname after the first "/".
\DBM]

If I specify a pathname that starts with a "$", then the first arc
between "$" and the first "/" is taken as the name of a so-called
"anchor".  CM knows a mapping [DBM: see below] from anchor names to directory names and
replaces the prefix $<anchor> with the name of the corresponding
directory.

[DBM: "CM knows a mapping" == There is a global anchor environment, call it
AE, that maps an anchor (a kind of identifier) to a dirpath (an OS-specific "pathname"
of a directory). There is one such global environment that is part of the 
compiler+cm state.]

[DBM: It is important to know whether the directory names that the anchor environment maps
to are absolute or relative file names, or either. And, if relative, what they are
relative to in any given instance. The fact that the "pathname" notation is very similar
to the notation for Unix file names may also be a source of confusion, as noted above.]

[DBM: The sequence of "arcs" separated by "/" that follows an anchor in a CM pathname are
identical to a Unix pathname. If we call this Unix pathname the "tail" of the CM pathname,
a CM pathname is thus mapped, via AE, as follows:

   pathname -> AE (anchor(pathname)) "/" tail(pathname)

where

   AE : anchor -> Unix pathname
   anchor : CM pathname -> anchor
   tail : CM pathname -> Unix pathname  (i.e. pathname -> filepath ?)

\DBM]

Therefore, an anchored path has the general form

   $<anchor>/<path>  [DBM: where path looks exactly like a _relative_ Unix pathname]

It is important that there is at least one arc in <path>.  In other
words, the form $<anchor> is NOT a valid pathname. [DBM]

(Actually, under certain circumstances it _is_ valid -- and means what it seems to mean,
namely the directory [DBM: i.e. Unix pathname] denoted by the name that <anchor> is mapped
to [DBM: but relative to what?].  However, since directory names do not usually occur by
themselves, you can think of this form as being invalid.  There is one exception to this:
"bind" specifications for .cm files.  See the CM manual for more details.)

[DBM: Is there a reason for _not_ treating $<anchor> as a valid CM pathname in all
contexts? Its meaning seems clear -- it is just the OS pathname denoted by the anchor
according to the global anchor environment (AE). What problems would arise if we treat it
as a pathname? We would not have to treat "bind" declarations as a special case.]

Examples:

   $smlnj/compiler/all.cm  [DBM: this does not exist currently, so maybe bad example?]
   $basis.cm/basis.cm
   $MLRISC/Control.cm      [DBM: maybe $MLRISC is now irrelevant?]

The special case where <anchor> coincides with the first arc of <pathname>
can be abbreviated by ommitting <anchor>.  This leads to the shorthand

  $/<anchor>/<more>...

for the longer

  $<anchor>/<anchor>/<more>...  [DBM: implies <anchor> is a valid OS filename]

Examples:

  $/foo/bar/baz.cm      (* same as $foo/foo/bar/baz.cm *)
  $/basis.cm            (* same as $basis.cm/basis.cm *)

[DBM: delete following?]
There used to be a notion of "implicit" anchors where in the case that
<anchor> is a known anchor [DBM: meaning that <anchor> is in the domain of
the glocal anchor enviroment, AE], paths of the form

   <anchor>/<more>...

were interpreted as if they had been written

   $<anchor>/<anchor>/<more>...

This is no longer the case. <foo>/<bar>... now always means what it
seems to mean: a relative path starting with an arc named <foo>.
[/DBM: can we delete discussion of features of older CM versions? It's
been 20 years, afterall!]

+ Why anchored paths?

The important point is that one can change the mapping of the anchor, and the translation
of the (anchored) pathname [DBM] -- together will all file names derived from it [DBM: how
is a "file name" derived from a CM pathname?]  -- will also change accordingly -- even
very late in the game.  CM avoids "elaborating" pathnames [DBM] until it really needs
them, i.e. when files are opened [DBM].  CM is also willing to re-elaborate the same names
if there is reason to do so. Thus, the "basis.cm" library that was analyzed "here" but
then moved "there" will also be found "there" if the anchor has been re-set accordingly.

The anchor mapping [DBM: call it "AE"] is (re-)initialized at startup time [DBM: ?] by reading two
configuration files.  Normally, those are the "../../lib/pathconfig" [DBM: now "../lib/pathconfig"]
file and the ".smlnj-pathconfig" file in your home directory (if such
exists).
[DBM: How do $SMLNJ/lib/pathconfig and ~/.smlnj-pathconfig specify anchor mappings/bindings
in AE? What is their format and semantics. Do the bindings of ~/.smlnj-pathconfig "overlay"
(and hence possibly shadow) the bindings specified in $SMLNJ/lib/pathconfig, or vice versa.
$SMLNJ/lib/pathconfig is a generated text file, generated during the build process 
(at least partly by the mlbuild function in CM/main/cm-boot.sml). What about the other,
non-generated, path configuration files: $SMLNJ/system/pathconfig and $SMLNJ/config/extrapathconfig?
What is their format and semantics, and how/where are they processed. How do they contribute to
the generated $SMLNJ/lib/pathconfig file?]

During an ongoing session, function CM.Anchor.anchor can be used to query and modify the
anchor mapping.
[DBM: document the type and semantics of CM.Anchor.anchor, or at least give a pointer to
where it is documented. Looks like CM Manual Sections 3.3, 3.4, and 5.1.4 contain some
relevant documentation, though there the Manual talks about "Anchor environments"
(plural!). Looks like the global, mutable, AE should be known as the "root anchor environment",
and in addition we have "local" anchor environments established by "bind:" directives.
Multiple such local anchor environments can coexist and bind the same anchor to different
filepaths, as illustrated in the example in Section 3.5.]

+ Different configurations at different times:

During compilation of the compiler [DBM: when running cmb-make], CMB uses a path
configuration that is read from the file "pathconfig" located here in this directory [DBM:
i.e. $SMLNJ/system/pathconfig].
[DBM: So system/pathconfig, and the AE that it defines (lets call it the CMB AE), is only
relevant during the cmb-make compilation of the compiler? Is also used when compiling the
tools and libraries?]

At bootstrap time (while running "makeml"), <<the same anchors>> [DBM: which anchors?  Do
you mean the set of anchors that are the domain of the CMB AE?] are mapped to the
corresponding sub-directory of the "boot" directory: basis.cm is mapped to
sml.boot.<arch>-<os>/basis.cm -- which means that CM will look for a library named
sml.boot.<arch>-<os>/basis.cm/basis.cm -- and so forth.

[Note, there are some anchors in "pathconfig" [DBM: $SMLNJ/lib/pathconfig? or
$SMLNJ/system/pathconfig?] that have no corresponding sub-directory of the boot directory.
Examples are "root.cm", "cm", and so on [DBM: must be $SMLNJ/system/pathconfig, since
"root.cm" doesn't appear in lib/pathconfig]. The reason is that there are no stable
libraries whose description files are named using these anchors; everything anchored at
"$cm" is a group but not a library. [DBM: Why is this?]]

By the way, you will perhaps notice that there is no file
	sml.boot.<arch>-<os>/basis.cm/basis.cm
but there _is_ the corresponding stable archive
	sml.boot.<arch>-<os>/basis.cm/CM/<arch>-<os>/basis.cm
CM always looks for stable archives first.
[DBM: Preceding para is out-of-date. Do you mean

    sml.boot.amd64-unix/smlnj/basis/.cm/amd64-unix/basis.cm?

Also, there is no "basis.cm" anchor defined in system/pathconfig.]

This mapping (from anchors to names in the boot directory) is the one
that will get frozen into the generated heap image at boot time.
Thus, unless it is changed, CM will look for its libraries in the boot
directory.  The aforementioned "testml" script will make sure (by
setting the environment variable CM_PATHCONFIG) that the mapping be
changed to the one specified in a new "pathconfig" file which was
created by makeml and placed into the test library directory.  It
points all anchors to the corresponding entry in the test library
directory.  Thus, "testml" will let a new heap image run with its
corresponding new libraries.

Normally, however, CM consults other pathconfig files at startup --
files that live in standard locations.  These files are used to modify
the path configuration to let anchors point to their "usual" places.
The names of the files that are read (if present) are configurable via
environment variables.  At the moment they default to
	/usr/lib/smlnj-pathconfig
and
	$HOME/.smlnj-pathconfig
[DBM: /usr/lib/smlnj-pathconfig does not exist. Should this now be
   /usr/local/smlnj/lib/pathconfig ?]

The first one is configurable via CM_PATHCONFIG (and the default is configurable at boot
time [DBM: When is "boot time"? Is it when system/makeml is running?], also via
CM_PATHCONFIG); the last is configurable via CM_LOCAL_PATHCONFIG.  In fact, the makeml
script sets the CM_PATHCONFIG variable before making the heap image.  Therefore, heap
images generated by makeml will look for their global pathconfig file in

	../../lib/pathconfig  [DBM: should be "../lib/pathconfig" ?]

[Note: The "makeml" script will not re-set the CM_PATHCONFIG
variable if it was already set before.  If it does re-set the
variable, it uses an absolute path name instead of the relative path
that I used for illustration above.]

For example, I always keep my "good" libraries in `pwd`/../../lib --
where both the main "install" script (in config/install.sh) and the
"installml" script (see above) also put them -- so I don't have to do
anything special about my pathconfig file.
[DBM: obsolete. Update this for the $SMLNJ/build.sh script.]

Once I have new heap image and libraries working, I replace the old
"good" image with the new one:

  mv <image>.<arch>-<osvariant> ../../bin/.heap/sml.<arch>-<osvariant>  [DBM: "../../" -> "../"]

After this I must also move all libraries from <image>.libs/* to their
corresponding position in ../../lib. [DBM: "../../lib" -> "../lib"]
[DBM: where "<image>" ranges over what? sml, ml-yacc, ml-burg, etc.?]

Since this is cumbersome to do by hand, there is a script called
"installml" that automates this task.  Using the script has the added
advantage that it will not clobber libraries that belong to other than
the current architecture.  (A rather heavy-handed "rm/mv" approach
will delete all stable libraries for all architectures.)
"installml" also patches the ../../lib/pathconfig file as necessary.
[DBM: How? "../.." -> ".."]

Of course, you can organize things differently for yourself -- the
path configuration mechanism should be sufficiently flexible.  If you
do so, you will have to set CM_PATHCONFIG.  This must be done before
you start sml.  If you want to change the pathname mapping at the time
sml is already running, then use the functions in CM.Anchor. 
[DBM: Hard for me to imagine scenarios where I would do either. One would
have to have a better grasp of what is going on than I do!]

%%%%%%%% DBM

* Libraries vs. Groups
----------------------

With the old CM, "group" was the primary concept while "library" and "stabilization" could
be considered afterthoughts.  This has changed.  Now "library" is the primary concept,
"stabilization" is semantically significant, and "groups" are a secondary mechanism.

[DBM: We should assume in this document that we are using the "new" (20-year-old) CM. No
need to discuss what the old CM was like, or how the "new" CM differs from it. So para
above is unneeded.]

Libraries are used to "structure the world"; groups are used to give structure to
libraries [DBM: so that libraries do not have to be monolythic].  Each group can be used
either in precisely one library (in which case it cannot be used at the interactive
toplevel) or at the toplevel (in which case it cannot be used in any library).  In other
words, if you count the toplevel as a library, then each group has a unique "owner"
library.  Of course, there still is no limit on how many times a group can be mentioned as
a member of other groups -- as long as all these other groups belong to the same owner
library.

[DBM: How disruptive would it be to now disallow "top-level" groups that do not belong
to any library?  Another point. A group G.cm can "declare" that it belongs to library A.cm,
but the A.cm (i.e. its CDF) may not even mention G.cm, in which case A.cm does not use or
depend on G.cm. This situation seems anomalous; should it be detected? Should it be considered
an error?]

Normally, collections of files that belong together should be made into proper CM
libraries.  CM groups (aka "library components") should be used only when there are
namespace problems within a library.

[DBM: I assume that if a library "contains/has" multiple groups, then the export lists of
its groups must be disjoint. But two different groups could have their own definitions of
a structure Foo (in files foo1.sml, foo2.sml respectively), as long as Foo was not
exported from both groups. Actually, the CM Manual, Section 2.4 allows two subgroups of
a library to export the same symbol, say "Foo", as long as the corresponding source
files where Foo is defined (say foo1.sml and foo2.sml) are "identical" (textually, or
ignoring comments and white space?).  This seems too liberal. I think two subgroups of
a library should not be allowed to export the same symbol (i.e. subgoup exports should
be disjoint).
Can we identify some realistic examples in the compiler?]

Aside from the fact that I find this design quite natural, there is
actually a technical reason for it: when you stabilize a library
(groups cannot be stabilized), then all its sub-groups (not
sub-libraries!)  get "sucked into" the stable archive of the library.
In other words, even if you have n+1 CM description files (1 for the
library, n for n sub-groups), there will be just one file representing
the one stable archive (per architecture/os) for the whole thing.  For
example, I structured the standard basis into one library with two
sub-groups, but once you compile it (CMB.make) there is only one
stable file that represents the whole basis library.  If groups were
allowed to appear in more than one library, then stabilization would
duplicate the group (its code, its environment data structures, and
even its dynamic state).

There is a small change to the syntax of group description files: they
must explicitly state which library they belong to. CM will verify
that.  The owner library is specified in parentheses after the "group"
keyword.  If the specification is missing (that's the "old" syntax),
then the the owner will be taken to be the interactive toplevel.
[DBM: see my comments on Sec 2.4 in CM-manual-errata.txt.]


* Pervasive environment, core environment, the init library "init.cmi"
-------------------------------------------------------------------------

CMB.make starts out by building and compiling the
"init library".  This library cannot be described in the "usual" way
because it uses "magic" in three ways:
 - it is used to later tie in the runtime system
 - it binds the "_Core" structure
 - it exports the "pervasive" environment

The pervasive environment no longer includes the entire basis library
but only non-modular bindings (top-level bindings of variables and
types, and "fixity bindings").

CM cannot automatically determine dependencies (or exports) for the
init library source files, but it still does use its regular cutoff
recompilation mechanism.  Therefore, dependencies must be given
explicitly.  This is done by a special description file which
currently lives in $SMLNJ/system/smlnj/init/init.cmi (as an anchored path:
"$smlnj/init/init.cmi"). See the long comment at the beginning of
that file for more details.

After it is built, $smlnj/init/init.cmi can be used as an "ordinary" library by other
libraries.  (This is done, for example, by the implementation of the Basis library.)
Access to "$smlnj/init/init.cmi" is protected by the privilege named "primitive".  Also,
note that the .cmi-file [DBM: which file?] is not automatically recognized as a CM
description file. ("cmi" should remind you of "CM - Initial library".)  Therefore, it must
be given an explicit member class:

     $smlnj/init/init.cmi : cm

[DBM: might it be possible to "codify" the magic involved in the "init library"
so that it is less ad hoc?]


* Autoloader
------------

The new system heavily relies on the autoloader.  As a result, almost no static
environments need to get unpickled at bootstrap time [DBM: what is "bootstrap time"? Does
this refer to the running of system/.makeml? (but not running system/.cmb-make?)].  The
construction of such environments is deferred until they become necessary.  Thanks to
this, it was possible to reduce the size of the heap image by more than one megabyte
(depending on the architecture) [DBM: one megabyte used to be a lot of memory. Now it is
1/64000th of my iMac's RAM.]  The downside (although not really terribly bad) is that
there is a short wait when you first touch an identifier that hasn't been touched before.
(I acknowledge that the notion of "short" may depend on your sense of urgency. :-)
[DBM: my iMac is also at least 100 times faster than what I was using in 2000.]

The reliance on the autoloader (and therefore CM's library mechanism)
means that in order to be able to use the system, your paths must be
properly configured.
[DBM: how much of a "black art" is this process of properly configuring your paths?]

Several libraries get pre-registered [DBM: for autoloading] at bootstap time.  Here, at
least the following two should be included: the basis library ($/basis.cm) and CM itself
($smlnj/cm.cm).  Currently, we also pre-register the library exporting structure Compiler
($smlnj/compiler.cm) and the SML/NJ library ($/smlnj-lib.cm) [DBM: just the Util libraries.].

Here are some other useful libraries that are not pre-registered but
which can easily be made accessible via CM.autoload (or, non-lazily,
via CM.make):

	$smlnj/cmb.cm		- provides "structure CMB"
	$smlnj/cmb/current.cm	- same as $smlnj/cmb.cm
	$smlnj/compiler/all.cm	- provides "structure <Arch>Compiler" and
				  "structure <Arch><OS>CMB" for various
				  values of <Arch> and <OS>

The file preloads.standard here in this directory currently includes
$smlnj/cmb.cm.  This means that by doing ./makeml one obtains a heap
image with the bootstrap compiler being pre-registered as well.  This
seems reasonable for compiler hackers.  (The config/install.sh script
uses config/preloads where $smlnj/cmb.cm is not pre-registered.  This
is appropriate as a setup for general users.)


* Internal sharing
------------------

Dynamic values of loaded modules are shared.  This is true even for
those modules that are used by the interactive compiler itself.  If
you load a module from a library that is also used by the interactive
compiler, then "loading" means "loading the static environmnent" -- it
does not mean "loading the code and linking it".  Instead, you get to
share the compiler's dynamic values (and therefore the executable
code as well).

Of course, if you load a module that hasn't been loaded before and
also isn't used by the interactive system, then CM will get the code
and link (execute) it.


* Access control
----------------

In some places, you will find that the "group" and "library" keywords
in description files are preceeded by certain strings, sometimes in
parentheses.  These strings are the names of "privileges".  Don't
worry about them too much at the moment.  For the time being, access
control is not enforced, but the infrastructure is in place.
[DBM: so "access control" is currently not operational. What are the
chances that this will change?]

* Preprocessor
--------------

The syntax of expressions in #if and #elif clauses is now more ML-ish
instead of C-ish.  (Hey, this is ML after all!)  In particular, you
must use "andalso", "orelse", and "not" instead of "&&", "||" and "!".
Unary minus is "~".
[DBM: another comparison with "old CM" this is unnecessary.]

A more interesting change is that you can now query the exports of
sources/subgroups/sublibraries:

  - Within the "members" section of the description (i.e., after "is"):
    The expression
	defined(<namespace> <name>)
    is true if any of the included members preceeding this clause exports
    a symbol "<namespace> <name>". [DBM: thus introducing an order dependence
    between members.]
  - Within the "exports" section of the description (i.e., before "is"):
    The same expression is true if _any_ of the members exports the
    named symbol.
    (It would be more logical if the exports section would follow the
     members section, but for esthetic reasons I prefer the exports
     section to come first.)

Example:

	+--------------------------+
	|Library		   |
	|  structure Foo	   |
	|#if defined(structure Bar)|
	|  structure Bar	   |
	|#endif			   |
	|is			   |
	|#if SMLNJ_VERSION > 110   |
	|  new-foo.sml		   |
	|#else			   |
	|  old-foo.sml		   |
	|#endif			   |
	|#if defined(structure Bar)|
	|  bar-client.sml	   |
	|#else			   |
	|  no-bar-so-far.sml	   |
	|#endif			   |
	+--------------------------+

Here, the file bar-client.sml gets included if SMLNJ_VERSION is
greater than 110 and new-foo.sml exports a structure Bar _or_ if
SMLNJ_VERSION <= 110 and old-foo.sml exports structure Bar. Otherwise
no-bar-so-far.sml gets included instead.  In addition, the export of
structure Bar is guarded by its own existence.  (Structure Bar could
also be defined by no-bar-so-far.sml in which case it would get
exported regardless of the outcome of the other "defined" test.)

Some things to note:

  - For the purpose of the pre-processor, order among members is
    significant.  (For the purpose of dependency analysis, order continues
    to be not significant).
  - As a consequence, in some cases pre-processor dependencies and
    compilation-dependencies may end up to be opposites of each other.
    (This is not a problem; it may very well be a feature.)


* The Basis Library is no longer built-in
-----------------------------------------

The SML'97 basis is no longer built-in.  If you want to use it, you
must specify $/basis.cm as a member of your group/library.


* No more aliases
-----------------

The "alias" feature is no longer with us.  At first I thought I could
keep it, but it turns out that it causes some fairly fundamental
problems with the autoloader.  However, I don't think that this is a
big loss because path anchors make up for most of it.  Moreover,
stable libraries can now easily be moved to convenient locations
without having to move large source trees at the same time. (See my
new config/install.sh script for examples of that.)

It is possible to simulate aliases (in a way that is safer than the
original alias mechanism).  For example, the root.cm file (which is the
root of the whole system as far as CMB.make is concerned) acts as an
alias for $smlnj/internal/intsys.cm.  In this case, root.cm is a group
to avoid having a (trivial) stable library file built for it.
[DBM: not a very strong justification for still allowing library-less groups.]

A library can act as an "alias" for another library if it has a
verbatim copy of the export list and mentions the other library as its
only member.  Examples for this are $smlnj/cm.cm (for
$smlnj/cm/full.cm), $smlnj/compiler.cm (for $smlnj/compiler/current.cm),
etc.  The stable library file for such an "alias" is typically very
small because it basically just points to the other library.  (For
example, the file representing $smlnj/cm.cm is currently 234 bytes
long.)


* Don't use relative or absolute pathnames to refer to libraries
----------------------------------------------------------------

Don't use relative or absolute pathnames to refer to libraries.  If
you do it anyway, you'll get an appropriate warning at the time when
you do CMB.make().  If you use relative or absolute pathnames to
refer to library B from library A, you will be committed to keeping B
in the same relative (to A) or absolute location.  This, clearly,
would be undesirable in many situations (although perhaps not always).

[DBM: These "relative or absolute pathnames" refer to CM pathnames that
are not "anchored", and are thus indistinguiable from (Unix-nomalized)
OS pathnames.]


==========================================================================================
DBM: A couple of other things that are out-of-date in $SMLNJ/system:

* system/serv.sml ("../../bin/sml" should be "../bin/sml")

* system/rserv.sml (mentions "/home/blume/ML/...")


==========================================================================================
I would like to see this README file updated and revised as suggested by my comments, and
perhaps combined with a revision of my notes/install.txt to provide a reasonably deep
and complete description of the system build process.

This would also require more documentation about the SML files in $SMLNJ/system/smlnj/installer,
and a bunch more information about the full, compiler-hacker's view of CM functionality,
including command-line options like @CMbuild and @CMredump that are used in build shell scripts
like $SMLNJ/build.sh, $SMLNJ/bin/ml-build, $SMLNJ/bin/.link-sml. In fact, it would be good to
have a separate "implementors" manual or tech-report describing the internals and special
functionalities of CM. Either of these documents might incorporate the contents of the existing
CMB Manual.

My goal is be a complete explanation of the build process and how CM is used in it that I can
understand and verify.


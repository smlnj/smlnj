compiler/Basic/README.txt

Basic is a "component" directory under compiler/ that contains fundamental types and functionalities that are used in the SML/NJ compiler.

Basic is both a top-level directory under compiler and a CML Library with CDF Basic/basic.cm. The new Admin and Basic directories/libraries replace Basics, Library, and ElabData/basics from the old organization.

Contents:

Basic/

  basic.cm

  symbol/  (<- Basics/symbol)
    Except fixity.sml belongs elsewhere. (Parse or ElabData/staticenv?)

  sympath/ (<- ElabData/basics)
    sympaths.s?? (with ElabData/basics/pathname.sml itegrated)
    ppsympaths.sml

  var/
    var.s??  (generic variables as numbers: LambdaVar, TyVar, TyconVar) 
    <LambdaVar>

  access/
    access.s??

  stamp/  (stamps, persstamps, pids, pidenv)
    stamps.s??      (<- ElabData/basics/stamps.s??)
    persstamps.s??  (<- Basics/pid)
    persmap.sml     (<- Basics/pid)
    pidenv.s??      (<- Basics/pid)

  env/    (basic environment type)
    env.s??         (<- ElabData/basics/env.s??)

  numbers/
    real-const.sml  (<- Basics/compiler)
    int-const.sml   (<- Basics/compiler)
    
  reals/            (<- Basics/reals)

  const-arith/      (<- Library/const-arith)

  params/           
    compinfo.sml    (<- ElabData/main/compinfo.sml)

  pickle/           (<- Library/pickle -- might belong in Admin instead?)
  
  util/             (<- Basics/util)

Note: Library/bignum is unused and can be discarded.

Changes:

1. When pathname.sml was integrated into sympaths.sml, a function "getName: path -> symbol option"
was added to each of the structures SymPath and InvPath.  SymPath.getName replaces PathName.getNameSP
and InvPath.getName replaces PathName.getNameIP, BUT THE TYPES ARE DIFFERENT! So uses of getNameSP
and getNameIP will need to be edited appropriately.


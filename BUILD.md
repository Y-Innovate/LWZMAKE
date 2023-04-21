# How LWZMAKE builds itself
The first build of `LWZMAKE` will have to be with a regular JCL, there's a sample JCL in this Git repository for just that purpose, see [ASMLKED.jcl](SOURCE/JCL/ASMLKED.jcl).

If you do have an `LWZMAKE` load module, you can use `LWZMAKE` to build `LWZMAKE`. There's another sample JCL in this Git repository for that as well, see [BUILD.jcl](SOURCE/JCL/BUILD.jcl). This JCL goes together with shell script [build.sh](SOURCE/build.sh). The JCL is not meant to be submitted interactively, but rather you should run `build.sh` which will submit `BUILD.jcl` for you.  
The reason for doing it with `build.sh` is so that you don't have to put your repository clone's directory in `BUILD.jcl` (causing a change which Git will want you to commit).

This page takes the `makefile` in BUILD.jcl apart to explain bit by bit what it's doing.

Here's the entire makefile:

    # build LWZMAKE using LWZMAKE

    .USSHOME = <homedir>  # <== set to your own home directory

    CEEHLQ          := CEE  # <== set to your LE data set HLQ
    HLAHLQ          := HLA  # <== set to your HLASM data set HLQ

    gitdir          := @@GITDIR@@

    feature         := ${sh cd $(gitdir);git branch | \
                         grep -E "^\\* (.*)$" | cut -d' ' -f2 }
    feature_upper   := ${sh echo "$(feature)"|tr 'a-z' 'A-Z'}

    hlq             := <your_hlq>.$(feature_upper)  # <== choose a HLQ

    asmlib          := $(hlq).ASM
    asmlstlib       := $(asmlib).LISTING
    cpylib          := $(hlq).COPY
    jcllib          := $(hlq).JCL
    objlib          := $(hlq).OBJECT
    sysadatalib     := $(hlq).SYSADATA
    eqalangxlib     := $(hlq).EQALANGX
    lkedlib         := $(hlq).LKED
    loadlib         := $(hlq).LOAD
    syslib_asma     := SYS1.MACLIB SYS1.MODGEN $(CEEHLQ).SCEEMAC\
                       $(HLAHLQ).SASMMAC2 $(cpylib)
    syslib_lked     := $(CEEHLQ).SCEELKED $(objlib)

    recfmFB80       := $(asmlib) $(cpylib) $(jcllib) $(objlib) $(lkedlib)
    recfmFBA133     := $(asmlstlib)
    recfmVB32756    := $(sysadatalib)
    recfmVB1562     := $(eqalangxlib)
    recfmU          := $(loadlib)

    asmdir          := $(gitdir)/ASM
    cpydir          := $(gitdir)/COPY
    jcldir          := $(gitdir)/JCL
    lkeddir         := $(gitdir)/LKED

    asmfiles        := ${sh cd $(asmdir);find * -prune -type f}
    asmmems         := ${stripext $(asmfiles)}
    
    cpyfiles        := ${sh cd $(cpydir);find * -prune -type f}
    cpymems         := ${stripext $(cpyfiles)}

    jclfiles        := ${sh cd $(jcldir);find * -prune -type f}
    jclmems         := ${stripext $(jclfiles)}

    lkedfiles       := ${sh cd $(lkeddir);find * -prune -type f}
    lkedmems        := ${stripext $(lkedfiles)}

    asmtgts         := ${addpdsname $(asmlib),$(asmmems)}
    cpytgts         := ${addpdsname $(cpylib),$(cpymems)}
    jcltgts         := ${addpdsname $(jcllib),$(jclmems)}
    lkedtgts        := ${addpdsname $(lkedlib),$(lkedmems)}
    objtgts         := ${addpdsname $(objlib),$(asmmems)}
    loadtgts        := ${addpdsname $(loadlib),$(lkedmems)}

    .PHONY BUILD_ALL
    BUILD_ALL : $(recfmFB80) $(recfmFBA133) $(recfmVB32756) $(recfmVB1562)\
                $(recfmU)\
                $(cpytgts) $(asmtgts) $(jcltgts) $(lkedtgts)\
                $(loadtgts)

    $(cpytgts) : $(cpydir)/$%.asm
    - CALL OGET '$(cpydir)/$%.asm' '$@' TEXT CONVERT(YES)
    - x := ${sh cd $(asmdir);touch $(asmfiles)}
    
    $(asmtgts) : $(asmdir)/$%.asm
    - CALL OGET '$(asmdir)/$%.asm' '$@' TEXT CONVERT(YES)
    
    $(jcltgts) : $(jcldir)/$%.jcl
    - CALL OGET '$(jcldir)/$%.jcl' '$@' TEXT CONVERT(YES)
    
    $(lkedtgts) : $(lkeddir)/$%.lked
    - CALL OGET '$(lkeddir)/$%.lked' '$@' TEXT CONVERT(YES)
    
    $(objtgts) : $(asmlib)($%)
    - CALL ASMA SYSIN($(asmlib)($%)) SYSLIN($(objlib)($%))\
    -           SYSLIB($(syslib_asma)) SYSADATA($(sysadatalib)($%))\
    -           SYSPRINT($(asmlstlib)($%)) PARM(ADATA,GOFF,LIST(133))\
    -           PRINTSUCCESS(NO)
    - CALL TOUCHMEM DATASET($(objlib)($%))
    - CALL TOUCHMEM DATASET($(asmlstlib)($%))
    - CALL TOUCHMEM DATASET($(sysadatalib)($%))
    - CALL EQALANGX SYSADATA($(sysadatalib)($%))\
    -               IDILANGX($(eqalangxlib)($%)) PARM(ASM ERROR)
    - CALL TOUCHMEM DATASET($(eqalangxlib)($%))
    
    $(loadtgts) : $(objtgts) $(lkedlib)($%)
    - CALL LKED SYSLIN($(lkedlib)($%)) SYSLMOD($(loadlib)($%))\
    -           SYSLIB($(syslib_lked)) PARM(LIST,XREF,RENT,REUS)\
    -           PRINTSUCCESS(NO)

    $(recfmFB80) :
    - CALL TSOCMD ALLOC DATASET('$@') NEW RECFM(F,B) LRECL(80)\
    -             CYLINDERS SPACE(1,1) DSORG(PO) DSNTYPE(LIBRARY)
    - CALL TSOCMD FREE DATASET('$@')

    $(recfmFBA133) :
    - CALL TSOCMD ALLOC DATASET('$@') NEW RECFM(F,B,A) LRECL(133)\
    -             CYLINDERS SPACE(1,1) DSORG(PO) DSNTYPE(LIBRARY)
    - CALL TSOCMD FREE DATASET('$@')

    $(recfmVB32756) :
    - CALL TSOCMD ALLOC DATASET('$@') NEW RECFM(V,B) LRECL(32756)\
    -             CYLINDERS SPACE(1,1) DSORG(PO) DSNTYPE(LIBRARY)
    - CALL TSOCMD FREE DATASET('$@')

    $(recfmVB1562) :
    - CALL TSOCMD ALLOC DATASET('$@') NEW RECFM(V,B) LRECL(1562)\
    -             CYLINDERS SPACE(1,1) DSORG(PO) DSNTYPE(LIBRARY)
    - CALL TSOCMD FREE DATASET('$@')

    $(recfmU) :
    - CALL TSOCMD ALLOC DATASET('$@') NEW RECFM(U) LRECL(0) BLKSIZE(32760)\
    -             CYLINDERS SPACE(1,1) DSORG(PO) DSNTYPE(LIBRARY)
    - CALL TSOCMD FREE DATASET('$@')

So let's start with the first line (after the top comment)

    .USSHOME = <homedir>  # <== set to your own home directory
    
We need to set the `.USSHOME` special register because we want to use `${sh..}` later on.

The next 2 lines are meant to make it easy to customize the `makefile` to your site which might have LE and HLASM installed in different HLQ's.

    CEEHLQ          := CEE  # <== set to your LE data set HLQ
    HLAHLQ          := HLA  # <== set to your HLASM data set HLQ

The next line declares the `gitdir` variable, which you should leave the weird value of `@@GITDIR@@`.

    gitdir          := @@GITDIR@@
    
The `@@GITDIR@@` value is substituted by the `build.sh` shell script with `<git_repo_dir>/SOURCE`.

The next lines get the current branch your Git repo is on.

    feature         := ${sh cd $(gitdir);git branch | \
                         grep -E "^\\* (.*)$" | cut -d' ' -f2 }

How this works is
- `cd $(gitdir)` changes the directory to the Git repo directory
- `git branch` returns a list of branches, each on a separate line, the checked out branch preceded with an asterisk (*)
- `grep -E "^\\* (.*)$"` reduces the list of branches to just the one with the asterisk by using a regular expression.
   - `^` is the start of a line
   - `\\* ` is actually `\*`, but since `\` is the `LWZMAKE` line continuation character we needed to escape it. So what goes into `grep` is `\*`, which in itself is an escaped asterisk because we're looking for the literal character, not some regular expression character used to match zero or more characters.
   - `(.*)` means there can be anything following the `* ` and a line will still match
   - `$` is the end of a line
- `cut -d' ' -f2` reduces the found line to just the second word in the line

The next line simply converts the found branch name to upper case (to be used as a qualified in data set names).

    feature_upper   := ${sh echo "$(feature)"|tr 'a-z' 'A-Z'}

The next line declares the `hlq` variable, which is appended with multiple low level qualifiers to create MVS data sets to store the `LWZMAKE` sources and build outputs.

    hlq             := <your_hlq>.$(feature_upper)  # <== choose a HLQ

You are supposed to update *<your_hlq>* to a high level qualifier of your choice. As you can see, the currently checked out Git branch name becomes a qualifier in the data set names.

The following lines define all of the MVS data set names needed to store all of `LWZMAKE's` source files and build outputs.

    asmlib          := $(hlq).ASM
    asmlstlib       := $(asmlib).LISTING
    cpylib          := $(hlq).COPY
    jcllib          := $(hlq).JCL
    objlib          := $(hlq).OBJECT
    sysadatalib     := $(hlq).SYSADATA
    eqalangxlib     := $(hlq).EQALANGX
    lkedlib         := $(hlq).LKED
    loadlib         := $(hlq).LOAD

So if for example you used `LWZMAKE` as your high level qualifier and your Git repository currently has the *master* branch checked out, the last of these lines would resolve to:

    loadlib         := LWZMAKE.MASTER.LOAD
    
The next lines assign 2 variables a list of data set names. `syslib_asma` is used as the DD concatenation where the Assembler looks for members to resolve COPY statements or macro's. `syslib_lked` is used as the DD concatenation where the Binder looks for object and/or load modules to link-edit along into the `LWZMAKE` load module.

    syslib_asma     := SYS1.MACLIB SYS1.MODGEN $(CEEHLQ).SCEEMAC\
                       $(HLAHLQ).SASMMAC2 $(cpylib)
    syslib_lked     := $(CEEHLQ).SCEELKED $(objlib)

The next 5 lines group the source and build output PDS's by their physical characteristics.

    recfmFB80       := $(asmlib) $(cpylib) $(jcllib) $(objlib) $(lkedlib)
    recfmFBA133     := $(asmlstlib)
    recfmVB32756    := $(sysadatalib)
    recfmVB1562     := $(eqalangxlib)
    recfmU          := $(loadlib)

The *recfm\** variables are used further on as build targets to make sure all these data sets get allocated before the real building begins.
 
The next 4 lines declare variables with the `LWZMAKE` source directories in USS.
 
    asmdir          := $(gitdir)/ASM
    cpydir          := $(gitdir)/COPY
    jcldir          := $(gitdir)/JCL
    lkeddir         := $(gitdir)/LKED
 
The next line lists the files in the `$(asmdir)` directory and stores it in the `asmfiles` variable.

    asmfiles        := ${sh cd $(asmdir);find * -prune -type f}

`find * -prune -type f` will search for only files (no directories) because of `-type f` and it will not traverse any subdirectories because of `-prune` (there shouldn't be any, but just to be on the safe side).  
So `asmfiles` will contain `CEEUOPT.asm LWZMAKE.asm LWZMAVL.asm LWZMFMG.asm LWZMINP.asm LWZMLOG.asm LWZMPRS.asm LWZMREX.asm LWZMSTM.asm LWZMSTR.asm LWZMTOK.asm LWZMUSS.asm LWZMVCP.asm`.

The next line strips each of those `asmfiles` of their file extensions.

    asmmems         := ${stripext $(asmfiles)}
    
So `asmmems` will contain `CEEUOPT LWZMAKE LWZMAVL LWZMFMG LWZMINP LWZMLOG LWZMPRS LWZMREX LWZMSTM LWZMSTR LWZMTOK LWZMUSS LWZMVCP`.

The next couple of lines repeat these same 2 variable assignments for the other source types.

    cpyfiles        := ${sh cd $(cpydir);find * -prune -type f}
    cpymems         := ${stripext $(cpyfiles)}

    jclfiles        := ${sh cd $(jcldir);find * -prune -type f}
    jclmems         := ${stripext $(jclfiles)}

    lkedfiles       := ${sh cd $(lkeddir);find * -prune -type f}
    lkedmems        := ${stripext $(lkedfiles)}

The next line turns the `asmmems` source member names into fully qualified data set names by adding the `asmlib` variable's data set name and member brackets.

    asmtgts         := ${addpdsname $(asmlib),$(asmmems)}

So if for example your `hlq` variable contains `LWZMAKE.MASTER` and `asmlib` contains `LWZMAKE.MASTER.ASM`, then `asmtgts` will contain `LWZMAKE.MASTER.ASM(CEEUOPT) LWZMAKE.MASTER.ASM(LWZMAKE) LWZMAKE.MASTER.ASM(LWZMAVL) LWZMAKE.MASTER.ASM(LWZMFMG) LWZMAKE.MASTER.ASM(LWZMINP) LWZMAKE.MASTER.ASM(LWZMLOG) LWZMAKE.MASTER.ASM(LWZMPRS) LWZMAKE.MASTER.ASM(LWZMREX) LWZMAKE.MASTER.ASM(LWZMSTM) LWZMAKE.MASTER.ASM(LWZMSTR) LWZMAKE.MASTER.ASM(LWZMTOK) LWZMAKE.MASTER.ASM(LWZMUSS) LWZMAKE.MASTER.ASM(LWZMVCP)`.

The next couple of lines repeat the same step for the other source types.

    cpytgts         := ${addpdsname $(cpylib),$(cpymems)}
    jcltgts         := ${addpdsname $(jcllib),$(jclmems)}
    lkedtgts        := ${addpdsname $(lkedlib),$(lkedmems)}
    
Then there are 2 more lines very similar which declare `objtgts` as a list of object modules in fully qualified data set names, and `loadtgts` as a list of load modules in fully qualified data set names (actually only one load module).
    
    objtgts         := ${addpdsname $(objlib),$(asmmems)}
    loadtgts        := ${addpdsname $(loadlib),$(lkedmems)}

Now follows the most important rule statement in the `makefile` which defines the PHONY target BUILD_ALL which is the default target that should get `LWZMAKE` built.

    .PHONY BUILD_ALL
    BUILD_ALL : $(recfmFB80) $(recfmFBA133) $(recfmVB32756) $(recfmVB1562)\
                $(recfmU)\
                $(cpytgts) $(asmtgts) $(jcltgts) $(lkedtgts)\
                $(loadtgts)

`BUILD_ALL` itself is PHONY, so not a real data set name, and there's no recipe below this rule. That is because `BUILD_ALL` is merely an anchor to link lots of other targets to in the form of prerequisites to get those built in the proper order.

`LWZMAKE` will go through the list of prerequisites one by one, from left to right, from top to bottom.

Starting with the *recfm\** targets which are in rules all the way at the bottom of the `makefile`.

Then follow `$(cpytgts) $(asmtgts) $(jcltgts) $(lkedtgts)`, which resolve to the total list of all of the source members in PDS's that make up `LWZMAKE`.

Finally there's `$(loadtgts)` which resolves to the `LWZMAKE` load module we want to have at the end of the build.

Everything after this rule are other rules and recipes for certain build tasks.

Beginning with the rule to get `SOURCE/COPY/*` files copied to the COPY PDS.

    $(cpytgts) : $(cpydir)/$%.asm
    - CALL OGET '$(cpydir)/$%.asm' '$@' TEXT CONVERT(YES)
    - x := ${sh cd $(asmdir);touch $(asmfiles)}
    
`$(cpytgts)` resolves to the full list of fully qualified data sets for all the COPY members. Each is checked against one prerequisite, which is the identically named (`$%`) USS file with the .asm extension attached to it and located in the `$(cpydir)` USS directory for COPY files.

If `LWZMAKE` determines that a USS copy file has a more recent modified date than it's equally named target in the PDS, or if the target in the PDS doesn't exist, then the recipe is executed.

The recipe consists of 2 actions:

- invoke the REXX EXEC called OGET which copies the USS file to the PDS and converts the coded character set on the go (in this case only from IBM-1047 to IBM-037, which doesn't change much except square brackets [] and the not ¬ sign).
- execute a small shell command line that sets the last modified date of the `$(asmfiles)` to 'now' (meaning all of the Assembler sources other than COPY files, so the ones that are actually ran through the assembler utility). This makes sure that all the `$(asmfiles)` get built whenever a COPY member changes, even when the Assembler source itself didn't change, and should the build fail, then a rerun of the job will pick these up all the same.

The next couple of lines are more of the same:

    $(asmtgts) : $(asmdir)/$%.asm
    - CALL OGET '$(asmdir)/$%.asm' '$@' TEXT CONVERT(YES)
    
    $(jcltgts) : $(jcldir)/$%.jcl
    - CALL OGET '$(jcldir)/$%.jcl' '$@' TEXT CONVERT(YES)
    
    $(lkedtgts) : $(lkeddir)/$%.lked
    - CALL OGET '$(lkeddir)/$%.lked' '$@' TEXT CONVERT(YES)
    
These rules & recipes get the other source USS files copied to their identically named targets in PDS's whenever the USS file is modified more recently or if the targets in the PDS's don't exist.

Next is the target that does the actual Assembler execution:

    $(objtgts) : $(asmlib)($%)
    - CALL ASMA SYSIN($(asmlib)($%)) SYSLIN($(objlib)($%))\
    -           SYSLIB($(syslib_asma)) SYSADATA($(sysadatalib)($%))\
    -           SYSPRINT($(asmlstlib)($%)) PARM(ADATA,GOFF,LIST(133))\
    -           PRINTSUCCESS(NO)
    - CALL TOUCHMEM DATASET($(objlib)($%))
    - CALL TOUCHMEM DATASET($(asmlstlib)($%))
    - CALL TOUCHMEM DATASET($(sysadatalib)($%))
    - CALL EQALANGX SYSADATA($(sysadatalib)($%))\
    -               IDILANGX($(eqalangxlib)($%)) PARM(ASM ERROR)
    - CALL TOUCHMEM DATASET($(eqalangxlib)($%))

This rule applies to all `$(objtgts)` which are all the object modules that should exist after a successful Assembly of each of the asm sources. Each object module is dependent on its identically named (`$%`) Assembler source in the `$(asmlib)` PDS (previously copied from the `$(asmdir)` USS directory). If the asm source was modified more recently than the corresponding object module, or if the object module doesn't exist, the recipe is executed.

The recipe consists of a number of tasks:

- A REXX EXEC called ASMA parses the parameter it was passed, dynamically allocates the necessary DD's and invokes the ASMA90 utility to Assemble a source. What each parameter means is described in the comments at the top of the [REXX EXEC](SOURCE/EXEC/ASMA.rexx).
- 3 x TOUCHMEM sets the last modified date & time for 3 Assembly output members to 'now' because the Assembler doesn't automatically do so.
- A REXX EXEC called EQALANGX parses the parameter it was passed, dynamically allocates the necessary DD's and invokes the EQALANGX utility to produce a side file needed for debugging. What each parameter means is described in the comments at the top of the [REXX EXEC](SOURCE/EXEC/EQALANGX.rexx).
- Finally one more TOUCHMEM to set the last modified date & time for the just created EQALANGX file to 'now' because the utility doesn't automatically do so.

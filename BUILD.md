# How LWZMAKE builds itself
After a fresh clone of this Git repository these are the necessary steps needed to have `LWZMAKE` build itself:

- Copy these files from .jcl.template to .jcl
  - [SOURCE/JCL/BUILD.jcl.template](SOURCE/JCL/BUILD.jcl.template) => SOURCE/JCL/BUILD.jcl
  - [SOURCE/JCL/INITPDS.jcl.template](SOURCE/JCL/INITPDS.jcl.template) => SOURCE/JCL/INITPDS.jcl
  - [SOURCE/JCL/ISPFMAKE.jcl.template](SOURCE/JCL/ISPFMAKE.jcl.template) => SOURCE/JCL/ISPFMAKE.jcl
  - [SOURCE/JCL/JOBSTMT.jcl.template](SOURCE/JCL/JOBSTMT.jcl.template) => SOURCE/JCL/JOBSTMT.jcl
  
  There's an entry in .gitignore to exclude JCL's from the Git repo, so don't worry about muddying it up.
- Edit each of them and change the files to suit your needs.  
  In BUILD.jcl you need to check if CEEHLQ and HLAHLQ are set to the correct HLQ.  
  In INITPDS.jcl you need to provide a job statement and fill in the variables LWZMHLQ and GITDIR.  
  In ISPFMAKE.jcl you need to check if ISPHLQ is set to the correct HLQ, and perhaps check if the ISPF libraries are namd correcly (SISP%ENU).  
  In JOBSTMT.jcl you need to provide a job statement and fill in the variable LWZMHLQ.
- Run INITPDS.jcl.  
  It will allocate 3 PDS's with the provided HLQ in LWZMHLQ. These PDS's are the EXEC, JCL and LOAD libraries and they will be populated with the minimum of members needed. Unlike the PDS's that `LWZMAKE` will build in a minute, there's no Git branch name in these data sets.
- Run [SOURCE/build.sh](SOURCE/build.sh)  
  It will create all of the `LWZMAKE` build source and output PDS's, copy the sources from USS to PDS's and Assemble and link-edit `LWZMAKE`.  
  The reason for doing it with `build.sh` is so that you don't have to put your home directory and your repository clone's directory in `BUILD.jcl`. That way the build can be used by different people and the cloned repo can theoretically be moved somewhere else.
- Optionally change SOURCE/JCL/JOBSTMT.jcl to append the MASTER branch name in the LWZMHLQ variable so that any incremental builds from now on use the previously built `LWZMAKE` and then you can delete the 3 PDS's created by the SOURCE/JCL/INITPDS.jcl job.

The rest of this page takes the `makefile` in BUILD.jcl apart to explain bit by bit what it's doing.

Here's the entire makefile:

    # build LWZMAKE using LWZMAKE

    .USSHOME = @@HOMEDIR@@  # <== change if you want a different home dir

    CEEHLQ          := CEE  # <== set to your LE data set HLQ
    HLAHLQ          := HLA  # <== set to your HLASM data set HLQ

    gitdir          := @@GITDIR@@

    feature         := ${sh cd $(gitdir);git branch | \
                         grep -E "^\\* (.*)$" | cut -d' ' -f2 }
    feature_upper   := ${sh echo "$(feature)"|tr 'a-z' 'A-Z'}

    hlq             := &LWZMHLQ..$(feature_upper)  # <== choose a HLQ

    execlib         := $(hlq).EXEC
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

    recfmFB80       := $(execlib) $(asmlib) $(cpylib) $(jcllib) $(objlib)\
                       $(lkedlib)
    recfmFBA133     := $(asmlstlib)
    recfmVB32756    := $(sysadatalib)
    recfmVB1562     := $(eqalangxlib)
    recfmU          := $(loadlib)

    execdir         := $(gitdir)/EXEC
    asmdir          := $(gitdir)/ASM
    cpydir          := $(gitdir)/COPY
    jcldir          := $(gitdir)/JCL
    lkeddir         := $(gitdir)/LKED

    execfiles       := ${sh cd $(execdir);find *.rexx -prune -type f}
    execmems        := ${stripext $(execfiles)}
    
    asmfiles        := ${sh cd $(asmdir);find *.asm -prune -type f}
    asmmems         := ${stripext $(asmfiles)}
    
    cpyfiles        := ${sh cd $(cpydir);find *.asm -prune -type f}
    cpymems         := ${stripext $(cpyfiles)}

    jclfiles        := ${sh cd $(jcldir);find *.jcl -prune -type f}
    jclmems         := ${stripext $(jclfiles)}

    lkedfiles       := ${sh cd $(lkeddir);find *.lked -prune -type f}
    lkedmems        := ${stripext $(lkedfiles)}

    exectgts        := ${addpdsname $(execlib),$(execmems)}
    asmtgts         := ${addpdsname $(asmlib),$(asmmems)}
    cpytgts         := ${addpdsname $(cpylib),$(cpymems)}
    jcltgts         := ${addpdsname $(jcllib),$(jclmems)}
    lkedtgts        := ${addpdsname $(lkedlib),$(lkedmems)}
    objtgts         := ${addpdsname $(objlib),$(asmmems)}
    loadtgts        := ${addpdsname $(loadlib),$(lkedmems)}

    .PHONY BUILD_ALL
    BUILD_ALL : $(recfmFB80) $(recfmFBA133) $(recfmVB32756) $(recfmVB1562)\
                $(recfmU)\
                $(exectgts) $(cpytgts) $(asmtgts) $(jcltgts) $(lkedtgts)\
                $(loadtgts)

    $(exectgts) : $(execdir)/$%.rexx
    - CALL OGET '$(execdir)/$%.rexx' '$@' TEXT CONVERT(YES)
    
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

    .USSHOME = @@HOMEDIR@@  # <== change if you want a different home dir
    
We need to set the `.USSHOME` special register because we want to use `${sh..}` later on. You should leave the weird value of `@@HOMEDIR@@` because it is substituted by the `build.sh` shell script with your home directory.

The next 2 lines are meant to make it easy to customize the `makefile` to your site which might have LE and HLASM installed in different HLQ's.

    CEEHLQ          := CEE  # <== set to your LE data set HLQ
    HLAHLQ          := HLA  # <== set to your HLASM data set HLQ

The next line declares the `gitdir` variable, which you should also leave the weird value of `@@GITDIR@@`.

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

    hlq             := &LWZMHLQ..$(feature_upper)  # <== choose a HLQ

You can leave the `&LWZMHLQ.` variable there to have all the allocated PDS's to start with the same HLQ as your LWZMAKE binary used to do this build. But can change it to a high level qualifier of your choice. As you can see, the currently checked out Git branch name becomes a qualifier in the data set names.

The following lines define all of the MVS data set names needed to store all of `LWZMAKE's` source files and build outputs.

    execlib         := $(hlq).EXEC
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

The next 6 lines group the source and build output PDS's by their physical characteristics.

    recfmFB80       := $(execlib) $(asmlib) $(cpylib) $(jcllib) $(objlib)\
                       $(lkedlib)
    recfmFBA133     := $(asmlstlib)
    recfmVB32756    := $(sysadatalib)
    recfmVB1562     := $(eqalangxlib)
    recfmU          := $(loadlib)

The *recfm\** variables are used further on as build targets to make sure all these data sets get allocated before the real building begins.
 
The next 5 lines declare variables with the `LWZMAKE` source directories in USS.
 
    execdir         := $(gitdir)/EXEC
    asmdir          := $(gitdir)/ASM
    cpydir          := $(gitdir)/COPY
    jcldir          := $(gitdir)/JCL
    lkeddir         := $(gitdir)/LKED
 
The next line lists the files in the `$(execdir)` directory and stores it in the `execfiles` variable.

    execfiles       := ${sh cd $(execdir);find *.rexx -prune -type f}

`find *.rexx -prune -type f` will search for only files (no directories) because of `-type f` and it will not traverse any subdirectories because of `-prune` (there shouldn't be any, but just to be on the safe side).  
So `execfiles` will contain a list of REXX EXECs like `APNDALL.rexx APPEND.rexx ASMA.rexx ... WRITEREC.rexx`.

The next line strips each of those `execfiles` of their file extensions.

    execmems        := ${stripext $(execfiles)}
    
So `execmems` will contain a list like `APNDALL APPEND ASMA ... WRITEREC`.

The next couple of lines repeat these same 2 variable assignments for the other source types.

    asmfiles        := ${sh cd $(asmdir);find *.asm -prune -type f}
    asmmems         := ${stripext $(asmfiles)}
    
    cpyfiles        := ${sh cd $(cpydir);find *.asm -prune -type f}
    cpymems         := ${stripext $(cpyfiles)}

    jclfiles        := ${sh cd $(jcldir);find *.jcl -prune -type f}
    jclmems         := ${stripext $(jclfiles)}

    lkedfiles       := ${sh cd $(lkeddir);find *.lked -prune -type f}
    lkedmems        := ${stripext $(lkedfiles)}

The next line turns the `execmems` source member names into fully qualified data set names by adding the `execlib` variable's data set name and member brackets.

    exectgts        := ${addpdsname $(execlib),$(execmems)}

So if for example your `hlq` variable contains `LWZMAKE.MASTER` and `execlib` contains `LWZMAKE.MASTER.EXEC`, then `exectgts` will contain a list like `LWZMAKE.MASTER.EXEC(APNDALL) LWZMAKE.MASTER.EXEC(APPEND) LWZMAKE.MASTER.EXEC(ASMA) ... LWZMAKE.MASTER.EXEC(WRITEREC)`.

The next couple of lines repeat the same step for the other source types.

    asmtgts         := ${addpdsname $(asmlib),$(asmmems)}
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
                $(exectgts) $(cpytgts) $(asmtgts) $(jcltgts) $(lkedtgts)\
                $(loadtgts)

`BUILD_ALL` itself is PHONY, so not a real data set name, and there's no recipe below this rule. That is because `BUILD_ALL` is merely an anchor to link lots of other targets to in the form of prerequisites to get those built in the proper order.

`LWZMAKE` will go through the list of prerequisites one by one, from left to right, from top to bottom.

Starting with the *recfm\** targets which are in rules all the way at the bottom of the `makefile`.

Then follow `$(exectgts) $(cpytgts) $(asmtgts) $(jcltgts) $(lkedtgts)`, which resolve to the total list of all of the source members in PDS's that make up `LWZMAKE`.

Finally there's `$(loadtgts)` which resolves to the `LWZMAKE` load module we want to have at the end of the build.

Everything after this rule are other rules and recipes for certain build tasks.

Beginning with the rule to get `SOURCE/EXEC/*` files copied to the EXEC PDS.

    $(exectgts) : $(execdir)/$%.rexx
    - CALL OGET '$(execdir)/$%.rexx' '$@' TEXT CONVERT(YES)

`$(exectgts)` resolves to the full list of fully qualified data sets for all the EXEC members. Each is checked against one prerequisite, which is the identically named (`$%`) USS file with the .rexx extension attached to it and located in the `$(execdir)` USS directory for REXX EXEC files.

If `LWZMAKE` determines that a USS .rexx file has a more recent modified date than it's equally named target in the PDS, or if the target in the PDS doesn't exist, then the recipe is executed.

The recipe consists of one line to invoke the REXX EXEC called OGET which copies the USS file to the PDS and converts the coded character set on the go (in this case only from IBM-1047 to IBM-037, which doesn't change much except square brackets [] and the not ¬ sign).

Then there's a similar rule to get `SOURCE/COPY/*` files copied to the COPY PDS.

    $(cpytgts) : $(cpydir)/$%.asm
    - CALL OGET '$(cpydir)/$%.asm' '$@' TEXT CONVERT(YES)
    - x := ${sh cd $(asmdir);touch $(asmfiles)}
    
`$(cpytgts)` resolves to the full list of fully qualified data sets for all the COPY members. Each is checked against one prerequisite, which is the identically named (`$%`) USS file with the .asm extension attached to it and located in the `$(cpydir)` USS directory for COPY files.

If `LWZMAKE` determines that a USS copy file has a more recent modified date than it's equally named target in the PDS, or if the target in the PDS doesn't exist, then the recipe is executed.

This recipe consists of 2 actions:

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
- 3 x TOUCHMEM sets the last modified date & time for 3 Assembly output members to 'now' because the Assembler doesn't automatically do so. There's really only need to do so on the object module as that is the only build output used in future builds to compare against source files. But it's just good practice to have all ISPF statistics reflect the latest changes as much as possible.
- A REXX EXEC called EQALANGX parses the parameter it was passed, dynamically allocates the necessary DD's and invokes the EQALANGX utility to produce a side file needed for debugging. What each parameter means is described in the comments at the top of the [REXX EXEC](SOURCE/EXEC/EQALANGX.rexx).
- Finally one more TOUCHMEM to set the last modified date & time for the just created EQALANGX file to 'now' because the utility doesn't automatically do so.

Then follows the rule for link-editing our load module:

    $(loadtgts) : $(objtgts) $(lkedlib)($%)
    - CALL LKED SYSLIN($(lkedlib)($%)) SYSLMOD($(loadlib)($%))\
    -           SYSLIB($(syslib_lked)) PARM(LIST,XREF,RENT,REUS)\
    -           PRINTSUCCESS(NO)

For `LWZMAKE` there's actually only one load module, but for consistency the same type of variables are used here. So this rule applies to all `$(loadtgts)` which is just `LWZMAKE`. It is dependent on all `$(objtgts)` object modules and the identically named link-edit input member in the `$(lkedlib)` PDS. If any of the object modules or the link-edit input member are modified more recently than the `LWZMAKE` load module, or if the load module doesn't exist, the recipe is executed.

The recipe has only one task which is to call the REXX EXEC called LKED, which parses the parameter it was passed, dynamically allocates the necessary DD's and invokes the IEWBLINK utility (IEWL and HEWL are alias of IEWBLINK). What each parameter means is described in the comments at the top of the [REXX EXEC](SOURCE/EXEC/LKED.rexx).

Finally there's a few targets just to get some PDS's allocated. These are actually the first dependencies of the `BUILD_ALL` target, but they're all the way at the bottom because they're the least interesting. The first is:

    $(recfmFB80) :
    - CALL TSOCMD ALLOC DATASET('$@') NEW RECFM(F,B) LRECL(80)\
    -             CYLINDERS SPACE(1,1) DSORG(PO) DSNTYPE(LIBRARY)
    - CALL TSOCMD FREE DATASET('$@')

The `$(recfmFB80)` variable expands to all the build input and output PDS's of record length 80 and fixed blocked record format. There's no dependency so all `LWZMAKE` does is test if it exists and if not execute the recipe, which is to invoke the REXX EXEC called TSOCMD twice, one time to allocate the data set and another to free it. TSOCMD takes whatever parameter it was passed and tries to execute it as a TSO command.

The rest of the rules:

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

are more of the same, but for the different PDS characteristics. Each `recfm*` variable expands to a different set of PDS's that share the same record length and format.

Let's do one more walkthrough of what happens in an incremental build when for example the source `SOURCE/ASM/LWZMAKE.asm` is updated.

- During phase 1 all of the assignments are executed, `LWZMAKE` remembers each to the variables and their values. The rules and recipes are parsed and also committed to memory, but not executed yet.
- Assume the build was executed either without a parameter or with `-t BUILD_ALL`. This means that phase 2 starts with `BUILD_ALL` as the target to build.
- Its first dependencies are the `recfm*` variables that resolve to all the build input and output PDS names. `LWZMAKE` finds the rules for those, which have no dependencies, so they're simple allocated if they don't exist yet, but we're walking through an incremental build, so let's assume all of them already exist, so no allocations are performed.
- The next dependencies of `BUILD_ALL` are all of the source PDS members (ASM, COPY, JCL and LKED). They in turn are each dependent on identically named files with the proper file extensions attached and in their respective USS directories. `LWZMAKE` will go through each of them and compare last modified dates and it will find that only `$(asmdir)/LWZMAKE.asm` is newer than `$(asmlib)(LWZMAKE)`, so that USS file is copied to the PDS. The rest is left untouched.
- The last dependency of `BUILD_ALL` is the `LWZMAKE` load module. It in turn is dependent on all the object modules and the link-edit input member. The object modules in turn are dependent on the identically named ASM source members. So `LWZMAKE` processes those rules first, comparing all ASM source members and the identically named object modules. Of those, only the `$(asmlib)(LWZMAKE)` member is more recently updated than its object module, so the recipe for Assembling is only executed for that source, producing the `$(objlib)(LWZMAKE)` object module and updating the last modified date and time. Once all object module targets are checked and processed (one) `LWZMAKE` continues with the rule of the load module, comparing each of its dependent object module's modified date and times with that of the load module. At that point one is more recent, namely the `$(objlib)(LWZMAKE)` object module just produced. This causes `LWZMAKE` to execute the link-edit recipe and the load module is produced fresh.
- At the end `LWZMAKE`, having gone through `BUILD_ALL` dependencies, it will 'build' `BUILD_ALL` itself, but since it has no recipe, nothing happens.

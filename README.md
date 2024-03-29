# LWZMAKE
z/OS Light Weight Z make utility

## Introduction
**`LWZMAKE`** is an incremental build and deploy tool loosely based on **`make`** (well known in the Unix/Linux world). It's a tool specific for the Z System platform, with an emphasis on traditional 'MVS' partitioned data sets (PDS(E)'s and their members).

Just like `make` does, `LWZMAKE` can 'update files from others whenever the others change', e.g. copy members from source PDS's to target PDS's but only when the source PDS's members were updated more recently than the target ones. For PDS's that contain text members `LWZMAKE` uses ISPF statistics to determine which member was updated more recently. For load modules `LWZMAKE` invokes the z/OS binder utility to extract the link-edit date from the load module. For USS files it uses the last modified time.

Also just like with `make`, the way to tell the utility what to do is with a script in `LWZMAKE`'s script language. Such a script is often called a **`makefile`** (again loosely based on `make`'s script syntax).

Unlike `make`, instead of firing off command lines for performing build activities, you mostly call REXX EXECs to do those things (although you still can invoke USS commands as well).

In a z/OS development setting, for example for a COBOL application, `LWZMAKE` allows you to update any combination of sources and it will figure out what load module(s) as a result are in need of (re)building. If the `makefile` was written correctly, `LWZMAKE` will go through all of the application's artifacts and based on comparing last modified dates of all of them decide to (re)build the ones that belong to the updated sources. Hence the term 'incremental build tool'.

An incremental deploy works in exactly the same way. You deploy your entire application with `LWZMAKE`, which, again if your `makefile` was written correctly, will go through all deployable artifacts and figure out which ones in the set being deployed have a more recent last modified date than the ones at the environment being deployed to. The ones that are modified more recently are built (e.g. load modules copied from deploy input to runtime libraries), the rest is left untouched.

This page further describes how `LWZMAKE` works. If you wish to get started on you own system, have a look at [BUILD.md](BUILD.md) for the steps to take after a fresh Git clone to get `LWZMAKE` to build itself.

## A tiny sample `makefile` explained
Here's an example of a very simple `makefile`:

    srchlq  := SOMEUSR
    tgthlq  := MYUSR
    targets := $(tgthlq).PDS.JCL(MEM1) $(tgthlq).PDS.JCL(MEM2)

    .PHONY ALL
    ALL : $(targets)

    #* Copy MEM1 and MEM2, but only if they changed
    $(targets) : $(srchlq).PDS.JCL($%)
    - CALL IEBCOPY PDSIN($(srchlq).PDS.JCL) PDSOUT($(tgthlq).PDS.JCL) +
    -              MEMBER($%)

Let's break that down:

    srchlq  := SOMEUSR
    tgthlq  := MYUSR

The first two lines are easy enough to understand, they're simple **direct assignments** of a value to a variable name. Those variable names can then be used throughout the rest of the `makefile` by enclosing them in `$(..)` or `${..}`.

This is demonstrated in the third line which refers to the `tgthlq` variable in the assignment of `targets`.

    targets := $(tgthlq).PDS.JCL(MEM1) $(tgthlq).PDS.JCL(MEM2)

Directly after this line the variable `targets` contains:

    MYUSR.PDS.JCL(MEM1) MYUSR.PDS.JCL(MEM2)

The next two lines:

    .PHONY ALL
    ALL : $(targets)

are what's known as a **`rule`**. This first sample `rule` defines the "phony" target `ALL` and specifies on what files that target is dependent. A **`target`** is something `LWZMAKE` will potentially build. In a `rule` one or more `targets` can be specified left of the `:` character.
Right of the `:` character can optionally be `files` and/or other `targets` that the ones left of the `:` character are dependent on.
Designating a target as **`phony`** tells `LWZMAKE` the target is not a file with a last modified date & time, but rather just a name used to get its prerequisites built.

The next line is a comment line, which is ignored by `LWZMAKE`. Comments don't need to be on separate lines, if `LWZMAKE` encounters the `#*` characters it will ignore the rest of the line.

    #* Copy MEM1 and MEM2, but only if they changed

Then follows our second sample `rule` in which the value of the 'targets' variable, so our 2 members in fully qualified data set names, are defined as targets (because they precede the `:` character).

    $(targets) : $(srchlq).PDS.JCL($%)

Those targets have one prerequisite, which is a source PDS with a special variable **`$%`** as the member name. This `$%` variable resolves to the same member name as the target currently being built. So in this example, when `MYUSR.PDS.JCL(MEM1)` is being built, the prerequisite resolves to `SOMEUSR.PDS.JCL(MEM1)`, and for `MYUSR.PDS.JCL(MEM2)` it becomes `SOMEUSR.PDS.JCL(MEM2)`.

Below a rule are optionally lines that tell `LWZMAKE` what to do if it decides a target should be built, known as a **`recipe`**.

    - CALL IEBCOPY PDSIN($(srchlq).PDS.JCL) PDSOUT($(tgthlq).PDS.JCL) +
    -              MEMBER($%)

Such lines are coded with a `recipe prefix` which defaults to the `-` character. In this example a REXX called `IEBCOPY` is invoked, which parses the parameter it is passed (which is everything starting from `PDSIN` down to and including `MEMBER($%)`), dynamically allocates the required DD's and calls the IEBCOPY utility.

One more thing this `recipe` demonstrates is the `+` continuation character. This effectively turns these last 2 lines into one long string. And as you can see in the example, a continued recipe line still has to begin with the `recipe prefix` on position 1.

## LWZMAKE 2 build phases
To utilize `LWZMAKE` to its full potential, it's important to understand that it processes a `makefile` in 2 phases.
1. In the first phase the `makefile` is parsed, meaning it's checked for correct syntax and semantics. During this 1st phase the following variables are resolved:
    - variables in direct assignments, but only ones outside recipes
    - variables used on the left hand side of rule statements, so before the `:` character.
    - variables used in .PHONY statements

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;All other variables are left intact, so unresolved.

2. In the second phase `LWZMAKE` will apply its incremental logic to whatever target is was told to build. During this 2nd phase the following variables are resolved:
    - every target which `LWZMAKE` has a `rule` for, if the rule has prerequisites, so whatever is on the right hand side of the `:` character, any variables used in those prerequisites are resolved
    - if `LWZMAKE` decides a target should be built, it will execute the `recipe` lines directly following the rule statement, normally the lines starting with `-` on position 1, until a statement is found without the `-` or end of file. Any variables found in the recipe lines are resolved.

`LWZMAKE` is either told via a command switch (more on this later) what target to build in phase 2, or it will use the first target in the first rule it encounters in the `makefile`. In the example above that is the target `ALL`. It will first go through all of a target's prerequisites, checking whether those prerequisites are declared as targets themselves. If so, `LWZMAKE` will process those rules first, and it does so recursively. So if the rules for those prerequisites have prerequisites of their own, and those are also declared as targets in other rules, those get processed first, etc.

When all prerequisites have been processed, `LWZMAKE` will then compare the current target's last modified date with each of the prerequisites' and if any of the prerequisites were modified more recently, the recipe below the rule (if present) is executed, in other words the target is built.

## Special variables
`LWZMAKE` has 2 special variables:

- **`$@`** is resolved to the current target name, if that is a member in PDS, this is the fully qualified data set name and the member, e.g. `SOME.DATA.SET(MEMBER)`
- **`$%`** is resolved to the member name of the current target if the target is a member in a PDS, or it's resolved to the file name without its path if the target is a USS file. Otherwise the variable is empy.

Both of these special variables can only be used in recipes or on the right-hand side of a rule (so after the `:` character).

So if, for example in the `makefile` above, instead of copying source members to the target PDS we only wished to list the data set name and member, we could have used this recipe:

    - CALL JUSTECHO $@

`JUSTECHO` is another tiny sample REXX that simply echos back whatever parameter it was passed. In our example the `$@` resolves to `MYUSR.PDS.JCL(MEM1)` and `MYUSR.PDS.JCL(MEM2)`.

## 3 types of assignments
To assign a variable a value, `LWZMAKE` knows of 3 different assignment operators:

- **`:=`** for direct assignment, any variables used in the value part of the assignment are immediately resolved, or if the assignment is in a recipe they are resolved when the recipe is executed
- **`=`** for unresolved assignment, any variables used in the value part of the assignment are left unresolved. Only when these variables are used in another assignment, or in a rule or recipe do they get resolved.
- **`?=`** for conditional assignment, which only assigns a variable a value if the variable does not exist yet. If the variable was already assigned a value before, the conditional assignment statement is skipped. If the variable is indeed new, then the conditional assignment behaves like direct assignment.

When a variable is resolved, `LWZMAKE` does so recursively, meaning that if the resolved value contains another variable, that gets resolved too, and `LWZMAKE` will keep going until no other variables are found.

Consider the following sample sequence of assignments:

    hlq     =  QUAL1
    app     =  $(hlq).ABC
    jclpds1 =  $(app).JCL
    app     =  $(hlq).DEF
    jclpds1 ?= $(app).JCL.NEW
    jclpds2 =  $(app).JCL
    targets := $(jclpds1) $(jclpds2)

The value assigned to variable 'targets' is going to be `QUAL1.DEF.JCL QUAL1.DEF.JCL`. The conditional assignment of 'jclpds1' is skipped, because at that point 'jclpds1' already exists. It's only at the very last statement that any variable gets resolved and by that time 'app' contains '$(hlq).DEF' for both 'jclpds1' and 'jclpds2'.

## REXX
`LWZMAKE` reuses any REXX environment it finds. If `LWZMAKE` is the main program in your JCL step, then you only have basic REXX capabilities at your disposal. If `LWZMAKE` was started under for example IKJEFT01, you have access to TSO commands. When `LWZMAKE` was started from within an ISPF environment, you have access to ISPF functionality like JCL skeletons, ISPF tables, library management services, etc. There's a sample in this repository called `ISPFMAKE.jcl` with a JCL procedure for running `LWZMAKE` under ISPF.

Any REXX invoked in a `makefile` is searched in the `SYSEXEC` DD concatenation.

### Calling REXX in a recipe
REXX's are invoked in a recipe by coding the recipe prefix `-` followed by the `CALL` keyword, then the name of the REXX you wish to run and optionally a parameter string.

    - CALL <REXX exec> [<parameter string>]

Any REXX called in a recipe should return `0` to indicate success. Any other return value indicates a failure and will cause `LWZMAKE` to terminate.

### REXX functions
You can also invoke REXX as a function for example in a variable assignment:

<pre>--+-$(-+-function--<i>REXX_exec_name</i>--+-----------------------+--+-)-+--
  '-${-'                           '--,--<i>parameter_string</i>--'  '-}-'</pre>

For example, consider a REXX called 'RVRSWRDS' (reverse words ;-)):

    /* REXX */
    arg1 = Arg(1)
    wordcount = Words(arg1)

    ret = ""

    If wordcount > 0 Then Do
      ret = Word(arg1,wordcount)

      Do I = wordcount - 1 To 1 By -1
        ret = ret" "Word(arg1,I)
      End
    End

    Return ret

Such a function could be invoked like this and produce the result that the comment line describes:

    var1 := ${function RVRSWRDS,ABC 123}
    #* var1 := 123 ABC

Functions can be invoked anywhere where variables can be placed. A REXX function is resolved to whatever the REXX returns, so be careful with how you handle a failure within a REXX function, since the return value can not indicate a failure. The only way a REXX function can terminate `LWZMAKE` is by causing a REXX interpreter error (for example by trying to calculate with a non-numeric variable).

## Builtin functions
There are a few builtin functions that are so common that is was worth writing a routine for in `LWZMAKE`. These are:

### memberlist
<pre>--+-$(-+-memberlist--<i>PDS_data_set_name</i>--+--------------------+--+-)-+--
  '-${-+                                '--,--<i>member_filter</i>--'  '-}-'</pre>

For example, with SOME.DATA.SET containing members AA001, AA002, AB001 and AB002:

    someds := SOME.DATA.SET

    mems1 := $(memberlist $(someds))
    #* mem1 := AA001 AA002 AB001 AB002

    mems2 := ${memberlist $(someds),AA}
    # mems2 := AA001 AA002

`memberlist` retrieves a PDS(E)'s directory and lists the member names as a space delimited list.
The *member_filter* is optional and limits the returned member names to only ones that *start* with *member_filter*.

### addpdsname
<pre>                                           v------<------¬
--+-$(-+-addpdsname--<i>PDS_data_set_name</i>--,--+-<i>member_name</i>-+--+-)-+--
  '-${-'                                                    '-}-+</pre>

For example:

    mems  := FOO BAR
    tgtds := SOME.PDS.COB
    tgts  := ${addpdsname $(tgtds),$(mems)}
    #* tgts := SOME.PDS.COB(FOO) SOME.PDS.COB(BAR)

`addpdsname` adds the *PDS_data_set_name* to each *member_name* to form a complete data set name. Exactly one *PDS_data_set_name* is required. When after the comma no *member_name* is provided, the function returns an empty string. If multiple *member_names* are provided, they need to be space delimited.

### append
<pre>                                    v------------<------------¬
--+-$(-+-append--<i>text_to_append</i>--,--+-<i>words_that_get_appended</i>-+--+-)-+--
  '-${-'                                                         '-}-+</pre>

For example:

    suffix := 00
    mems   := WORDA WORDB WORDC
    mems   := ${append $(suffix),$(mems)}
    #* mems := WORDA00 WORDB00 WORDC00

`append` adds a *text_to_append* as a suffix to every space delimited word in *words_that_get_appended*.

### prepend
<pre>                                      v-------------<------------¬
--+-$(-+-prepend--<i>text_to_prepend</i>--,--+-<i>words_that_get_prepended</i>-+--+-)-+--
  '-${-'                                                            '-}-+</pre>

For example:

    prefix := A
    mems   := WORD1 WORD2 WORD3
    mems   := ${prepend $(prefix),$(mems)}
    #* mems := AWORD1 AWORD2 AWORD3

`prepend` adds a *text_to_prepend* as a prefix to every space delimited word in *words_that_get_prepended*.

### stripext
<pre>                   v----------------<---------------¬
--+-$(-+-stripext--+-<i>filename_to_strip_of_extension</i>-+--+-)-+--
  '-${-'                                               '-}-'</pre>

For example:

    files := file1.txt file2.txt file_without_ext file3.txt
    files := ${stripext $(files)}
    #* files := file1 file2 file_without_ext file3

`stripext` strips everything after the last period (.), including the period, found in each space delimited word in *filename_to_strip_of_extension*. If there's no period then the word is untouched.

## Executing shell command lines
Shell commands can be used in assignments and in recipes. `LWZMAKE` executes shell commands by calling the spawn callable service BPX1SPN. This can be one single command, or multiple commands separated by the command delimter ;.

### .USSHOME required
A special register `.USSHOME` needs to be assigned a USS directory which is to function as the home directory for the given commands. For example:

    .USSHOME = /u/yin/ybtk  #* directory to use as USS home dir

### SH in a recipe
You can issue a shell command line in a recipe similar to how you would call a REXX EXEC.

    - SH <command line>

For example, to copy an augmented JCL (by putting a job statement in front of it) from USS to a PDS you could:

    mypds  := QUAL1.PDS.JCL
    mydir  := /some/directory
    myjobs := FOOBAR
    tgts   := ${addpdsname $(mypds),$(myjobs)}

    $(tgts) : $(mydir)/$%.jcl
    - SH cd $(mydir);cat JOBSTMT.jcl $%.jcl > JOB.tmp
    - CALL OGET '$(mydir)/JOB.tmp' '$@' TEXT CONVERT(YES)
    - SH rm $(mydir)/JOB.tmp

### SH the builtin function
There's one more builtin function for executing shell command lines: sh.

<pre>--+-$(-+-sh--<i>shell_command(s)</i>--+-)-+--
  '-${-'                       '-}-'</pre>

For example, with a folder *somedir* that contains COB01.cbl, COB02.cbl and COB03.cbl:

    .USSHOME = /u/yin/ybtk  #* directory to use as USS home dir

    mydir   := ~/somedir
    myfiles := ${sh cd $(mydir);find *.cbl -prune -type f}
    #* myfiles := COB01.cbl COB02.cbl COB03.cbl

Although being able to execute shell commands opens a large range of capabilities, be aware that there's no way to indicate an error in the shell command line instructions that are executed. If one of the shell commands has an error, you will simply get the stdout and stderr output returned.

So for example:

    .USSHOME = /u/yin/ybtk

    test := ${sh not_a_valid_command}
    #* test := not_a_valid_command: FSUM7351 not found

and `LWZMAKE` will still end with CC 0.

## Special registers
`LWZMAKE` has these special registers:

### .PHONY
`.PHONY` is followed by one or more target names that are to be considered "phony" targets. Marking a target as phony tells `LWZMAKE` not to consider it a real file name and therefor not to look for a last modified date. Phony targets are always built, regardless of whether there are prerequisites and what their last modified dates are.

For example:

    tgts := AAA BBB CCC

    .PHONY ALL
    ALL : $(tgts)

    .PHONY $(tgts)
    $(tgts) :
    - CALL JUSTECHO $@

will result in 3 lines in SYSTSPRT

    AAA
    BBB
    CCC

Having the line with `.PHONY` just before the rule statement is just good practice, the line can be anywhere in the `makefile`. This script for example would have the exact same result:

    tgts := AAA BBB CCC

    ALL : $(tgts)

    $(tgts) :
    - CALL JUSTECHO $@

    .PHONY ALL $(tgts)

### .USSHOME
`.USSHOME` is a variable that is assigned a USS directory that `LWZMAKE` uses as the home directory when executing shell commands with the `$(sh ..)` function.

For example:

    .USSHOME = /tmp  #* instead of my own home dir, use /tmp

    var1 := ${sh myscript.sh}

Assuming there's a script `myscript.sh` in /tmp then `var1` will now contain the output of that script.

### .RECIPEPREFIX
`.RECIPEPREFIX` is a variable that contains a single character that is to be used as the recipe prefix in the rest of the `makefile`. Its default value is `-` which is used in the examples in this documentation. However if you have need of the `-` character for other reasons, you can switch to another recipe prefix.

For example:

    .RECIPEPREFIX = ^

    MYTARGET :
    ^ CALL JUSTECHO -$@-

### .BUILDWHEN
`.BUILDWHEN` is a variable which influences `LWZMAKE's` decisions on when to build a target. It has only a couple of accepted values:

- **`TOM`** is the **default**, it stands for `Timestamp Old or Missing`, meaning `LWZMAKE` will build a target if its last modified date is older than that of its prerequisites, or if the target doesn't exist (it's missing).
- `TUM` stands for `Timestamp Unequal or Missing`, meaning `LWZMAKE` will build a target if its last modified date is unequal to that of its prerequisites, or if the target doesn't exist (it's missing).
- `TO` stands for `Timestamp Old`, meaning `LWZMAKE` will build a target if its last modified date is older than that of its prerequisites, but it will skip the target if it doesn't exist.
- `TU` stands for `Timestamp Unequal`, meaning `LWZMAKE` will build a target if its last modified date is unequal to that of its prerequisites, but it will skip the target if it doesn't exist.

## More on rules
As stated above, a `rule` statement declares `targets` and their `prerequisites`:

<pre>  v----<---¬     v----<---¬
--+-<i>target</i>-+--<b>:</b>--+-<i>prereq</i>-+--</pre>

For example:

    #* Compile COBOL source to get object module
    #* Prerequisite to each object module is a COBOL source with the same
    #* member name ($%) and a shared copybook LCOB01
    MY.APP.OBJ(COB01) MY.APP.OBJ(COB02) : MY.APP.COB($%) MY.APP.CPY(LCOB01)

A *target* that isn't declared PHONY is assumed to be a file, which can be a member in a PDS(E), a sequential data set or a USS file. In every case it needs to be fully qualified, so MVS data sets are not prefixed with you user's HLQ and the USS files need to start with a forward slash /.

A rule is optionally followed by a `recipe`, which are the steps `LWZMAKE` is to take to build the target(s). If there's no recipe, the logging will still mention `LWZMAKE` builds a target, but apart from the logging line nothing else will happen for that target.

If more than 1 target in a rule share the same recipe, that recipe will possibly be executed for each of those targets, meaning `LWZMAKE` will process those recipe lines over and over.

*prereqs* can be other targets (real or PHONY) or they can be files that `LWZMAKE` doesn't know how to build, but merely checks for their existence and compares their last modified dates.
If they are defined as targets too, whether in a different rule or in the same one doesn't matter, that target gets processed first.

After potentially processing the prerequisites' recipes there is still a check for the prereqs existence and their last modified date. So even if a prereq was just built, it is not assumed that as a result the prereq has to thereby exist.

A special prerequisite comes in the form of a binary value of `0` or `1`. A prerequisite of `0` is ignored, but a `1` makes the build of the current target unconditional. This value 0 or 1 can of course be derived from a resolved variable or function.

For example:

    uncond := 1

    MY.APP.OBJ(COB01) : MY.APP.COB(COB01) $(uncond)
    - CALL JUSTECHO $@

## Running LWZMAKE
As mentioned before, `LWZMAKE` reuses any REXX environment it finds, which makes is it important to choose the best way to run it based on the REXX's you wish to execute and the functionality they require.

### Plain LWZMAKE
The simplest way to run `LWZMAKE` is by running it as the main JCL STEP program:

    //* Run LWZMAKE to build my app
    //ZMAKE   EXEC PGM=LWZMAKE,PARM='-t BUILD_APP'
    //STEPLIB   DD DISP=SHR,DSN=LWZMAKE.MASTER.LOAD
    //SYSEXEC   DD DISP=SHR,DSN=LWZMAKE.MASTER.EXEC
    //LWZMINP   DD DISP=SHR,DSN=MY.APP.CNTL(MAKEFILE)
    //LWZMLOG   DD SYSOUT=*,DCB=(RECFM=FB,LRECL=160)
    //SYSTSPRT  DD SYSOUT=*

- The optional parameter specifies the target to build (more on this later)
- STEPLIB needs to at least contain the `LWZMAKE` load module. If your REXX's invoke other loads, then their load libraries should be in STEPLIB also
- SYSEXEC is where `LWZMAKE` looks for your REXX EXECs. There are a number of sample REXX EXECs in this Git repository.
- LWZMINP is the DD that contains the `makefile`.
- LWZMLOG is where `LWZMAKE` writes its execution log
- SYSTSPRT is where REXX EXEC output goes

### LWZMAKE in TSO
If you need TSO commands in your REXX EXECs, you need to run LWZMAKE from TSO:

    //* Run LWZMAKE from TSO to build my app
    //ZMAKE   EXEC PGM=IKJEFT1B
    //SYSEXEC   DD DISP=SHR,DSN=LWZMAKE.MASTER.EXEC
    //SYSTSIN   DD *
    CALL 'LWZMAKE.MASTER.LOAD(LWZMAKE)' '-T BUILD_APP'
    //LWZMINP   DD DISP=SHR,DSN=MY.APP.CNTL(MAKEFILE)
    //LWZMLOG   DD SYSOUT=*,DCB=(RECFM=FB,LRECL=160)
    //SYSTSPRT  DD SYSOUT=*

- The parameter is optional (more on this later)
- STEPLIB is only needed if your REXX EXECs need certain load modules

### LWZMAKE in ISPF
There's a JCL procedure in this Git repository you can use to run `LWZMAKE` in ISPF, see [ISPFMAKE.jcl](SOURCE/JCL/ISPFMAKE.jcl).

Using this procedure running `LWZMAKE` from ISPF looks like this:

    //* Run LWZMAKE from ISPF to build my app
    //ZMAKE   EXEC PROC=ISPFMAKE,
    //             LWZMHLQ=LWZMAKE.MASTER,
    //             MAKEPARM='-T BUILD_APP',
    //             MAKEFILE=MY.APP.CNTL(MAKEFILE),
    //             EXECLIB=LWZMAKE.MASTER.EXEC

- The parameter is optional, you can omit the MAKEPARM parameter
- If your REXX EXECs need certain load modules, you will have to alter the ISPFMAKE.jcl procedure and add additional load libraries to the ISPLLIB DD concatenation

### Parameters to LWZMAKE
`LWZMAKE` accepts 2 parameters:

- <code>-t <<i>target to build</i>></code>
- <code>-v <<i>log level</i>></code>

*target_to_build* specifies which target `LWZMAKE` is supposed to build in phase 2 of its execution. Whatever is specified, it's parsed áfter phase 1, so you can use variables. For example given this `makefile`:

    hlq := QUAL1
    src := $(hlq).PDS.OLD(MEM1)
    tgt := $(hlq).PDS(MEM1)

    $(tgt) : $(src)
    - CALL JUSTECHO $@

it is perfectly valid to tell `LWZMAKE` to build <code>-t $(tgt)</code> and `QUAL1.PDS(MEM1)` will be built.

*log level* is a number that controls how detailed `LWZMAKE` will log its activities in the file attached to the `LWZMLOG` DD name. Valid values are:

- `0` = LOG_LEVEL_NONE: no logging produced
- `1` = LOG_LEVEL_ERROR: only error messages logged
- `2` = LOG_LEVEL_WARNING: error and warning messages logged
- <code>***3***</code> = LOG_LEVEL_INFO: error, warning and informational messages logged. This is the default.
- `4` = LOG_LEVEL_DEBUG: all of the above, plus debugging messages logged.
- `5` = LOG_LEVEL_DEBUG2: extra debug (only for investigating support issues)
- `6` = LOG_LEVEL_DEBUG3: extra extra debug (only for investigating support issues)

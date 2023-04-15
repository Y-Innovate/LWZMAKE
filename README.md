# LWZMAKE
z/OS Light Weight Z make utility

## Introduction
**`LWZMAKE`** is an incremental build and deploy tool loosely based on **`make`** (well known in the Unix/Linux world). It's a tool specific for the Z System platform, with an emphasis on traditional 'MVS' partitioned data sets (PDS(E)'s and their members).

Just like `make` does, `LWZMAKE` can 'update files from others whenever the others change', e.g. copy members from source PDS's to target PDS's but only when the source PDS's members were updated more recently than the target ones. For PDS's that contain text members `LWZMAKE` uses ISPF statistics to determine which member was updated more recently. For load modules `LWZMAKE` invokes the z/OS binder utility to extract the link-edit date from the load module. For USS files it uses the last modified time.

Also just like with `make`, the way to tell the utility what to do is with a script in `LWZMAKE`'s script language. Such a script is often called a **`makefile`** (again loosely based on `make`'s script syntax).

Unlike `make`, instead of firing off command lines for performing build activities, you mostly call REXX EXECs to do those things (although you still can invoke USS commands as well).

In a z/OS development setting, for example for a COBOL application, `LWZMAKE` allows you to update any combination of sources and it will figure out what load module(s) as a result are in need of (re)building. If the `makefile` was written correctly, `LWZMAKE` will go through all of the application's artifacts and based on comparing last modified dates of all of them decide to (re)build the ones that belong to the updated sources. Hence the term 'incremental build tool'.

An incremental deploy works in exactly the same way. You deploy your entire application with `LWZMAKE`, which, again if your `makefile` was written correctly, will go through all deployable artifacts and figure out which ones in the set being deployed have a more recent last modified date than the ones at the environment being deployed to. The ones that are modified more recently are built (e.g. load modules copied from deploy input to runtime libraries), the rest is left untouched.

## A tiny sample `makefile` explained
Here's an example of a very simple `makefile`:

    srchlq  := SOMEUSR
    tgthlq  := MYUSR
    targets := $(tgthlq).PDS.JCL(MEM1) $(tgthlq).PDS.JCL(MEM2)
    
    .PHONY ALL
    ALL : $(targets)
    
    # Copy MEM1 and MEM2, but only if they changed
    $(targets) : $(srchlq).PDS.JCL($%)
    - CALL IEBCOPY PDSIN($(srchlq).PDS.JCL) PDSOUT($(tgthlq).PDS.JCL) \
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

The next line is a comment line, which is ignored by `LWZMAKE`. Comments don't need to be on separate lines, if `LWZMAKE` encounters the `#` character it will ignore the rest of the line.

    # Copy MEM1 and MEM2, but only if they changed

Then follows our second sample `rule` in which the value of the 'targets' variable, so our 2 members in fully qualified data set names, are defined as targets (because they precede the `:` character).

    $(targets) : $(srchlq).PDS.JCL($%)

Those targets have one prerequisite, which is a source PDS with a special variable **`$%`** as the member name. This `$%` variable resolves to the same member name as the target currently being built. So in this example, when `MYUSR.PDS.JCL(MEM1)` is being built, the prerequisite resolves to `SOMEUSR.PDS.JCL(MEM1)`, and for `MYUSR.PDS.JCL(MEM2)` it becomes `SOMEUSR.PDS.JCL(MEM2)`.

Below a rule are optionally lines that tell `LWZMAKE` what to do if it decides a target should be built, known as a **`recipe`**.

    - CALL IEBCOPY PDSIN($(srchlq).PDS.JCL) PDSOUT($(tgthlq).PDS.JCL) \
    -              MEMBER($%)

Such lines are coded with a `recipe prefix` which defaults to the `-` character. In this example a REXX called `IEBCOPY` is invoked, which parses the parameter it is passed (which is everything starting from `PDSIN` down to and including `MEMBER($%)`), dynamically allocates the required DD's and calls the IEBCOPY utility.

One more thing this `recipe` demonstrates is the `\` continuation character. This effectively turns these last 2 lines into one long string. And as you can see in the example, a continued recipe line still has to begin with the `recipe prefix` on position 1.

## LWZMAKE 2 build phases
To utilize `LWZMAKE` to its full potential, it's important to understand that it processes a `makefile` in 2 phases.
1. In the first phase the `makefile` is parsed, meaning it's checked for correct syntax and semantics. During this 1st phase the following variables are resolved:
    - variables in direct assignments, but only ones outside recipes
    - variables used on the left hand side of rule statements, so before the `:` character.

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
    # var1 := 123 ABC

Functions can be invoked anywhere where variables can be placed. A REXX function is resolved to whatever the REXX returns, so be careful with how you handle a failure within a REXX function, since the return value can not indicate a failure. The only way a REXX function can terminate `LWZMAKE` is by causing a REXX interpreter error (for example by trying to calculate with a non-numeric variable). 

## Builtin functions
There are a few builtin functions that are so common that is was worth writing a routine for in `LWZMAKE`. These are:

### Memberlist
<pre>--+-$(-+-memberlist--<i>PDS_data_set_name</i>--+--------------------+--+-)-+--
  '-${-+                                '--,--<i>member_filter</i>--'  '-}-'</pre>

For example, with SOME.DATA.SET containing members AA001, AA002, AB001 and AB002:

    someds := SOME.DATA.SET
    
    mems1 := $(memberlist $(someds))
    # mem1 := AA001 AA002 AB001 AB002
    
    mems2 := ${memberlist $(someds),AA}
    # mems2 := AA001 AA002

`memberlist` retrieves a PDS(E)'s directory and lists the member names as a space delimited list.  
The *member_filter* is optional and limits the returned member names to only ones that *start* with *member_filter*.

### Addpdsname
<pre>                                           v------<------¬
--+-$(-+-addpdsname--<i>PDS_data_set_name</i>--,--+-<i>member_name</i>-+-)-+--
  '-${-'                                                 '-}-+</pre>

For example:

    mems  := FOO BAR
    tgtds := SOME.PDS.COB
    tgts  := ${addpdsname $(tgtds),$(mems)}
    # tgts := SOME.PDS.COB(FOO) SOME.PDS.COB(BAR)

`addpdsname` adds the *PDS_data_set_name* to each *member_name* to form a complete data set name. Exactly one *PDS_data_set_name* is required. When after the comma no *member_name* is provided, the function returns an empty string. If multiple *member_names* are provided, they need to be space delimited.

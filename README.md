# LWZMAKE
z/OS Light Weight Z make utility

## Introduction
**`LWZMAKE`** is an incremental build and/or deploy tool loosely based on **`make`** (well known in the *nix world). It's a tool specific for the Z System platform, with an emphasis on traditional 'MVS' partitioned data sets (PDS(E)'s and their members).

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

    targets := $(tgthlq).PDS.JCL(MEM1) $(tgthlq).PDS.JCL(MEM2)

This is demonstrated in the third line which refers to the `tgthlq` variable in the assignment of `targets`. Directly after this line the variable `targets` contains:

    MYUSR.PDS.JCL(MEM1) MYUSR.PDS.JCL(MEM2)

The next two lines:

    .PHONY ALL
    ALL : $(targets)

are what's known as a **`rule`**. This first sample `rule` defines the "phony" target `ALL` and specifies on what files that target is dependent. A **`target`** is something `LWZMAKE` will potentially build. In a `rule` one or more `targets` can be specified left of the `:` character.  
Right of the `:` character can optionally be `files` and or other `targets` that the ones left of the `:` character are dependent on.  
Designating a target as **`phony`** tells `LWZMAKE` the target is not a file with a last modified date & time, but rather just a name to get its prerequisites built.

    # Copy MEM1 and MEM2, but only if they changed

The next line is a comment line, which is ignored by `LWZMAKE`. Comments don't need to be on separate lines, if `LWZMAKE` encounters the `#` character it will ignore the rest of the line.

    $(targets) : $(srchlq).PDS.JCL($%)

Then follows our second sample `rule` in which the value of the targets variable, so our 2 members in fully qualified data set names, are defined as targets (because they precede the `:` character). And those targets have one prerequisite, which is a source PDS with a special variable **`$%`** as the member name. This `$%` variable resolves to the same member name as the target currently being built. So in this example, when `MYUSR.PDS.JCL(MEM1)` is being built, the prerequisite resolves to `SOMEUSR.PDS.JCL(MEM1)`, and for `MYUSR.PDS.JCL(MEM2)` it becomes `SOMEUSR.PDS.JCL(MEM2)`.

    - CALL IEBCOPY PDSIN($(srchlq).PDS.JCL) PDSOUT($(tgthlq).PDS.JCL) \
    -              MEMBER($%)

Below a rule are optionally lines that tell `LWZMAKE` what to do if it decides a target should be built, known as a **`recipe`**. Such lines are coded with a `recipe prefix` which defaults to the `-` character. In this example a REXX called `IEBCOPY` is invoked, which parses the parameter it is passed (which is everything starting from `PDSIN` down to and including `MEMBER($%)`), dynamically allocates the required DD's and calls the IEBCOPY utility.

One more thing this `recipe` demonstrates is the `\` continuation character. This effectively turns these last 2 lines into one long string. And as you can see in the example, a continued recipe line still has to begin with the `recipe prefix` on position 1.

## LWZMAKE 2 build phases
To utilize `LWZMAKE` to its full potential, it's important to understand that it processes a `makefile` in 2 phases.
1. In the first phase the `makefile` is parsed, meaning it's checked for correct syntax and semantics. During this 1st phase the following variables are resolved:
    - variables in direct assignments, but only ones outside recipes
    - variables used on the left hand side of rule statements, so before the `:` character.

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;All other variables are left intact, so unresolved.

2. In the second phase `LWZMAKE` will apply its incremental logic to whatever target is was told to build. During this 2nd phase the following variables are resolved:
    - every target `LWZMAKE` has a `rule` for, if the rule has prerequisites, so whatever is on the right hand side of the `:` character, any variables used in those prerequisites are resolved
    - if `LWZMAKE` decides a target should be built, it will execute the `recipe` lines directly following the rule statement, normally the lines starting with `-` on position 1, until a statement is found without the `-` or end of file. Any variables found in the recipe lines are resolved.

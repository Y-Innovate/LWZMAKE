# Samples
z/OS Light Weight Z make utility examples.

## Usage
To run the samples you need to 'build' them so they are copied from USS to PDS's.

There is a job prepared for that purpose which works in the same way as the job to build `LWZMAKE` with `LWZMAKE`.

You need to:

- copy [SAMPLES/JCL/JOBSTMT.jcl.template](SAMPLES/JCL/JOBSTMT.jcl.template) to `SAMPLES/JCL/JOBSTMT.jcl` and update it so that it contains a valid job statement
- run [SAMPLES/build.sh](SAMPLES/build.sh) which will prepend `SAMPLES/JCL/BUILD.jcl` with the job statement in `SAMPLES/JCL/JOBSTMT.jcl`, do some text replacements and submit the job for you.

The reason for doing it with `build.sh` is so that you don't have to put your home directory and your repository clone's directory in `BUILD.jcl` (causing a change which Git will want you to commit).

When the build has finished you should have a number of new PDS's with the samples ready to run. For each there will be a JCL PDS with a `BUILD` member to run the sample. Every `BUILD` member is prepended with the same `JOBSTMT.jcl` as is the BUILD.jcl to build the samples.

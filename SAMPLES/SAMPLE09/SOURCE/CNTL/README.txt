SAMPLE 09

This sample demonstrates:
- a tiny deploy makefile
- by getting a member list on a source pds
- creating a list of targets of those members, but with the target
  pds name
- and by making the targets dependent on the same member in the
  source pds
- that way whenever a member doesn't exist in the target pds or if
  the member in the source pds was touched more recently the recipe
  is invoked
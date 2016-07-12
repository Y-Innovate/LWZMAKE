SAMPLE 05

This sample demonstrates:
- using the := sign to indicate immediately resolved values, necessary
  to concatenate to an existing variable
- making one target dependent on another, in this case 2 MVS data sets
- multiple data set names in a single target variable, allowing the
  recipe to create each data set
- the $@ variable that is resolved to the current target
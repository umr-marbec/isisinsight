# ISIS-Fish outputs simulations formatting

Function for ISIS-Fish outputs simulations formatting and manipulation.

## Usage

``` r
outputs_simulations_settings(
  directory_path,
  output_path = NULL,
  output_format = "rds",
  input_colnames = TRUE,
  input_colnames_ids_names = NULL,
  round_significant_digits = 5L
)
```

## Arguments

- directory_path:

  Mandatory. Class character expected. Directory path of the ISIS-Fish
  outputs simulations.

- output_path:

  Optional. Default NULL. Class character expected. Output path for
  saved data from function element "simulations_data_improved_merged".
  If the value is NULL, nothing will be exported.

- output_format:

  Mandatory. Default "rds". Class character expected. Output(s) format
  expected. You wan choose between .rds or .csv (with ";" for the
  delimiter).

- input_colnames:

  Mandatory. Default TRUE. Class logical expected. Does the input(s)
  file(s) contain colnames. You can use the next argument to define more
  precisely the input file(s) concern.

- input_colnames_ids_names:

  Optional. Default NULL. Class character expected. Related to the
  previous argument, this one defines which input contains colname. If
  NULL, all the input contains colname.

- round_significant_digits:

  Mandatory. Default 5. Class integer expected. Round on a number of
  significant digits. Maximum 21.

## Value

The function returns a list with a length in relation to the number of
simulation directory provided. Each element of the list has information
about metadata and data (original and improved) associated with the
simulation.

## Examples

``` r
#replace the value of directory_path by a correct path
try(outputs_simulations_settings(directory_path = "my/path/to/simulations/directory"))
#> Error : Process stopped
```

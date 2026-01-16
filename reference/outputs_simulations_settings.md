# ISIS-Fish outputs simulations formatting

Function for ISIS-Fish outputs simulations formatting and manipulation.

## Usage

``` r
outputs_simulations_settings(directory_path)
```

## Arguments

- directory_path:

  Mandatory. Class character expected. Directory path of the ISIS-Fish
  outputs simulations.

## Value

The function returns a list with a length in relation to the number of
simulation directory provided. Each element of the list has information
about metadata and data (original and improved) associated with the
simulation.

## Examples

``` r
#replace the value of directory_path by a correct path
try(outputs_simulations_settings(directory_path = "my/path/to/simulations/directory"))
#> Error in outputs_simulations_settings(directory_path = "my/path/to/simulations/directory") : 
#>   2026-01-16 11:36:04 - Error, no input simulation available in the directory path.
```

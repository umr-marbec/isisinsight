# ISIS-Fish outputs simulations formatting

Function for ISIS-Fish outputs simulations formatting and manipulation.

## Usage

``` r
outputs_simulations_settings(directory_path, output_path = NULL)
```

## Arguments

- directory_path:

  Mandatory. Class character expected. Directory path of the ISIS-Fish
  outputs simulations.

- output_path:

  Optional. Default NULL. Output path for saved data from function
  element "simulations_data_improved_merged". If the value is NULL,
  nothing will be exported.

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
#>   2026-03-10 16:00:04 - Error, no input simulation available in the directory path.
```

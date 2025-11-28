# Checkpointing in MetaRVM

## Introduction

This vignette demonstrates how to configure and use the checkpointing
functionality in MetaRVM with examples. Checkpointing allows to store
the model state at specific time points during the simulation, allowing
one to resume runs from saved states.

## How Checkpointing Works

MetaRVM’s checkpointing system is configured through the YAML
configuration file.

1.  **Saving checkpoints**: During simulation, the complete state
    (compartments, parameters, time step) is saved to disk at specified
    dates
2.  **Restoring from checkpoints**: A new simulation can be initialized
    from a previously saved checkpoint, continuing from that point
    forward
3.  **Flexible scheduling**: Exact dates when checkpoints should be
    saved are to be supplied, or by default it will checkpoint at the
    end-of-simulation date.

### Configuration Parameters

The checkpointing system uses three main configuration options in the
`simulation_config` section:

- **`checkpoint_dir`**: Directory where checkpoint files will be saved
  (created if it doesn’t exist)
- **`checkpoint_dates`**: (Optional) List of specific dates when
  checkpoints should be saved
- **`restore_from`**: (Optional) Path to a checkpoint file to resume
  from

## Example

For the purpose of this example, we will set the checkpointed directory
to be a temporary directory, and will create a temporary yaml
configuration file based on a template.

### Set Up

``` r
library(MetaRVM)

# Get the example configuration file
example_config <- system.file("extdata", "example_config_checkpoint.yaml", 
                              package = "MetaRVM")

# Create a temporary directory for checkpoints
checkpoint_dir <- tempdir()
cat("Checkpoint directory:", checkpoint_dir, "\n")
#> Checkpoint directory: /tmp/RtmprlPA4V
```

### Create a Configuration with Checkpointing

Now copy the example configuration and modify it to add checkpointing:

``` r
# Read the example configuration
yml <- yaml::read_yaml(example_config)
yml_tmp <- data.table::copy(yml)

# Assign checkpoint directory
yml_tmp$simulation_config$checkpoint_dir <- checkpoint_dir

# Create a temporary config
temp_config <- tempfile(tmpdir = dirname(example_config), fileext = ".yaml")
yaml::write_yaml(yml_tmp, temp_config)
temp_config <- normalizePath(temp_config)
```

### Run Simulation with Checkpoints

``` r

# Run the simulation
results <- metaRVM(temp_config)
#> Loading required namespace: pkgbuild
#> Generating model in c
#> ℹ Re-compiling odin3a2926f6 (debug build)
#> ── R CMD INSTALL ───────────────────────────────────────────────────────────────
#> * installing *source* package ‘odin3a2926f6’ ...
#> ** this is package ‘odin3a2926f6’ version ‘0.0.1’
#> ** using staged installation
#> ** libs
#> using C compiler: ‘gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0’
#> gcc -std=gnu2x -I"/opt/R/4.5.2/lib/R/include" -DNDEBUG   -I/usr/local/include    -fpic  -g -O2  -UNDEBUG -Wall -pedantic -g -O0 -fdiagnostics-color=always -c odin.c -o odin.o
#> odin.c: In function ‘user_get_scalar_int’:
#> odin.c:1770:47: warning: format ‘%d’ expects argument of type ‘int’, but argument 2 has type ‘const char *’ [-Wformat=]
#>  1770 |       Rf_error("Expected scalar integer for '%d'", name);
#>       |                                              ~^    ~~~~
#>       |                                               |    |
#>       |                                               int  const char *
#>       |                                              %s
#> odin.c: In function ‘user_get_array’:
#> odin.c:1928:48: warning: format ‘%d’ expects argument of type ‘int’, but argument 2 has type ‘size_t’ {aka ‘long unsigned int’} [-Wformat=]
#>  1928 |         Rf_error("Incorrect size of dimension %d of %s (expected %d)",
#>       |                                               ~^
#>       |                                                |
#>       |                                                int
#>       |                                               %ld
#>  1929 |                  i + 1, name, dim_expected);
#>       |                  ~~~~~                          
#>       |                    |
#>       |                    size_t {aka long unsigned int}
#> odin.c: In function ‘interpolate_check_y’:
#> odin.c:2007:45: warning: format ‘%d’ expects argument of type ‘int’, but argument 3 has type ‘size_t’ {aka ‘long unsigned int’} [-Wformat=]
#>  2007 |       Rf_error("Expected %s to have length %d (for '%s')",
#>       |                                            ~^
#>       |                                             |
#>       |                                             int
#>       |                                            %ld
#>  2008 |                name_arg, nx, name_target);
#>       |                          ~~                  
#>       |                          |
#>       |                          size_t {aka long unsigned int}
#> odin.c:2011:37: warning: format ‘%d’ expects argument of type ‘int’, but argument 2 has type ‘size_t’ {aka ‘long unsigned int’} [-Wformat=]
#>  2011 |       Rf_error("Expected dimension %d of %s to have size %d (for '%s')",
#>       |                                    ~^
#>       |                                     |
#>       |                                     int
#>       |                                    %ld
#>  2012 |                i, name_arg, nx, name_target);
#>       |                ~                     
#>       |                |
#>       |                size_t {aka long unsigned int}
#> odin.c:2011:59: warning: format ‘%d’ expects argument of type ‘int’, but argument 4 has type ‘size_t’ {aka ‘long unsigned int’} [-Wformat=]
#>  2011 |       Rf_error("Expected dimension %d of %s to have size %d (for '%s')",
#>       |                                                          ~^
#>       |                                                           |
#>       |                                                           int
#>       |                                                          %ld
#>  2012 |                i, name_arg, nx, name_target);
#>       |                             ~~                             
#>       |                             |
#>       |                             size_t {aka long unsigned int}
#> gcc -std=gnu2x -I"/opt/R/4.5.2/lib/R/include" -DNDEBUG   -I/usr/local/include    -fpic  -g -O2  -UNDEBUG -Wall -pedantic -g -O0 -fdiagnostics-color=always -c registration.c -o registration.o
#> gcc -std=gnu2x -shared -L/opt/R/4.5.2/lib/R/lib -L/usr/local/lib -o odin3a2926f6.so odin.o registration.o -L/opt/R/4.5.2/lib/R/lib -lR
#> installing to /tmp/RtmprlPA4V/devtools_install_1a2b627f93f2/00LOCK-file1a2bff97b96/00new/odin3a2926f6/libs
#> ** checking absolute paths in shared objects and dynamic libraries
#> * DONE (odin3a2926f6)
#> ℹ Loading odin3a2926f6

# Check what checkpoint files were created
checkpoint_files <- list.files(checkpoint_dir, 
                               pattern = "^chk_.*\\.Rda$",
                               full.names = FALSE)

if (length(checkpoint_files) > 0) {
  for (file in checkpoint_files) {
    cat("  -", file, "\n")
  }
} else {
  cat("  (No checkpoint files found)\n")
}
#>   - chk_2024-12-29_1.Rda
```

### Examine the Results

``` r
cat("Number of instances:", results$run_info$n_instances, "\n")
#> Number of instances: 1
cat("Date range:", format(results$run_info$date_range[1]), "to", 
    format(results$run_info$date_range[2]), "\n")
#> Date range: 2024-10-01 to 2024-12-29

# Display first few rows
print(head(results$results, 10))
#>           date    age   race   zone disease_state        value instance
#>         <Date> <char> <char> <char>        <char>        <num>    <int>
#>  1: 2024-10-01   0-17      A     11             D 2.252583e-04        1
#>  2: 2024-10-01   0-17      A     11             E 1.305178e+01        1
#>  3: 2024-10-01   0-17      A     11             H 2.304447e-01        1
#>  4: 2024-10-01   0-17      A     11         I_all 2.731688e+01        1
#>  5: 2024-10-01   0-17      A     11       I_asymp 3.227854e-01        1
#>  6: 2024-10-01   0-17      A     11         I_eff 2.647304e+01        1
#>  7: 2024-10-01   0-17      A     11     I_presymp 7.531660e-01        1
#>  8: 2024-10-01   0-17      A     11        I_symp 2.624093e+01        1
#>  9: 2024-10-01   0-17      A     11             P 3.074200e+04        1
#> 10: 2024-10-01   0-17      A     11             R 1.148308e+01        1
```

### Resume from a Checkpoint

We’ll create a new configuration that restores from the checkpoint. We
need to provide the checkpoint file name, and a start date which should
be the next day of the end date in the previous run.

``` r
# Get the first checkpoint file
checkpoint_files_full <- list.files(checkpoint_dir, 
                                    pattern = "^chk_.*\\.Rda$",
                                    full.names = TRUE)

if (length(checkpoint_files_full) > 0) {
  checkpoint_to_restore <- checkpoint_files_full[1]
  
  cat(checkpoint_to_restore, "\n\n")
  
  new_start_date <- "12/30/2024"
  yml_tmp <- copy(yml)
  yml_tmp$simulation_config$start_date <- new_start_date
  yml_tmp$simulation_config$restore_from <- checkpoint_to_restore
  yml_tmp$population_data$initialization <- NULL # ensure that we don't want to reinitialize the population from the initialization file
  
  temp_config_resume <- tempfile(tmpdir = dirname(example_config),fileext = ".yaml")
  yaml::write_yaml(yml_tmp, temp_config_resume)
  temp_config_resume <- normalizePath(temp_config_resume)
  
  
  # Run the resumed simulation
  results_resumed <- metaRVM(temp_config_resume)
  
  cat("Number of instances:", results_resumed$run_info$n_instances, "\n")
  cat("Date range:", format(results_resumed$run_info$date_range[1]), "to", 
      format(results_resumed$run_info$date_range[2]), "\n")
  
} else {
  cat("No checkpoint files found to resume from.\n")
}
```

## Understanding Checkpoint Files

When checkpointing is enabled, MetaRVM creates checkpoint files with the
following naming convention:

    chk_YYYY-MM-DD_N.Rda

Where: - `YYYY-MM-DD` is the checkpoint date - `N` is the simulation
instance number (1 to nsim)

For more information on configuring MetaRVM simulations, see the [YAML
Configuration](https://RESUME-Epi.github.io/MetaRVM/articles/yaml-configuration.md)
vignette.

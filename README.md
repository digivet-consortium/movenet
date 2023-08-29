
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Movenet

Movenet is a data digitalisation toolkit being developed by the
[NordForsk DigiVet
project](https://www.nordforsk.org/projects/digitalisation-livestock-data-improve-veterinary-public-health).
It facilitates the dataflow from livestock movement data to social
network analysis and disease transmission models, while addressing
common data issues such as the diversity of data formats and privacy
preservation.

We are also developing an accompanying interactive web app (temporarily
housed [here](https://github.com/digivet-consortium/movenetapp)) to
cater to users who may prefer a graphical user interface.

**Disclaimer:** Movenet is under active development. The way functions
are called, and the internal methods, may still change without prior
warning.

## Workflows

Movenet facilitates a range of objectives and workflows pertaining to
the processing and analysis of livestock movement data and (optional)
holding data: - standardisation into a single format, allowing for
interoperability and integration of data from different countries or
systems; - pseudonymisation, and a range of options to make the data
non-identifiable; - generation of networks, and (basic) social network
analysis; - integration of the data into transmission models; -
exploration of the effects of different anonymisation options on
outcomes of network analyses and transmission models, so as to allow
users to find a suitable balance between the scale of anonymisation and
the accuracy of these outcomes.

The functionality made available by this package can be divided into
four sections: reading in and reformatting data; making data
non-identifiable; generation of networks and (basic) social network
analysis; and integration of data into transmission models (in
progress). The following flow chart illustrates how the key functions in
these categories relate to one another and can be used as reference for
how this package fits together.

<figure>
<img src="man/figures/readme-flowchart.svg"
alt="Flow chart split into sections corresponding to the next sections of this document. It shows the general flow of usage of the Movenet package, and how some of the important functions can be used to transform the data. This is covered in more detail in the next sections of this document." />
<figcaption aria-hidden="true">Flow chart split into sections
corresponding to the next sections of this document. It shows the
general flow of usage of the Movenet package, and how some of the
important functions can be used to transform the data. This is covered
in more detail in the next sections of this document.</figcaption>
</figure>

The remaining sections of this document explain how to use the package
in more detail, with code samples. For even more detail, consult the
documentation of each function.

## Reading in and reformatting data

A first step in any workflow is reading in livestock movement data (and
optional holding data). At this stage, the challenge is the diversity of
formats that movement and holding data can come in. Movenet addresses
this by requiring users to provide a configuration file, indicating
appropriate values for a range of factors that commonly vary between
datasets, such as the separator and date format used, as well as headers
of relevant columns. Example configuration files as well as an empty
template are available in the `/inst/configurations` folder. Using the
appropriate configurations, data are read in and converted to a
standardised Movenet format. This standardised format focuses on
minimally required data (from, to, date, weight) but allows for
additional columns; data types for specific columns are checked, and
where possible converted to standards (e.g. R Date format) to improve
interoperability.

To read in and reformat some data, first we select a configuration. We
do this using the `load_config(configfile)` function. For example, to
load the configuration for ScotEID data, we run:

``` r
# The ScotEID configuration is loaded from ScotEID.yml into Movenet's environment.
load_config("ScotEID")
#> Successfully loaded config file: ScotEID
```

Next, we can use the `reformat_data(datafile, type)` function to read
and reformat the data.

``` r
datafile <- "tests/testthat/test_input_files/sample_pigs_UK_with_dep_arr_dates.csv"
data <- reformat_data(datafile, "movement")
# Here we are using movement data, but we can also read and reformat holding
# data if we change the second parameter to "holding".
```

The reformatted data is returned as a tibble. The columns are in a
standardised order.

``` r
# Look at the first six rows of the tibble
head(data)
#> # A tibble: 6 × 5
#>   departure_cph dest_cph    departure_date qty_pigs movement_reference
#>   <chr>         <chr>       <date>            <dbl>              <dbl>
#> 1 95/216/1100   19/818/9098 2019-02-08           97             304781
#> 2 69/196/5890   71/939/3228 2019-08-15          167             229759
#> 3 52/577/5349   82/501/8178 2019-09-15          115              36413
#> 4 39/103/5541   13/282/1763 2019-10-26          125             488616
#> 5 41/788/6464   57/418/6011 2019-10-17          109             581785
#> 6 69/393/9398   39/947/2201 2019-10-06           72             564911
```

## Making data non-identifiable

To address concerns around commercial sensitivity, Movenet includes a
range of functions to make livestock movement data and/or holding data
non-identifiable. It can pseudonymise holding identifiers:

``` r
# Pseudonymise our data using the anonymise() function:
anonymised <- anonymise(data, "FARM")

data <- anonymised$data
anonymisation_key <- anonymised$key

# Now we can see the data has been pseudonymised:
head(data)
#> # A tibble: 6 × 5
#>   departure_cph dest_cph departure_date qty_pigs movement_reference
#>   <chr>         <chr>    <date>            <dbl>              <dbl>
#> 1 FARM132       FARM322  2019-02-08           97             304781
#> 2 FARM276       FARM496  2019-08-15          167             229759
#> 3 FARM329       FARM256  2019-09-15          115              36413
#> 4 FARM334       FARM316  2019-10-26          125             488616
#> 5 FARM393       FARM430  2019-10-17          109             581785
#> 6 FARM375       FARM178  2019-10-06           72             564911

# And if desired, we can keep the anonymisation key to reverse this process later:
head(anonymisation_key)
#> 79/642/5562 95/903/2776 86/580/7898 31/473/4857 77/458/3246 52/672/6036 
#>     "FARM1"     "FARM2"     "FARM3"     "FARM4"     "FARM5"     "FARM6"

# anonymise() also takes an optional `key` argument to use an existing key
```

It can also modify dates or weights by applying a small amount of noise
or by rounding:

``` r
# Use jitter to coarsen the dates
data <- data |> coarsen_date(
  jitter = 5, # add jitter of ±5 days
  rounding_unit = FALSE # do not round the data
)
#> Warning: As 'rounding_unit' is FALSE, no rounding or summarising by date has
#>     been performed. Arguments 'sum_weight' and '...' have been ignored.

# Use rounding to coarsen the weights
data <- data |> coarsen_weight(
  round = 10, # round weights to the nearest multiple of 10
  jitter = FALSE # do not jitter the data
)
```

A function to resample holding coordinates in a density-dependent manner
is under development. The envisaged effect is to increase data sharing
between different countries or organisations, which is of particular
relevance when modelling transboundary disease spread.

## Generation of networks and (basic) social network analysis

Movenet uses the [Statnet suite of
packages](https://statnet.org/packages/) (e.g. NetworkDynamic and tsna)
for (temporal) social network analysis. We are currently working on
functions in this section - e.g. creation of a pdf report with network
measures of particular relevance to disease transmission.

Currently it is already possible to generate a NetworkDynamic and
perform some kinds of analysis:

``` r
# Create a NetworkDynamic from our data
smallData <- head(data, n=300) # use a subset of our data so this notebook knits faster
network <- movedata2networkDynamic(smallData)
network
#> NetworkDynamic properties:
#>   distinct change times: 211 
#>   maximal time range: 17898 until  18257 
#> 
#>  Dynamic (TEA) attributes:
#>   Edge TEAs:    movement_reference.active 
#>      qty_pigs.active 
#> 
#> Includes optional net.obs.period attribute:
#>  Network observation period info:
#>   Number of observation spells: 1 
#>   Maximal time range observed: 17898 until 18257 
#>   Temporal mode: continuous 
#>   Time unit: unknown 
#>   Suggested time increment: NA 
#> 
#>  Network attributes:
#>   vertices = 364 
#>   directed = TRUE 
#>   hyper = FALSE 
#>   loops = FALSE 
#>   multiple = FALSE 
#>   bipartite = FALSE 
#>   net.obs.period: (not shown)
#>   vertex.pid = true_id 
#>   total edges= 300 
#>     missing edges= 0 
#>     non-missing edges= 300 
#> 
#>  Vertex attribute names: 
#>     active true_id vertex.names 
#> 
#>  Edge attribute names: 
#>     active movement_reference.active qty_pigs.active

# Perform some analysis - find the max reachability
parallel_max_reachabilities(list(network), n_threads=8)
#> [1] 8
```

## Integration of data into transmission models

Movenet links up with [SimInf](https://github.com/stewid/SimInf) for
transmission modelling. We are currently working on functions in this
section.

(A more extensive summary and usage/example code will be added shortly.)

## Setting a Default Configuration

If you use Movenet with the same configuration most or all of the time,
you may want to have Movenet automatically load a configuration file
when the Movenet package is loaded.

To do this, you should find the `movenet/R` directory (where all of the
package’s code is located). Create a file in this directory called
`zzz.R`. In this file, type the following code:

``` r
.onLoad <- function(libname, pkgname){
  load_config("ScotEID")
}
```

where `ScotEID` can be replaced with whatever configuration you wish to
load. Now, whenever the Movenet package is loaded, this `.onLoad`
function will be called, loading the desired configuration. See [this
page](https://r-pkgs.org/Code.html#sec-code-onLoad-onAttach) for more
information on `.onLoad` and zzz.R.

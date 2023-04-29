
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
housed [here](https://github.com/digivet-consortium/movenet_app)) to
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

(Usage/example code will be added shortly.)

## Making data non-identifiable

To address concerns around commercial sensitivity, Movenet includes a
range of functions to make livestock movement data and/or holding data
non-identifiable. It can pseudonymise holding identifiers, and modify
dates or weights by applying a small amount of noise or by rounding. A
function to resample holding coordinates in a density-dependent manner
is under development. The envisaged effect is to increase data sharing
between different countries or organisations, which is of particular
relevance when modelling transboundary disease spread.

(Usage/example code will be added shortly.)

## Generation of networks and (basic) social network analysis

Movenet uses the [Statnet suite of
packages](https://statnet.org/packages/) (e.g. NetworkDynamic and tsna)
for (temporal) social network analysis. We are currently working on
functions in this section - e.g. creation of a pdf report with network
measures of particular relevance to disease transmission.

(A more extensive summary and usage/example code will be added shortly.)

## Integration of data into transmission models

Movenet links up with [SimInf](https://github.com/stewid/SimInf) for
transmission modelling. We are currently working on functions in this
section.

(A more extensive summary and usage/example code will be added shortly.)

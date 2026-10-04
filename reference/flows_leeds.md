# Example flow data for Leeds. It is from the 2021 census, and it contains all Origin - Destination flows at the MSOA level. For more info on census flow data, see the [ONS documentation](https://www.ons.gov.uk/census/aboutcensus/censusproducts/origindestinationflowdata) See data-raw/flows_leeds.R for how this data was created.

Example flow data for Leeds. It is from the 2021 census, and it contains
all Origin - Destination flows at the MSOA level. For more info on
census flow data, see the [ONS
documentation](https://www.ons.gov.uk/census/aboutcensus/censusproducts/origindestinationflowdata)
See data-raw/flows_leeds.R for how this data was created.

## Usage

``` r
flows_leeds
```

## Format

An object of class
[`sf`](https://r-spatial.github.io/sf/reference/sf.html) with LINESTRING
geometry. It has the following columns:

- origin:

  MSOA code of origin zone

- destination:

  MSOA code of destination zone

- count:

  number of people moving from origin to destination

- geometry:

  desire line between origin and destination

## Source

<https://www.nomisweb.co.uk/sources/census_2021_od>

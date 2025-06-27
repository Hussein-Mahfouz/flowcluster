#' Example flow data for Leeds. It is from the 2021 census, and it contains all Origin - Destination 
#' flows at the MSOA level. For more info on census flow data, see the [ONS documentation](https://www.ons.gov.uk/census/aboutcensus/censusproducts/origindestinationflowdata)
#' See data-raw/flows_leeds.R for how this data was created.
#' 
#' @format An object of class \code{\link[sf]{sf}} with LINESTRING geometry. It has the following columns:
#' \describe{
#'    \item{origin}{MSOA code of origin zone}
#'    \item{destination}{MSOA code of destination zone}
#'    \item{count}{number of people moving from origin to destination}
#'    \item{geometry}{desire line between origin and destination}
#'  }
#' @source \url{https://www.nomisweb.co.uk/sources/census_2021_od}
"flows_leeds"
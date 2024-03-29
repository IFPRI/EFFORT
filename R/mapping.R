#' Function to return mapping for IMPACT results
#'
#' @param type which mapping to return (j or c or cty to region)
#'
#' @importFrom openxlsx2 read_xlsx
#' @return Mapping set (activity, commodity etc,)
#' @export
#'
#' @examples
#' \dontrun{
#' mapping(type = "j")
#' }
#' @author Abhijeet Mishra

mapping <- function(type = "j") {
  if (type == "j") {
    ## J sets ----
    out <- read_xlsx(system.file("extdata", "ef_checklist.xlsx",
                                 package = "EFFORT"),
                     sheet = "impact_map_fao")
    names(out)[1] <- "itemcode" # Set same as what FAO calls it
    out <- out[!is.na(out$j), ]
  }

  if (type == "cty") {
    ## C sets ----
    out <- read_xlsx(system.file("extdata", "ef_checklist.xlsx",
                                 package = "EFFORT"),
                     sheet = "cty")
  }

  return(out)
}

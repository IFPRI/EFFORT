#' Cleanup fao data
#'
#' @param f Name of file downloaded from FAOSTAT
#' @import dplyr janitor
#' @importFrom utils read.csv
#' @return Cleaned FAO data
#' @export
#'
#' @examples
#' \dontrun{
#' clean_fao(f = "Emissions_crops_E_All_Data_(Normalized).csv")
#' }
#' @author Abhijeet Mishra

clean_fao <- function(f) {
  message("reading ", f)
  df <- read.csv(file = f)
  names(df) <- gsub("\\.", replacement = "", x = names(df))
  message("bit more cleanup ....")
  if ("Source" %in% names(df)) df <- df[df$Source == "FAO TIER 1", ]
  df <- df[, c("AreaCode", "Area", "ItemCode", "Item", "ElementCode",
               "Element", "Year", "Unit", "Value")]
  df <- df[df$Year >= 2015, ]
  names(df) <- tolower(names(df))
  df <- df %>% mutate(across(where(is.integer), as.factor))
  df$element <- as.factor(df$element)
  message("Checking duplicates ....")
  if (nrow(df %>% get_dupes(!"value")) != 0) stop("Duplicates detected")
  return(df)
}

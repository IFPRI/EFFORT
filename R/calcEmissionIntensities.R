#' Calculate Emission Intensities
#'
#' @param source_folder Folder where FAOSTAT data (unzipped) is saved
#' @importFrom Hmisc `%nin%`
#' @importFrom collapse join
#' @importFrom tidyr drop_na
#' @importFrom openxlsx2 read_xlsx
#' @importFrom utils globalVariables
#' @importFrom utils head
#' @return Emission intensities for IMPACT activities
#' @export
#'
#' @examples
#' \dontrun{
#' calcEmissionIntensities(source_folder)
#' }
#' @author Abhijeet Mishra

calcEmissionIntensities <- function(source_folder = NULL) {

  if (is.null(source_folder)) stop("No source folder provided")

  j <- cty <- unit <- year <- value <- element <- head <- NULL

  # Crops emissions (these are "farm gate" in FAO) ----
  crops <- readEmissions(indicator = "crops",
                         source_folder = source_folder)

  # Livestock emissions (these are "rearing" emissions in FAO) ----
  livestock <- readEmissions(indicator = "livestock",
                             source_folder = source_folder)

  # FAOSTAT production  ----
  production <- readProduction(source_folder = source_folder)

  ## Mapping sets ----

  ### Pull activity mapping ----
  j_set <- mapping(type = "j")

  ### Pull country mapping ----
  cty_set <- mapping(type = "cty")

  ## Check which production numbers exist according to impact mapping ----
  for (i in unique(j_set$itemcode)) {
    logic <- i %nin% sort(unique(production$itemcode))
    if (logic) cat("Itemcode not found in production data from FAO: ")
    if (logic) cat(i, "---", i %in% sort(unique(production$itemcode)), "\n")
  }

  # Production from FAO on IMPACT activity level ----
  production_fao_j <- join(production, j_set, on = "itemcode")

  production_fao_j <- join(production_fao_j, cty_set, on = "areacode")

  production_fao_j <- production_fao_j %>% drop_na(j, cty)

  production_fao_j <- production_fao_j %>%
    group_by(cty, j, unit, year) %>%
    summarise(value = sum(value, na.rm = TRUE))

  production_fao_j$value[production_fao_j$unit == "t"] <-
    production_fao_j$value[production_fao_j$unit == "t"] / 1000

  production_fao_j$unit[production_fao_j$unit == "t"] <- "000 t"

  # Emission from Crops on IMPACT activity level ----
  emis_crop_fao_j <- join(crops, j_set, on = "itemcode")

  emis_crop_fao_j <- join(emis_crop_fao_j, cty_set, on = "areacode")

  emis_crop_fao_j <- emis_crop_fao_j %>% drop_na(j, cty)

  emis_crop_fao_j <- emis_crop_fao_j %>%
    group_by(cty, j, unit, element, year) %>%
    summarise(value = sum(value, na.rm = TRUE))

  ## Check global numbers ----
  cat("Global FAO crops emissions when converted to IMPACT activities... \n")
  print(head(emis_crop_fao_j %>%
               group_by(unit, element, year) %>%
               summarise(value = sum(value)) %>%
               arrange(year), 2))

  cat("Global FAO crops emissions ... \n")
  print(head(crops[crops$item %in% "All Crops"
                   & crops$area == "World"
                   & crops$year == 2015, ], 2))

  # Emission from Livestock on IMPACT activity level ----

  # Some codes for livestock are different, set them aside
  livestock_new_codes <- read_xlsx(system.file("extdata", "ef_checklist.xlsx",
                                               package = "EFFORT"),
                                   sheet = "lvstremap")
  # Do remapping
  livestock_remapped <- join(livestock,
                             livestock_new_codes,
                             on = "itemcode",
                             multiple = TRUE)

  livestock_remapped <-
    livestock_remapped[!is.na(livestock_remapped$itemcode_new), ]

  livestock_remapped <-
    livestock_remapped[, !names(livestock_remapped) %in%
                         c("itemcode", "description", "j")]

  names(livestock_remapped)[ncol(livestock_remapped)] <- "itemcode"

  livestock_remapped <- livestock_remapped[, names(livestock)]

  emis_livestock_fao_j <- join(livestock_remapped, j_set, on = "itemcode")

  emis_livestock_fao_j <- join(emis_livestock_fao_j, cty_set, on = "areacode")

  emis_livestock_fao_j <- emis_livestock_fao_j %>% drop_na(j, cty)

  emis_livestock_fao_j <- emis_livestock_fao_j %>%
    group_by(cty, j, unit, element, year) %>%
    summarise(value = sum(value, na.rm = TRUE))

  ## Check global numbers ----
  # Our recalculation numbers are higher because there might be miscounting of
  # emissions in different "items"
  # This is because FAO does not report emissions on all items which exist in
  # production data from FAO + What matches IMPACT activities
  cat("Global FAO livestock emissions when converted to IMPACT activities... \n")
  print(head(emis_livestock_fao_j %>%
               group_by(unit, element, year) %>%
               summarise(value = sum(value)) %>%
               arrange(year), 2))

  cat("Global FAO livestock emissions ... \n")
  print(head(livestock[livestock$item %in% "All Animals"
                       & livestock$area == "World"
                       & livestock$year == 2015, ], 2))

  # Start generating Emission intensities ----

  ## Emission factors for Crops ----

  ef_crops <- join(emis_crop_fao_j,
                   production_fao_j,
                   on = c("cty", "j", "year"),
                   suffix = c(".emission", ".production"))

  ef_crops$value.EI <- ef_crops$value.emission / ef_crops$value.production

  ef_crops$unit.EI <-
    paste(ef_crops$unit.emission,
          ef_crops$element,
          "/",
          ef_crops$unit.production,
          sep = " ")

  ef_crops <-
    ef_crops[, !names(ef_crops) %in% c("unit.emission", "unit.production")]

  ef_crops <- ef_crops[!is.na(ef_crops$value.production), ]

  ef_crops <- ef_crops[ef_crops$value.production > 0, ]

  if (any(is.na(range(ef_crops$value.EI)))) stop(
    "NAs detected in emission factor calculations for crops.")

  ## Emission factors for livestock ----

  ef_livestock <- join(emis_livestock_fao_j,
                       production_fao_j,
                       on = c("cty", "j", "year"),
                       suffix = c(".emission", ".production"))

  ef_livestock$value.EI <- ef_livestock$value.emission / ef_livestock$value.production

  ef_livestock$unit.EI <- paste(ef_livestock$unit.emission,
                                ef_livestock$element,
                                "/",
                                ef_livestock$unit.production,
                                sep = " ")

  ef_livestock <-
    ef_livestock[, !names(ef_livestock) %in% c("unit.emission", "unit.production")]

  ef_livestock <- ef_livestock[!is.na(ef_livestock$value.production), ]

  ef_livestock <- ef_livestock[ef_livestock$value.production > 0, ]

  if (any(is.na(range(ef_livestock$value.EI)))) stop(
    "NAs detected in emission factor calculations for livestock.")

  # Generate "FAO" Emission intensities on IMPACT level ----
  ef_db <- rbind(ef_crops, ef_livestock)
  names(ef_db)[names(ef_db) %in% "year"] <- "yrs"

  # Bugfix for India
  ef_db$value.EI[ef_db$j == "jbeef" & ef_db$cty == "IND"] <-
    ef_db$value.EI[ef_db$j == "jbeef" & ef_db$cty == "IND"] * 1.5

  return(ef_db)
}

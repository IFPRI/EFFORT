#' Calculate emissions from an IMPACT run
#'
#' @param gdx path to an IMPACT run GDX
#' @param source_folder Folder where FAOSTAT data (unzipped) is saved
#' @param ef_db name of the object in environment which contains the output of
#' calcEmissionIntensities(). If this is not provided the function
#' calcEmissionIntensities() will be called. This is time consuming process and
#' it is adviced that you run calcEmissionIntensities() before running this
#' function and feed the output of that function in "ef_db" parameter of this
#' function.
#' @param efficiency_improvement Improvement in Emission intensities annually
#' (defaults to 5 percent i.e., 0.05)
#'
#' @importFrom Hmisc `%nin%`
#' @importFrom collapse join
#' @importFrom tidyr drop_na
#' @importFrom openxlsx2 read_xlsx
#' @importFrom DOORMAT readGDX
#' @importFrom stats median
#'
#' @return Emissions from an IMPACT run
#' @export
#'
#' @examples
#' \dontrun{
#' calcImpactEmissions()
#' }
#' @author Abhijeet Mishra

calcImpactEmissions <- function(gdx = NULL,
                                source_folder = NULL,
                                ef_db = NULL,
                                efficiency_improvement = 0.05) {

  if (is.null(gdx)) stop("No GDX file provided from an IMPACT run.")

  if (is.null(source_folder)) stop("No source_folder provided for FAO data.")

  j <- element <- unit.EI <- value.EI <- NULL
  cty <- description <- model <- NULL

  if (is.null(ef_db)) ef_db <- calcEmissionIntensities(source_folder = source_folder)

  # Prepare for missing / unknown values ----

  ## Find Global medians ----

  cat("Calculating median emission intensities for IMPACT activities by GHG \n")

  global_ef_db <- ef_db %>%
    group_by(j, element, unit.EI) %>%
    summarise(value.EI = median(value.EI, na.rm = TRUE))

  global_ef_db_dummy <- global_ef_db

  colnames(global_ef_db_dummy)[1] <- "jnew"

  ## Pull production data from IMPACT run ----
  impact_production <- readGDX(gdx = gdx, name = "QSX0")$data

  ## Merge emission intensities with IMPACT production data ----
  df <- join(impact_production, ef_db, multiple = TRUE)

  ## Fill missing Emission intensities / factors ----

  ### Find which new mapping we need for missing activities ----
  ef_remap <- read_xlsx(system.file("extdata", "ef_checklist.xlsx",
                                    package = "EFFORT"),
                        sheet = "efremap")

  ### Fill missing values ----

  mod_list  <- list()

  for (i in unique(df$cty)) {

    cat(i, "\n")

    temp <- df[df$cty == i, ]

    clean <- temp[!is.na(temp$value.EI), ]

    dirty <- temp[is.na(temp$value.EI), ]

    # Check if j in dirty data already exists in clean data i.e.,
    # if numbers already exist for historical period

    separate_dirty <- dirty[dirty$j %in% unique(clean$j), ]

    dirty <- dirty[dirty$j %nin% unique(clean$j), ]

    for (yrs in unique(separate_dirty$yrs)) {

      temp2 <-  clean %>%
        group_by(j, cty, description, model, element) %>%
        filter(yrs %in% max(as.numeric(as.character(clean$yrs))))

      temp2$yrs <- yrs

      # Reduce emissions in future annually (within year loop)
      temp2$value.EI <- temp2$value.EI * (1 - efficiency_improvement)

      x1 <- temp2[, !names(temp2) %in% c("value",
                                       "value.emission",
                                       "value.production")]

      x2 <- separate_dirty[separate_dirty$yrs == yrs,
                           colSums(is.na(separate_dirty)) < nrow(separate_dirty)]

      temp2 <- join(x2,
                    x1,
                    on = c("j", "cty", "yrs", "description", "model"),
                    multiple = TRUE)

      clean <- dplyr::bind_rows(clean, temp2)
    }

    dirty2 <- join(dirty, ef_remap)

    dirty2 <- join(dirty2,
                   global_ef_db_dummy,
                   multiple = TRUE,
                   on = c("jnew"),
                   suffix = c(".dirty", ".global"))

    dirty2 <- dirty2[!is.na(dirty2$jnew), ]

    dropnames <- grep(pattern = "dirty", x = names(dirty2), value = TRUE)

    dirty2 <- dirty2[, names(dirty2) %nin% dropnames]

    names(dirty2) <- gsub(pattern = "\\.global",
                          replacement = "",
                          x = names(dirty2))

    dirty2 <- dirty2[, names(dirty2) %in% names(dirty)]

    mod_list[[i]] <- rbind(clean, dirty2)
  }

  ### Merge in one dataset
  df_filled <- bind_rows(mod_list)

  df_filled <- df_filled[df_filled$unit.EI %nin% grep(pattern = "1000 No",
                                                      x = unique(df_filled$unit.EI),
                                                      value = TRUE), ]

  df_filled <- df_filled[!is.na(df_filled$value.EI), ]

  # Calculate emissions ----
  emissions_impact <-
    df_filled[, names(df_filled) %nin% c("description",
                                        "model",
                                        "value.emission",
                                        "value.production")]

  emissions_impact$emission <-
    emissions_impact$value * emissions_impact$value.EI

  emissions_impact$unit.emission <- gsub(pattern = "\\/ 000 t",
                                         replacement = "",
                                         x = emissions_impact$unit.EI)

  return(emissions_impact)

}

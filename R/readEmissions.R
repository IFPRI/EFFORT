#' Read Emission data from FAO
#'
#' @param indicator Options are "crop" emissions (reads file
#' "Emissions_crops_E_All_Data_(Normalized).csv"), "livestock" emissions (reads
#' file "Emissions_livestock_E_All_Data_(Normalized).csv")), and "total" emissions
#' (reads file "Emissions_Totals_E_All_Data_(Normalized).csv").
#' These files are all part of the bulk download from FAOSTAT.
#' @param source_folder Folder where FAOSTAT data (unzipped) is saved
#'
#' @return Emissions from FAO
#' @export
#'
#' @examples
#' \dontrun{
#' readEmissions(indicator = "crops")
#' }
#' @author Abhijeet Mishra

readEmissions <- function(indicator = "crops",
                          source_folder = NULL) {

  if (is.null(source_folder)) stop("No source folder provided")

  # Emissions from Crops ----

  # The FAOSTAT domain Emissions from Crops provides estimates of emissions
  # associated with crop processes, namely Crop residues, Burning of crop residues,
  # and Rice cultivation and the application of nitrogen (N) fertilizers, including
  # mineral and chemical fertilizers, to soils. Estimates are computed at Tier 1 following
  # the 2006 IPCC Guidelines for National greenhouse gas (GHG) Inventories (IPCC, 2006).

  if (indicator == "crops") {
    src <- "Emissions_crops_E_All_Data_(Normalized).csv"
    out <- clean_fao(f = paste0(source_folder, "/", src))
    elements <- grep(pattern = "total",
                     x = unique(out$element),
                     ignore.case = TRUE,
                     value = TRUE)
    out <- droplevels(out[out$element %in% elements, ])
    levels(out$element)
    out$element <- factor(out$element, labels = c("CH4", "N2O"))
  }

  # Emissions from livestock ----

  # The FAOSTAT domain Emissions from Livestock contains the greenhouse gas (GHG) emissions
  # originating from livestock rearing. The domain disseminates total methane (CH4) and
  # nitrous oxide (N2O) emissions originating from livestock-related processes.
  # Detailed emissions are also disseminated from
  # 1) Enteric fermentation: the CH4 emissions produced from enteric fermentation processes
  # in the digestive systems of ruminants and to a lesser extent of non-ruminants;
  # 2) Manure left on pasture: the N2O emissions originated from the nitrogen in manure
  # left by grazing livestock on pasture;
  # 3) Manure management: the CH4 and N2O emissions originating from aerobic and anaerobic
  # processes of manure decomposition;
  # 4) Manure applied to soils: the N2O emissions originated from soils applications of manure.
  #
  # The FAOSTAT emissions database is computed following Tier 1 IPCC 2006 Guidelines for
  # National GHG Inventories vol. 4, ch. 10 and 11
  # (http://www.ipcc-nggip.iges.or.jp/public/2006gl/vol4.html).
  # GHG emissions are provided by country, regions and special groups, with global coverage,
  # relative to the period 1961-present (with annual updates) and with projections
  # for 2030 and 2050, expressed in units of kilotonnes of gas by livestock species
  # (asses, buffaloes, camels, cattle (dairy and non-dairy), goats, horses, llamas, mules,
  # sheep, swine (breeding and market)) and relevant species aggregates (all animals, camels
  # and llamas, cattle, mules and asses, sheep and goats, swine).
  #
  # This FAOSTAT domain also disseminates the activity data and emissions reported by
  # countries to the United Nations Framework Convention on Climate Change (UNFCCC).
  # Emission data are sourced directly from the UNFCCC data portal or from Biennial Update Reports (BURs).
  # UNFCCC data are disseminated in FAOSTAT with permission, formalized via a FAO-UNFCCC MoU.
  # The IPCC (2019) Guidelines indicate the FAOSTAT database as a useful tool for
  # NGHGI QA/QC processes and validation of both activity data and emissions estimates.

  if (indicator == "livestock") {
    src <- "Emissions_livestock_E_All_Data_(Normalized).csv"
    out <- clean_fao(f = paste0(source_folder, "/", src))
    elements <- grep(pattern = "total",
                     x = unique(out$element),
                     ignore.case = TRUE,
                     value = TRUE)
    out <- droplevels(out[out$element %in% elements, ])
    levels(out$element)
    out$element <- factor(out$element, labels = c("CH4", "N2O"))
  }

  # Emissions totals ----

  # The FAOSTAT domain Emissions Totals summarizes the greenhouse gas (GHG) emissions
  # generated from agrifood systems and that are disseminated in the FAOSTAT Climate
  # Change Emissions domains. Data are computed following the Tier 1 methods of the
  # Intergovernmental Panel on Climate Change (IPCC) Guidelines for National greenhouse
  # gas (GHG) Inventories(IPCC, 1996; 1997; 2000; 2002; 2006; 2014). Emissions from other
  # economic sectors as defined by the IPCC are also disseminated in the domain for completeness.
  #
  # The domain includes methane (CH4), nitrous oxide (N2O) and carbon dioxide (CO2)
  # emissions from all the above activities as well as the aggregate fluorinated gases
  # (F-gases) emissions used in industrial processes. Estimates are available by country,
  # with global coverage for the period 1961–2020 with projections for 2030 and 2050 for
  # some categories of emissions or 1990–2020 for others. The database is updated annually
  # The FAOSTAT domain Emissions Totals disseminates information estimates for the single
  # gases and their aggregates in CO2eq in units of kilotonnes (kt or 10*6 kg). The latter
  # are computed by using the IPCC Fifth Assessment report global warming potentials, AR5 (IPCC, 2014).
  # Emissions from agrifood systems are those generated within farm gate, those associated
  # with the land use change and the emissions from pre- and post-production food processes.
  # The latter emissions are calculated based on data from the UN Statistical Division (UNSD),
  # the International Energy Agency (IEA) and other third-party as well as by integrating emission
  # information from the PRIMAP-hist dataset v2.4 (Gütschow et al., 2022). Methodologies for
  # these estimates are described in dedicated working papers as follows: I) food transport; II)
  # food systems waste disposal and III) fertilizers manufacturing, food processing, retail,
  # packaging and household consumption.
  # It should be noted that, the world aggregate estimates of food transport also includes
  # international bunkers related to food. Emissions from “International Bunkers” is derived
  # from data on ‘International aviation’ and ‘International navigation/shipping’ of the
  # EDGARv6.0 dataset (JRC/PBL, 2019), covering the period 1990–2018 and extrapolated linearly
  # to 2020 by FAOSTAT Domain Emissions Totals. Emissions for this category are only available
  # for the world aggregate. PRIMAP data for other IPCC sectors is also disseminated for
  # completeness and in view of computing shares of emissions for the whole economy
  # (these are disseminate separately in the FAOSTAT domain Emissions shares).
  # The IPCC economic sectors are: energy, industrial processes and product use (IPPU), waste and other n.e.c.
  # These data are sourced from the PRIMAP-hist v2.4 dataset (Gütschow et al., 2022).
  #
  # Emissions Totals jointly disseminates the emissions reported by countries to the
  # United Nations Framework Convention on Climate Change (UNFCCC). Emission data are
  # sourced directly from the UNFCCC data portal as submitted by countries through their
  # most recent GHG National Inventories (NGHGI) or are extracted from Biennial Update Reports (BURs).
  # UNFCCC data are disseminated in FAOSTAT with permission, formalized via a FAO-UNFCCC Memorandum of Understanding.
  # Values for Annex I and non-Annex I groups are computed from the UNFCCC data for
  # the single categories of emissions listed in Table 1 and for IPCC Agriculture aggregate.
  # It should be noted that due to incomplete reporting, significant data gaps characterize the non-Annex I aggregates.

  if (indicator == "total") {
    src <- "Emissions_Totals_E_All_Data_(Normalized).csv"
    out <- clean_fao(f = paste0(source_folder, "/", src))
    elements <- grep(pattern = "AR5|Indirect|Direct",
                     x = unique(out$element),
                     ignore.case = TRUE,
                     value = TRUE,
                     invert = TRUE)
    out <- droplevels(out[out$element %in% elements, ])
    levels(out$element)
    out$element <- factor(out$element, labels = c("CH4", "CO2", "N2O"))
  }

  # Return data
  return(out)
}

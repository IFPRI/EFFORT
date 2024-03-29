#' Read Production data from FAO
#'
#' @param source_folder Folder where FAOSTAT data (unzipped) is saved
#'
#' @return Emissions from FAO
#' @export
#'
#' @examples
#' \dontrun{
#' readProduction(indicator = "crops")
#' }
#' @author Abhijeet Mishra

readProduction <- function(source_folder = NULL) {

  if (is.null(source_folder)) stop("No source folder provided")

  # Crop and livestock statistics are recorded for 278 products,
  # covering the following categories:
  #
  #   1) CROPS PRIMARY: Cereals, Citrus Fruit, Fibre Crops, Fruit,
  # Oil Crops, Oil Crops and Cakes in Oil Equivalent, Pulses,
  # Roots and Tubers, Sugar Crops, Treenuts and Vegetables.
  #
  # Data are expressed in terms of area harvested, production quantity and yield.
  #
  # Cereals: Area and production data on cereals relate to crops harvested for
  # dry grain only. Cereal crops harvested for hay or harvested green for food,
  # feed or silage or used for grazing are therefore excluded.
  #
  # 2) CROPS PROCESSED: Beer of barley; Cotton lint; Cottonseed; Margarine, short;
  # Molasses; Oil, coconut (copra); Oil, cottonseed; Oil, groundnut; Oil, linseed;
  # Oil, maize; Oil, olive, virgin; Oil, palm; Oil, palm kernel; Oil, rapeseed;
  # Oil, safflower; Oil, sesame; Oil, soybean; Oil, sunflower; Palm kernels;
  # Sugar Raw Centrifugal; Wine.
  #
  # 3) LIVE ANIMALS: Animals live n.e.s.; Asses; Beehives; Buffaloes;
  # Camelids, other; Camels; Cattle; Chickens; Ducks; Geese and guinea fowls;
  # Goats; Horses; Mules; Pigeons, other birds; Pigs; Rabbits and hares;
  # Rodents, other; Sheep; Turkeys.
  #
  # 4) LIVESTOCK PRIMARY: Beeswax; Eggs (various types); Hides buffalo, fresh;
  # Hides, cattle, fresh; Honey, natural;
  # Meat (ass, bird nes, buffalo, camel, cattle, chicken, duck, game, goat,
  #       goose and guinea fowl, horse, mule, Meat nes, meat other camelids,
  #       Meat other rodents, pig, rabbit, sheep, turkey);
  # Milk (buffalo, camel, cow, goat, sheep);
  # Offals, nes; Silk-worm cocoons, reelable;
  # Skins (goat, sheep); Snails, not sea; Wool, greasy.
  #
  # 5) LIVESTOCK PROCESSED:
  #   Butter (of milk from sheep, goat, buffalo, cow);
  # Cheese (of milk from goat, buffalo, sheep, cow milk);
  # Ghee (cow and buffalo milk);
  # Milk (dry buttermilk, skimmed condensed, skimmed cow, skimmed dried,
  #       skimmed evaporated, whole condensed, whole dried, whole evaporated);
  # Whey (condensed and dry);
  # Cheese of skimmed cow milk; Cream fresh; Lard; Silk raw; Tallow; Yoghurt

  production <- clean_fao(
    f = paste0(source_folder, "/",
               "Production_Crops_Livestock_E_All_Data_(Normalized).csv"))

  elements <- grep(pattern = "Yield|Harvested",
                   x = unique(production$element),
                   ignore.case = TRUE,
                   value = TRUE,
                   invert = TRUE)

  production <- droplevels(production[production$element %in% elements, ])

  elements <- grep(pattern = "Production",
                   x = unique(production$element),
                   ignore.case = TRUE,
                   value = TRUE)

  production <- droplevels(production[production$element %in% elements, ])

  # Rice fix for IMPACT
  production$value[production$item == "Rice"] <-
    production$value[production$item == "Rice"] * 0.66

  # Return data
  return(production)
}

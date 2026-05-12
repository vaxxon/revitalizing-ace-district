# import libraries
library(corrr)
library(ggcorrplot)
library(factoextra)
library(spatstat)
library(tidycensus)
library(tidyr)
library(dplyr)
library(tigris)
library(sf)
library(mapview)
library(RColorBrewer)
library(car)
library(GWmodel)

# set working directory
setwd("/Users/vincentallport/Documents/INFO615/FinalProject")

# import raw address point data
points_raw <- read.csv("AddressPoints.csv")
View(points_raw)

# get unique values for bus type, bus type 2, fac name, and com name
bus_name_list <- sort(unique(points_raw$BUS_NAME))
View(bus_name_list)
bus_type_list <- sort(unique(points_raw$BUS_TYPE))
View(bus_type_list)
bus_type2_list <- sort(unique(points_raw$BUS_TYPE2))
View(bus_type2_list)
fac_name_list <- sort(unique(points_raw$FAC_NAME))
View(fac_name_list)
com_name_list <- sort(unique(points_raw$COM_NAME))
View(com_name_list)

# project in tx state plane
points_raw_sf <- st_as_sf(points_raw, coords = c("x", "y"), crs = 3857)
points_sp <- st_transform(points_raw_sf, crs = 2278)

# filtering for food points
com_name_set <- c("BAR", "CLUB", "CLUBHOUSE", "FOOD", "RESTAURANT")
fac_name_set <- c("BAKERY", "BAR", "BEER", "BURGER", "CLUB HOUSE",
                  "DONUT SHOP", "FOOD", "GROCERIES", "GROCERY",
                  "GROCERY STORE", "ICECREAM", "MCDONALDS", "MEXICAN",
                  "REATAURANT", "RESTAURANT", "RESTAUTANT", "SANDWICH",
                  "SNOW CONE SHOP")
bus_type_set <- c("B-B-Q", "BAKERY", "BAR", "BBQ", "BUFFET", "BURGER",
                  "BURGERS", "Cafeteria", "CHECKERS", "CHICKEN", "CHINESE",
                  "DONUTS", "FOOD TOWN", "GROCERIES", "GROCERY",
                  "ITALION", "MEAT MARKET", "MEXICAN", "PIZZA",
                  "RESRAURANT", "RESTARAUNT", "RESTAURANT", "SANDWICH",
                  "SEAFOOD", "SEAFOOD / SEAFOOD MARKET", "SNO CONES",
                  "STEAK", "STEAK HOUSE", "TEX-MEX", "WINGS")
bus_type2_set <- c("DONUT SHOP", "ITALION", "MEXICAN",
                   "SANDWICH SHOP", "SEAFOOD")
bus_name_set <- c(" TACO BELL", "2 CLAWS SEAFOOD", "4 CORNERS B-B-Q", "BAR",
                  "BAYTOWN FOOD MART", "BAYTOWN SANDWICH AND NOODLE HOUSE",
                  "BAYTOWN SEAFOOD", "BOWIE FOOD MART", "BUDS BBQ",
                  "BUFFALO WILD WINGS", "BURGER HUT",
                  "C & D GROCERY AND BAKERY", "CASA OLE", "CHAVEZ GROCERIES",
                  "CHECKERS", "CHEDDARS", "CHICK-FIL-A", "CHICKEN EXPRESS",
                  "CHILI'S GRILL & BAR", "CHIPOLTE", "CHURCH'S CHICKIN",
                  "CICI'S PIZZA", "COLONIAL HOUSE OF SANDWICHES",
                  "CRACKER BARREL OLD COUNTRY STORE", "CREEKS KITCHEN",
                  "CUBAN CAFE", "CUBAN DELI SANDWICH SHOP", "DAIRY QUEEN",
                  "DENNYS", "DOMINO'S PIZZA", "DONALDS DONUTS",
                  "DOUBLE DAVES PIZZA", "DOWN ON MAIN ST BAR",
                  "ECONO LODGE - SHADYS LOUNGE - RESTAURANT",
                  "EL CACIQUE TAQUERIA", "El Sinaloense Restaurant",
                  "EL TORO MEXICAN RESTAURANT", "ELTORO MEXICN", "FAIRWAY",
                  "FIREHOUSE SUBS", "FOOD TOWN", "FOODTOWN",
                  "FREDDYS FROZEN CUSTARD AND STEAKBURGERS", "GOINGS BBQ",
                  "HEB", "HOOTERS", "ITALION", "J&J ASIAN CAFE",
                  "JACK IN THE BOX", "JACK-N-THE-BOX",
                  "JIMMY JOHNS SANDWICHES", "JoeV's",
                  "JUNIORS SMOKE HOUSE", "KFC", "KROGER",
                  "LA MICHOACANA MEAT MARKET", "LANI'S DONUTS",
                  "LITTLE CEASARS", "LOS TORITOS TAMALES", "LUBY'S",
                  "MAMBO SEAFOOD", "MARY'S CAKES AND COOKIES", "MEXICAN",
                  "MOD PIZZA", "NARA THAI DINING", "O NEALS", "OASIS",
                  "OLIVE GARDEN", "OUTBACK STEAK HOUSE", "PANADERIA",
                  "PANDA EXPRESS", "PANERA BREAD", "PIZZA BELLA", "PIZZA HUT",
                  "POPEYES CHICKEN", "R K GROCERIES", "RAISING CANES # 6",
                  "RED LOBSTER", "RESTAURANT", "ROOSTER BBQ", "SALATA SALADS",
                  "SALTGRASS STEAK HOUSE", "SCHLOTSKY'S", "SHIPLEY DONUTS",
                  "SHIPLEYS DONUTS", "SNACK TIME FOOD MART", "SNO CONES",
                  "SNOWFLAKE DONUTS", "SOME BURGER", "SONIC DRIVE IN",
                  "STARBUCKS COFFEE", "SUBWAY", "TACO BELL", "TACO CABANA",
                  "TAQUERIA", "TAQUERIA RESTAURANT EL SOL DE MEXICO",
                  "TEXAS RIOAD HOUSE", "THE CAKE BOX",
                  "THE DIRTY BAY BEER COMPANY", "WAFFLE HOUSE", "WATTABURGER",
                  "WENDY'S BURGERS", "WEST MAIN FOOD STORE",
                  "WESTCOAST DONUTS", "WHATABURGER", "WHATAPOLLO",
                  "WOK D'LITE")
food_points <- points_raw %>%
  filter(FAC_NAME %in% fac_name_set | COM_NAME %in% com_name_set | 
           BUS_TYPE %in% bus_type_set | BUS_TYPE2 %in% bus_type2_set |
           BUS_NAME %in% bus_name_set)
View(food_points)
write.csv(food_points, "food_points_raw.csv", row.names = FALSE)

# did more filtering in Excel since there are 200 points
food_points_clean <- read.csv("food_points_clean.csv")
food_points_clean_sf <- st_as_sf(food_points_clean,
                                 coords = c("x", "y"), crs = 3857)
food_points_sp <- st_transform(food_points_clean_sf, crs = 2278)

# filtering for residential points
res_name_set <- c("APARTMENT", "APARTMENT COMPLEX", "APARTMENTS",
                  "ASSISTED LIVING", "HOUSE", "INDEPENDENT LIVING",
                  "RESIDENTIAL", "RESIDENTIAL MF3",
                  "RESIDENTIAL SF")
res_points <- points_sp %>%
  filter(COM_NAME %in% res_name_set)

# set census api key
census_api_key("d4d65f2cfac8c11446f841aca5cd624b87679252")

# load boundary data for baytown
baytown_boundary <- places(state = "TX", cb = TRUE) %>%
  filter(NAME == "Baytown")
baytown_boundary <- st_transform(baytown_boundary, crs = 2278)
mapview(baytown_boundary)

map_boundary <- mapview(baytown_boundary,
                        alpha.regions = 0.2,
                        col.regions = "lightpink",
                        color = "black",
                        lwd = 1,
                        layer.name = "Baytown Limit")

map_boundary

num_types <- length(unique(food_points_clean$REST_TYPE))
my_colors <- colorRampPalette(brewer.pal(9, "Set1"))(num_types)

map_points <- mapview(food_points_sp,
                      cex = 3,
                      zcol = "REST_TYPE",
                      col.regions = my_colors,
                      alpha.regions = 1,
                      layer.name = "Restaurants")

map_boundary + map_points

# get census data for baytown's census tracts
variables <- load_variables(2023, "acs5", cache = TRUE)
View(variables)

## list of vars
varc <- c(total_pop = "B01003_001", # total population
          med_hh_inc = "B19013_001", # median household income
          total_occupied = "B25002_001", # total occupied housing units
          owner_occupied = "B25002_002", # owner occupied housing units
          renter_occupied = "B25002_003", # renter occupied housing units
          total_hh = "B11005_001", # total households
          hh_children = "B11005_002") # households w/ children under 18

baytown_bg <- get_acs(
  geography = "block group",
  variables = varc,
  state = "TX",
  county = c("Harris County", "Chambers County"),
  year = 2022,
  geometry = TRUE,
  output = "wide",
  cb = TRUE
) %>%
  st_transform(baytown_bg, crs = 2278) %>%
  st_filter(baytown_boundary) %>%
  mutate(
    pct_renter = (renter_occupiedE / total_occupiedE) * 100,
    pct_owner  = (owner_occupiedE / total_occupiedE) * 100,
    pct_hh_children = (hh_childrenE / total_hhE) * 100
  )

## visualize % households with children
mapview(baytown_bg, zcol = "pct_hh_children",
        col.regions = brewer.pal(9, "YlGn"),
        layer.name = "% Households with Children")
View(baytown_bg)

## export to GeoPackage for GeoDa
st_write(baytown_bg, "baytown_data.gpkg", delete_dsn = TRUE)

# join count per cuisine to each block group
## temp df to create wide counts per cuisine
bg_cuisine_counts <- baytown_bg %>%
  st_join(food_points_sp) %>%
  st_drop_geometry() %>%
  # Count how many of each REST_TYPE are in each block group (GEOID)
  count(GEOID, REST_TYPE) %>%
  # Remove NAs
  filter(!is.na(REST_TYPE)) %>%
  tidyr::pivot_wider(names_from = REST_TYPE, values_from = n, values_fill = 0)

## join counts to census data
food_bg_joined <- baytown_bg %>%
  left_join(bg_cuisine_counts, by = "GEOID") %>%
  mutate(across(where(is.numeric), ~tidyr::replace_na(., 0)))
View(food_bg_joined)

# OLS for mexican restaurants
ols_mexican <- lm(mexican ~ med_hh_incE + pct_hh_children +
                    pct_owner, data = food_bg_joined)
summary(ols_mexican)
vif(ols_mexican)
food_bg_joined$mexican_res <- residuals(ols_mexican)
# Deep Red = FEWER restaurants than predicted
mapview(food_bg_joined, zcol = "mexican_res",
        col.regions = brewer.pal(9, "RdBu"))

# OLS for fast food restaurants
ols_fast_food <- lm(fast_food ~ med_hh_incE + pct_hh_children +
                      pct_owner, data = food_bg_joined)
summary(ols_fast_food)
vif(ols_fast_food)
food_bg_joined$fast_food_res <- residuals(ols_fast_food)
mapview(food_bg_joined, zcol = "fast_food_res",
        col.regions = brewer.pal(9, "RdBu"))

# OLS for grocery stores
ols_grocery <- lm(grocery_store ~ med_hh_incE + pct_hh_children +
                    pct_owner, data = food_bg_joined)
summary(ols_grocery)
vif(ols_grocery)
food_bg_joined$grocery_store_res <- residuals(ols_grocery)
mapview(food_bg_joined, zcol = "grocery_store_res",
        col.regions = brewer.pal(9, "RdBu"))

# OLS for bakeries
ols_bakery <- lm(bakery ~ med_hh_incE + pct_hh_children +
                    pct_owner, data = food_bg_joined)
summary(ols_bakery)
vif(ols_bakery)
food_bg_joined$bakery_res <- residuals(ols_bakery)
mapview(food_bg_joined, zcol = "bakery_res",
        col.regions = brewer.pal(9, "RdBu"))

# OLS for sandwich shops
ols_sandwich <- lm(sandwich ~ med_hh_incE + pct_hh_children +
                    pct_owner, data = food_bg_joined)
summary(ols_sandwich)
vif(ols_sandwich)
food_bg_joined$sandwich_res <- residuals(ols_sandwich)

# OLS for bars
ols_bar <- lm(bar ~ med_hh_incE + pct_hh_children +
                    pct_owner, data = food_bg_joined)
summary(ols_bar)
vif(ols_bar)
food_bg_joined$bar_res <- residuals(ols_bar)

# OLS for pizza shops
ols_pizza <- lm(pizza ~ med_hh_incE + pct_hh_children +
                    pct_owner, data = food_bg_joined)
summary(ols_pizza)
vif(ols_pizza)
food_bg_joined$pizza_res <- residuals(ols_pizza)

# OLS for american restaurants
ols_american <- lm(american ~ med_hh_incE + pct_hh_children +
                    pct_owner, data = food_bg_joined)
summary(ols_american)
vif(ols_american)
food_bg_joined$american_res <- residuals(ols_american)

# OLS for bbq shops
ols_bbq <- lm(bbq ~ med_hh_incE + pct_hh_children +
                    pct_owner, data = food_bg_joined)
summary(ols_bbq)
vif(ols_bbq)
food_bg_joined$bbq_res <- residuals(ols_bbq)

# OLS for seafood shops
ols_seafood <- lm(seafood ~ med_hh_incE + pct_hh_children +
                    pct_owner, data = food_bg_joined)
summary(ols_seafood)
vif(ols_seafood)
food_bg_joined$seafood_res <- residuals(ols_seafood)

# PCA
pca_data <- food_bg_joined %>%
  st_drop_geometry() %>%
  select(med_hh_incE, pct_hh_children, pct_owner)

pca_results <- prcomp(pca_data, center = TRUE, scale. = TRUE)

## loadings
pca_results$rotation

## add PC1 to joined data and visualize
food_bg_joined$target_score <- pca_results$x[, 1]
mapview(food_bg_joined, zcol = "target_score",
        col.regions = brewer.pal(9, "RdBu"))

# export to CSV to resume here
write.csv(food_bg_joined, file = "premgwr.csv", row.names = FALSE)
food_bg_joined <- read.csv("premgwr.csv") %>%
  st_as_sf(coords = c("geometry.1", "geometry.2"), crs = 2278)

# run MGWR
## convert to Spatial object for MGWR
food_sp <- as(food_bg_joined, "Spatial")

mgwr_mexican <- gwr.multiscale(
  formula = mexican ~ target_score, 
  data = food_sp, 
  criterion = "AICC", 
  adaptive = TRUE, 
  kernel = "bisquare"
)
print(mgwr_mexican)
# add residual values to the joined data
food_bg_joined$mex_gap <- mgwr_mexican$SDF$residual
# pull the local coefficients (the demand strength)
# This shows how much 'target_score' actually matters in each specific block group
food_bg_joined$mex_beta <- mgwr_mexican$SDF$target_score
mapview(food_bg_joined, zcol = "mex_gap",
        col.regions = brewer.pal(9, "RdBu"),
        layer.name = "Mexican Restaurant Gap")

mgwr_fast_food <- gwr.multiscale(
  formula = fast_food ~ target_score, 
  data = food_sp, 
  criterion = "AICC", 
  adaptive = TRUE, 
  kernel = "bisquare"
)
food_bg_joined$fast_food_gap <- mgwr_fast_food$SDF$residual
food_bg_joined$fast_food_beta <- mgwr_fast_food$SDF$target_score
print(mgwr_fast_food)

mgwr_grocery_store <- gwr.multiscale(
  formula = grocery_store ~ target_score, 
  data = food_sp, 
  criterion = "AICC", 
  adaptive = TRUE, 
  kernel = "bisquare"
)
food_bg_joined$grocery_store_gap <- mgwr_grocery_store$SDF$residual
food_bg_joined$grocery_store_beta <- mgwr_grocery_store$SDF$target_score
print(mgwr_grocery_store)

mgwr_bakery <- gwr.multiscale(
  formula = bakery ~ target_score, 
  data = food_sp, 
  criterion = "AICC", 
  adaptive = TRUE, 
  kernel = "bisquare"
)
food_bg_joined$bakery_gap <- mgwr_bakery$SDF$residual
food_bg_joined$bakery_beta <- mgwr_bakery$SDF$target_score
print(mgwr_bakery)

mgwr_bar <- gwr.multiscale(
  formula = bar ~ target_score, 
  data = food_sp, 
  criterion = "AICC", 
  adaptive = TRUE, 
  kernel = "bisquare"
)
food_bg_joined$bar_gap <- mgwr_bar$SDF$residual
food_bg_joined$bar_beta <- mgwr_bar$SDF$target_score
print(mgwr_bar)

mgwr_sandwich <- gwr.multiscale(
  formula = sandwich ~ target_score, 
  data = food_sp, 
  criterion = "AICC", 
  adaptive = TRUE, 
  kernel = "bisquare"
)
food_bg_joined$sandwich_gap <- mgwr_sandwich$SDF$residual
food_bg_joined$sandwich_beta <- mgwr_sandwich$SDF$target_score
print(mgwr_sandwich)

mgwr_pizza <- gwr.multiscale(
  formula = pizza ~ target_score, 
  data = food_sp, 
  criterion = "AICC", 
  adaptive = TRUE, 
  kernel = "bisquare"
)
food_bg_joined$pizza_gap <- mgwr_pizza$SDF$residual
food_bg_joined$pizza_beta <- mgwr_pizza$SDF$target_score
print(mgwr_pizza)

mgwr_american <- gwr.multiscale(
  formula = american ~ target_score, 
  data = food_sp, 
  criterion = "AICC", 
  adaptive = TRUE, 
  kernel = "bisquare"
)
food_bg_joined$american_gap <- mgwr_american$SDF$residual
food_bg_joined$american_beta <- mgwr_american$SDF$target_score
print(mgwr_american)

mgwr_bbq <- gwr.multiscale(
  formula = bbq ~ target_score, 
  data = food_sp, 
  criterion = "AICC", 
  adaptive = TRUE, 
  kernel = "bisquare"
)
food_bg_joined$bbq_gap <- mgwr_bbq$SDF$residual
food_bg_joined$bbq_beta <- mgwr_bbq$SDF$target_score
print(mgwr_bbq)

mgwr_seafood <- gwr.multiscale(
  formula = seafood ~ target_score, 
  data = food_sp, 
  criterion = "AICC", 
  adaptive = TRUE, 
  kernel = "bisquare"
)
food_bg_joined$seafood_gap <- mgwr_seafood$SDF$residual
food_bg_joined$seafood_beta <- mgwr_seafood$SDF$target_score
print(mgwr_seafood)

mapview(food_bg_joined, zcol = "target_score",
        col.regions = brewer.pal(9, "RdBu"),
        layer.name = "Target Score")

st_write(food_bg_joined, "final_results.gpkg", delete_dsn = TRUE)

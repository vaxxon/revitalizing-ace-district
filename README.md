# Revitalizing the ACE District • Case study

## Overview

**Technologies used:** R (`factoextra`, `tidycensus`, `tidyr`, `dplyr`, `tigris`, `sf`, `mapview`, `car`, `GWmodel`)

A study into what kinds of cuisine Baytown’s ACE District lacks and might want to attract.

## Details

Baytown, Texas is a rapidly growing suburb of Houston economically and socially defined by the oil refineries and chemical plants that line the Houston Ship Channel. In 2016, the City of Baytown began a campaign to revitalize its old downtown neighborhood and rename it the ACE (Arts, Culture, and Entertainment) District.

A decade later, the effort has seen some success: formerly empty storefronts now host clothing boutiques, comic book shops, and a visitor center. However, the area still has quite a few empty retail spaces, and the current leadership would like to fill those spaces with restaurants. They specifically want to entice young families with relatively high median income to the area.

This project attempts to figure out what kind of restaurant the ACE District should try to attract by finding cold spots for the most popular types of cuisine within Baytown.

In June 2021, Baytown published [a map of Address Points](https://baytowngis-baytown-tx.hub.arcgis.com/items/1da454953f1744b38904fcf6e61a7461) within the city. This map includes all address points within the city, including residential addresses, businesses, billboards, and utilities. This data was cleaned to remove all addresses except for restaurants, then manually tagged with a cuisine (`REST_TYPE`), then joined with block group-level demographic data from the Census Bureau to create a target audience score (`target_score`). The residuals from OLS regressions used to test for multicollinearity were also added (the `[category]_res` columns).

Finally, multiscale geographically weighted regression (`GWmodel::gwr.multiscale`) was calculated using a count of each type of restaurant in the block group and its target audience score. The residuals of the MGWR were added to the dataset (the `[cuisine]_gap` columns) along with the $\beta$ of `target_score` (the `[cuisine]_beta` columns). These results were mapped throughout with `mapview`.


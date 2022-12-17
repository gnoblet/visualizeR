
#------ Border - admin 0
border_admin0 <- sf::st_read("data-raw/border_admin0.shp")
usethis::use_data(border_admin0, overwrite = TRUE)

#------ Frontier - admin 0
frontier_admin0 <- sf::st_read("data-raw/frontier_admin0.shp")
usethis::use_data(frontier_admin0, overwrite = TRUE)

#------ Line - admin 1
line_admin1 <- sf::st_read("data-raw/line_admin1.shp")
usethis::use_data(line_admin1, overwrite = TRUE)

#------ Centroid - admin 1
centroid_admin1 <- sf::st_read("data-raw/centroid_admin1.shp") |>
  dplyr::rename(ADM1_FR_UPPER = ADM1_FR_)
usethis::use_data(centroid_admin1, overwrite = TRUE)

#------ Indicator polygon  - admin 1
indicator_admin1 <- sf::st_read("data-raw/indicator_admin1.shp")
usethis::use_data(indicator_admin1, overwrite = TRUE)

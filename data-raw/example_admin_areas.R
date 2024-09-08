#create example admin areas, using voronoi cells around example holding coordinates

#first create a bounding box around the example coordinates, as overall boundary for the map
bbox <-
  example_holding_data$coordinates %>%
  sf::st_transform(3035) %>% #transform to projected coords, because voronoi triangulation doesn't work well with lat/long
  sf::st_bbox() %>%
  sf::st_as_sfc() %>%
  sf::st_as_sf()

#then create voronoi cells (admin areas) around the example holding coordinates
example_admin_areas <-
  example_holding_data$coordinates %>%
  sf::st_as_sf() %>%
  sf::st_transform(3035) %>% #transform to projected coords, because voronoi triangulation doesn't work well with lat/long
  hexscape::discretise_voronoi(bbox, .) %>%
  select(-x, -Area, -centroid) %>%
  sf::st_transform(4258)

#save example admin areas as dataset in package
usethis::use_data(example_admin_areas, overwrite = TRUE)

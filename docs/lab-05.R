palobbwgs = palobb %>% 
  st_transform(4326)

palobb = st_bbox(palobbwgs)

scenes = lsat_scenes()

paloscene = scenes %>% 
  filter(min_lat <= palobb$ymin, max_lat >= palobb$ymax,
         min_lon <= palobb$xmin, max_lon >= palobb$xmax,
         as.Date(acquisitionDate) == as.Date("2016-09-26"))

write.csv(paloscene, file = "data/palo-flood.csv", row.names = FALSE)
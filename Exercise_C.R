library("readr")
library("sf")
library("dplyr")
library(ggplot2)
library(tidyr)
library(tmap)
library(dplyr)
library(purrr)
library(tibble)
library(plotly)


# Runs fast
# FILE <- "datasets/20260312-172538_short.gpx"
# Warning: It takes long to run with this file.
FILE <- "datasets/20260312-172538.gpx"

# FILE <- "datasets/20260425-144523.gpx"
# My original dataset is very large and takes a long time to load.
# That's why I created a second dataset from a bike ride.
# The settings in the GPS tracking app are identical.
# However, it doesn't take as long to load and makes it easier
# to try out different things. 

difftime_secs <- function(x, y){
  as.numeric(difftime(x, y, units = "secs"))
}

distance_by_element <- function(later, now){
  as.numeric(
    st_distance(later, now, by_element = TRUE)
  )
}


st_layers(FILE)

GPS_Track <- st_read(FILE, layer = "track_points")

GPS_Track <- GPS_Track |> 
  st_transform(crs = 2056)

head(GPS_Track)


GPS_Track <- GPS_Track |> 
  arrange(time) |> 
  mutate(
    timediff = difftime_secs(lead(time), time),
    steplength = distance_by_element(lead(geometry), geometry),
    sped_clac = steplength / timediff
  )


# clean up file:

GPS_Track <- GPS_Track |>
  select(where(~ !all(is.na(.))))



# tmap_mode("view")
# tm_shape(GPS_Track) +
#   tm_dots()


GPS_Track_line <- GPS_Track |> 
  summarise(do_union = FALSE) |> 
  st_cast("LINESTRING")

tmap_options(basemap.server = 
               "OpenStreetMap.CH"
               # "Stadia" 
               # "Jawg"
               # "SwissFederalGeoportal"  
               # "SwissFederalGeoportal.SWISSIMAGE"
               # "SwissFederalGeoportal.NationalMapColor"
               # "MtbMap"
               # "CartoDB.Positron"
               # "CartoDB.DarkMatter"
               # "Stamen.Toner"
               # "Stamen.Watercolor"
               # "Esri.WorldGrayCanvas"
               # "OpenTopoMap"
               # "Esri.WorldTopoMap"
               # "Watercolor"
               # "Terrain"
               # "TopoMap"
               # "OSM"
               # "TomTom.Basic"
               # "TopPlusOpen" 
               )

tmap_mode("view")

tm_shape(GPS_Track_line) +
  tm_lines(col = "blue", lwd = 5)# + 
  # tm_shape(GPS_Track) +
    # tm_dots(fill = "green", size = 0.2)

tmap_providers()

tmap_options_reset()



# GPS_Track <- GPS_Track |>
#   arrange(time, .by_group = TRUE) |>
#   mutate(
#     # 0
#     timelag0 = difftime_secs(
#       lead(time),
#       time
#     ),
#     steplength0 = distance_by_element(
#       lead(geometry),
#       geometry
#     ),
#     speed0 = steplength0 / timelag0,
#     # 1
#     timelag1 = difftime_secs(
#       lead(time),
#       lag(time)
#     ),
#     steplength1 = distance_by_element(
#       lead(geometry),
#       lag(geometry)
#     ),
#     speed1 = steplength1 / timelag1,
#     # 2 | n = 2
#     timelag2 = difftime_secs(
#       lead(time, n = 2),
#       lag(time, n = 2)
#     ),
#     steplength2 = distance_by_element(
#       lead(geometry, n = 2),
#       lag(geometry, n = 2)
#     ),
#     speed2 = steplength2 / timelag2,
#     timelag3 = difftime_secs(
#       lead(time, n = 4),
#       lag(time, n = 4)
#     ),
#     #3 | n = 4
#     steplength3 = distance_by_element(
#       lead(geometry, n = 4),
#       lag(geometry, n = 4)
#     ),
#     speed3 = steplength3 / timelag3,
#     # 4 | n = 8
#     timelag4 = difftime_secs(
#       lead(time, n = 8 * 100),
#       lag(time, n = 8 * 100)
#     ),
#     steplength4 = distance_by_element(
#       lead(geometry, n = 8 * 100),
#       lag(geometry, n = 8 * 100)
#     ),
#     speed4 = steplength4 / timelag4,
#
#   )


# calc_timelag <- function(time, n){
#   difftime_secs(
#     lead(time, n),
#     lag(time, n)
#   )
# }
#
# calc_steplength <- function(geometry, n){
#   distance_by_element(
#     lead(geometry, n),
#     lag(geometry, n)
#   )
# }
#
# new_sampling_interval <- function(time, geometry, n){
#   # ...
# }
#
#
# GPS_Track <- GPS_Track |>
#   arrange(time, .by_group = TRUE) |>
#   mutate(
#     # new_sampling_interval()
#   )

add_sampling_interval <- function(.data, i, n) {
  timelag_nm    <- paste0("timelag", i)
  steplength_nm <- paste0("steplength", i)
  speed_nm      <- paste0("speed", i)

  .data |>
    mutate(
      "{timelag_nm}" := if (i == 0) {
        difftime_secs(lead(time), time)
      } else {
        difftime_secs(lead(time, n = n), lag(time, n = n))
      },

      "{steplength_nm}" := if (i == 0) {
        distance_by_element(lead(geometry), geometry)
      } else {
        distance_by_element(lead(geometry, n = n), lag(geometry, n = n))
      },

      "{speed_nm}" := .data[[steplength_nm]] / .data[[timelag_nm]]
    )
}



a <- function(n) {
  if (n <= 2) {
    return(1)
  } else {
    return(2^(n - 2))
  }
}

n_speedsteps <- 30 # 14

intervals <- tibble(
  i = 0:(n_speedsteps - 2),
  n = c(sapply(1:(n_speedsteps -1), a))
)



GPS_Track <- GPS_Track |>
  arrange(time, .by_group = TRUE)

GPS_Track <- reduce2(
  .x = intervals$i,
  .y = intervals$n,
  .f = add_sampling_interval,
  .init = GPS_Track
)

# GPS_Track_2 <- GPS_Track |>
#   st_drop_geometry() |>
#   select(time, speed0, speed1, speed2, speed3, speed4)
#
# GPS_Track_long <- GPS_Track_2 |>
#   pivot_longer(c(speed0, speed1, speed2, speed3, speed4))

GPS_Track_2 <- GPS_Track |>
  st_drop_geometry() |>
  select(time, starts_with("speed"))

GPS_Track_long <- GPS_Track_2 |>
  pivot_longer(
    c(starts_with("speed"))
  )

ggplot(GPS_Track_long, aes(name, value))+
  geom_boxplot(outliers = FALSE)


max(GPS_Track$speed, na.rm = TRUE) * 3.6
max(GPS_Track$speed0, na.rm = TRUE)

# GPS_Track_max_speed <- data.frame(GPS_Track_2 |>
#                            select(where(~ starts_with("speed"))))


GPS_Track_max_speed <- GPS_Track |>
  select(-time, -geometry, -speed) |>
  summarise(across(starts_with("speed"), ~ max(.x, na.rm = TRUE)))

GPS_long <- GPS_Track_max_speed |>
  pivot_longer(
    cols = starts_with("speed"),
    names_to = "speed_nr",
    values_to = "max_speed"
  )

ggplot(GPS_long, aes(x = speed_nr, y = max_speed, group = 1)) +
  geom_line() +
  geom_point()


GPS_long <- GPS_long |>
  mutate(
    speed_order = if_else(speed_nr == "speed", 0, parse_number(speed_nr) + 1),
    speed_nr = factor(speed_nr, levels = speed_nr[order(speed_order)])
  )

ggplot(GPS_long, aes(x = speed_nr, y = max_speed, group = 1)) +
  geom_line() +
  geom_point()


# ggplot(data = GPS_Track_max_speed, aes(x = starts_with("speed")))



GPS_Track_3d <- GPS_Track |>
  st_transform(2056) |>
  mutate(
    x = st_coordinates(geometry)[, 1],
    y = st_coordinates(geometry)[, 2],
    z = ele
  )

plot_ly(
  GPS_Track_3d,
  x = ~x,
  y = ~y,
  z = ~z,
  type = "scatter3d",
  mode = "lines",
  line = list(width = 4)
)

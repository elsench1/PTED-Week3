library("readr")
library("sf")
library("dplyr")
library("ggplot2")
library(tmap)

## Task 1

wildschwein_BE1 <- read_delim(
  "datasets/wildschwein_BE_2056.csv",
  ","
)

wildschwein_BE2 <- st_as_sf(wildschwein_BE1,
                           coords = c("E", "N"),
                           crs = 2056
                           )

# Note by ChatGPT:
# Unlike %>% from magrittr, the base pipe |> only passes the result as the first
# argument. In your case that works perfectly because sf::st_as_sf() expects the
# data as its first argument anyway.

# remove "wildschwein_BE1" and "wildschwein_BE2"
rm(wildschwein_BE1, wildschwein_BE2)


wildschwein_BE <- read_delim("datasets/wildschwein_BE_2056.csv", ",") |>
  st_as_sf(coords = c("E", "N"), crs = 2056)


## Task 2


difftime_secs <- function(later, now){
  as.numeric(difftime(later, now, units = "secs"))
}

wildschwein_BE <- wildschwein_BE |>
  group_by(TierID) %>% 
  mutate(
    timediff = difftime_secs(lead(DatetimeUTC), DatetimeUTC)
  )


wildschwein_BE_smry <- wildschwein_BE |> 
  st_drop_geometry() |> 
  group_by(TierID, TierName) |> 
  mutate(
    timediff = difftime_secs(lead(DatetimeUTC), DatetimeUTC)
  ) |>
  summarise(
    start = min(DatetimeUTC),
    end = max(DatetimeUTC),
    duration_hours = as.numeric(difftime(end, start, units = "hours")),
    n_obs = n(),
    min_dt = min(timediff, na.rm = TRUE),
    max_dt = max(timediff, na.rm = TRUE),
    mean_dt = mean(timediff, na.rm = TRUE),
    median_dt = median(timediff, na.rm = TRUE) # temporal sampling interval
  )



print("max_dt >> mean_dt -> there are gaps")


# concurrently or sequentially?
range(wildschwein_BE_smry$start)
range(wildschwein_BE_smry$end)

ggplot(wildschwein_BE_smry, aes(y = TierName))+
  geom_segment(
    aes(x = start, xend = end, yend = TierName),
    size = 4
  ) +
  labs(
    x = "Time",
    y = "Individual",
    title = "Tracking periods of wild boars"
  ) +
  theme_minimal()

print("The tracking periods overlap clearly in the timeline, 
      indicating that individuals were tracked concurrently.")

print("The temporal sampling interval between the locations is 15 minutes")
# for(i in wildschwein_BE_smry$median_dt){
#   cat("The interval between the location is ", i, " seconds \n")
# }

for(i in 1:nrow(wildschwein_BE_smry)){
  cat(
    "The temporal sampling interval for", wildschwein_BE_smry$TierName[i],
    "is", wildschwein_BE_smry$median_dt[i], "seconds\n"
  )
}



# wildschwein_BE_smry <- st_drop_geometry(wildschwein_BE)

# wildschwein_BE_smry <- summarise(group_by(wildschwein_BE_smry,
#                                                TierName))

# wildschwein_BE_smry <- wildschwein_BE_smry |> 
#   group_by(TierName, TierID) |> 
#   summarise()
# 
# number_of_individuals <- nrow(wildschwein_BE_smry)
# cat("There are ", number_of_individuals, " individuals.")
# print("They are named:")
# for(i in wildschwein_BE_smry$TierName){
#   print(i)
# }
# 
# 
# df_tracking_duration <- wildschwein_BE_without_geometry |> 
#   group_by(TierID) |>
#   summarise(
#     start = min(DatetimeUTC),
#     end = max(DatetimeUTC),
#     duration_hours = as.numeric(difftime(end, start, units = "hours"))
#   )


rm(i)



# Task 3

# later <- lag(wildschwein_BE$geometry)
# now <- wildschwein_BE$geometry
# st_distance(later, now, by_element = TRUE)

distance_by_element <- function(later, now){
  as.numeric(
    st_distance(later, now, by_element = TRUE)
  )
}


wildschwein_BE <- wildschwein_BE |>
  group_by(TierID, TierName) |>
  arrange(DatetimeUTC, .by_group = TRUE) |>
  mutate(
    steplength = distance_by_element(
      lead(geometry),
      geometry
    )
  )


# Task 4

wildschwein_BE <- wildschwein_BE |> 
  group_by(TierID, TierName) |> 
  arrange(DatetimeUTC, .by_group = TRUE) |> 
  mutate(
    speed = steplength / timediff
  )



# Task 5

wildschwein_sample <- wildschwein_BE |> 
  filter(TierName == "Sabi") |> 
  head(100)


tmap_mode("view")
tm_shape(wildschwein_sample) +
  tm_dots()


wildschwein_sample_line <- wildschwein_sample |> 
  summarise(do_union = FALSE) |> 
  st_cast("LINESTRING")

tmap_options(basemap.server = "OpenStreetMap")

tm_shape(wildschwein_sample_line) +
  tm_lines() + 
  tm_shape(wildschwein_sample) +
  tm_dots

now <- as.POSIXct("2024-04-26 10:20:00")
later <- as.POSIXct("2024-04-26 11:35:00")

later - now

difftime(later, now)
time_difference <- difftime(later,now, units = "secs")

time_difference # Does not shows what is realy saved

str(time_difference)

class(time_difference)

# with diffitme, we can be specific about our units of output
# with as.numeric we turn the output into a numeric value
time_difference <- as.numeric(difftime(later, now, units = "secs"))


str(time_difference)

class(time_difference)

#I'm creating a function that automates the steps above
# function name           functions inputs
difftime_secs <- function(spaeter, jetzt) {
  as.numeric(difftime(spaeter, jetzt, units = "secs"))
}


# testing the function with our two objects
difftime_secs(spaeter = later, jetzt = now)
difftime_secs(later, now)



## offsets with lead and lag


numbers <- 1:10

# offset the number to the "left"
dplyr::lead(numbers, n = 2)

# offset the number to the "right"
dplyr::lag(numbers, n = 6)

# default replace "NA" wiht "0"
dplyr::lag(numbers, n = 6, default = 0)



## Offset with dataframes

library(dplyr)

wildschwein <- tibble(       # aka data.frame
  TierID = rep(c("Hans", "Klara"), each = 5),
  DatetimeUTC = rep(as.POSIXct("2015-01-01 00:00:00", tz = "UTC") + 0:4 * 15 * 60, 2)
)

later <- later <- lead(wildschwein$DatetimeUTC)
now <- wildschwein$DatetimeUTC

wildschwein$timediff <- difftime_secs(later, now)

wildschwein$timediff <- NULL

# Doing the same steps above (calculating time differce between
# obesercations), but this time with mutate and grouping
mutate(
  wildschwein,
  timediff = difftime_secs(lead(DatetimeUTC), DatetimeUTC),
  .by = TierID
)


# Piping can be done with |> oder %>%
# short cut: |> with CTRS + SHIFT + m


wildschwein <- wildschwein |>
  group_by(TierID) %>% 
  mutate(
    timediff = difftime_secs(lead(DatetimeUTC), DatetimeUTC)
  )


wildschwein_smry <- wildschwein |> 
  group_by(TierID) |> 
  summarise(
    mean = mean(timediff, na.rm = TRUE)
  )






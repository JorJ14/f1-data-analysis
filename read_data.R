library(tidyverse)

# Importing data

# Info about circuits
circuits <- read_csv("data/circuits.csv")
# Info about races
races <- read_csv("data/races.csv")
# Info about different status (+1 lap, engine problem, collision, etc.)
status <- read_csv("data/status.csv")
# Info about constructors
constructors <- read_csv("data/constructors.csv")
# Info about constructors results per race
constructor_results <- read_csv("data/constructor_results.csv")
# Info about constructors results per race (more details)
constructor_standings <- read_csv("data/constructor_standings.csv")
# Info about drivers
drivers <- read_csv("data/drivers.csv")
# Info about drivers standings
driver_standings <- read_csv("data/driver_standings.csv")
# Info about qualifying
qualifying <- read_csv("data/qualifying.csv")
# Info about lap times (races)
lap_times <- read_csv("data/lap_times.csv")
# Info about pit stops
pit_stops <- read_csv("data/pit_stops.csv")
# Info about each race results
results <- read_csv("data/results.csv")
# Info about seasons
seasons <- read_csv("data/seasons.csv")
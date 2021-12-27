get_season_races <- function(season_year) {
    races %>% 
        filter(year == season_year)
}

get_season_circuits <- function(season_year) {
    circuits %>% 
        filter(circuitId %in% get(paste("races_", as.character(season_year), sep=""))$circuitId)
}

get_season_constructors_results <- function(season_year) {
    constructor_results %>% 
        filter(raceId %in% get(paste("races_", as.character(season_year), sep=""))$raceId)
}

get_season_constructors_standings <- function(season_year) {
    constructor_standings %>% 
        filter(raceId %in% get(paste("races_", as.character(season_year), sep=""))$raceId)
}

get_season_constructors <- function(season_year) {
    constructors %>% 
        filter(constructorId %in% get(paste("constructors_results_",
                                            as.character(season_year), sep=""))$constructorId)
}

get_season_results <- function(season_year) {
    results %>% 
        filter(raceId %in% get(paste("races_", as.character(season_year), sep=""))$raceId)
}

get_season_drivers <- function(season_year) {
    drivers %>% 
        filter(driverId %in% get(paste("results_", as.character(season_year), sep=""))$driverId)
}

get_season_driver_standings <- function(season_year) {
    driver_standings %>% 
        filter(raceId %in% get(paste("races_", as.character(season_year), sep=""))$raceId)
}

get_season_qualifying <- function(season_year) {
    qualifying %>% 
        filter(raceId %in% get(paste("races_", as.character(season_year), sep=""))$raceId)
}

get_season_lap_times <- function(season_year) {
    lap_times %>% 
        filter(raceId %in% get(paste("races_", as.character(season_year), sep=""))$raceId)
}

get_season_pit_stops <- function(season_year) {
    pit_stops %>% 
        filter(raceId %in% get(paste("races_", as.character(season_year), sep=""))$raceId)
}

for (x in 2021:2021) {
    assign(paste("races_", as.character(x), sep=""), get_season_races(x))
    assign(paste("circuits_", as.character(x), sep=""), get_season_circuits(x))
    assign(paste("constructors_results_", as.character(x), sep=""),
           get_season_constructors_results(x))
    assign(paste("constructors_standings_", as.character(x), sep=""),
           get_season_constructors_standings(x))
    assign(paste("constructors_", as.character(x), sep=""),
           get_season_constructors(x))
    assign(paste("results_", as.character(x), sep=""),
           get_season_results(x))
    assign(paste("drivers_", as.character(x), sep=""),
           get_season_drivers(x))
    assign(paste("driver_standings_", as.character(x), sep=""),
           get_season_driver_standings(x))
    assign(paste("qualifying_", as.character(x), sep=""),
           get_season_qualifying(x))
    assign(paste("lap_times_", as.character(x), sep=""),
           get_season_lap_times(x))
    assign(paste("pit_stops", as.character(x), sep=""),
           get_season_pit_stops(x))
}
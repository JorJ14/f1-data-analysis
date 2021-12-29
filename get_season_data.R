get_season_races <- function(season_year) {
    races %>% 
        filter(year == season_year) %>% 
        arrange(round)
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

change_qualifying_format <- function(season_year) {
    aux <- get(paste("qualifying_", as.character(season_year), sep="")) %>% 
        select(-qualifyId)
    aux <- aux %>% inner_join(select(get(paste("drivers_", as.character(season_year), sep="")),
                                     driverId, code), by = "driverId") %>% 
        select(-driverId)
    aux <- aux[, c(1, 8, 2, 3, 4, 5, 6, 7)]
    aux <- aux %>% inner_join(select(get(paste("constructors_", as.character(season_year), sep="")),
                                     constructorId, name),
                              by = "constructorId") %>% 
        select(-constructorId)
    aux <- aux[, c(1, 4, 2, 3, 8, 5, 6, 7)]
    aux <- aux %>% rename(constructor = name)
    aux <- aux %>% inner_join(select(get(paste("races_", as.character(season_year),
                                               sep="")), raceId, name, round), by = "raceId")
    aux <- aux %>% select(-raceId)
    aux <- aux[, c(8, 9, 1, 2, 3, 4, 5, 6, 7)]
    aux <- aux %>% rename(race = name)
}

change_lap_times_format <- function(season_year) {
    aux <- get(paste("lap_times_", as.character(season_year), sep=""))
    aux <- aux %>% inner_join(select(get(paste("drivers_", as.character(season_year), sep="")),
                                     driverId, code), by = "driverId") %>% 
        select(-driverId)
    aux <- aux[, c(1, 6, 2, 3, 4, 5)]
    aux <- aux %>% inner_join(select(get(paste("races_", as.character(season_year),
                                               sep="")), raceId, name, round), by = "raceId")
    aux <- aux %>% select(-raceId)
    aux <- aux[, c(6, 7, 1, 2, 3, 5, 4)]
    aux <- aux %>% rename(race = name)
    aux <- aux %>% inner_join(unique(select(get(paste("qualifying_", as.character(season_year),
                                               sep="")), code, constructor)), by = "code")
    aux <- aux[, c(1, 2, 8, 3, 4, 5, 6, 7)]
}

change_pit_stops_format <- function(season_year) {
    aux <- get(paste("pit_stops_", as.character(season_year), sep=""))
    aux <- aux %>% inner_join(select(get(paste("drivers_", as.character(season_year), sep="")),
                                     driverId, code), by = "driverId") %>% 
        select(-driverId)
    aux <- aux[, c(1, 7, 2, 3, 4, 5, 6)]
    aux <- aux %>% inner_join(select(get(paste("races_", as.character(season_year),
                                               sep="")), raceId, name, round), by = "raceId")
    aux <- aux %>% select(-raceId)
    aux <- aux[, c(7, 8, 1, 2, 3, 4, 5, 6)]
    aux <- aux %>% rename(race = name)
    aux <- aux %>% inner_join(unique(select(get(paste("qualifying_", as.character(season_year),
                                               sep="")), code, constructor)), by = "code")
    aux <- aux[, c(1, 2, 9, 3, 4, 5, 6, 7, 8)]
}

change_results_format <- function(season_year) {
    aux <- get(paste("results_", as.character(season_year), sep="")) %>% 
        select(-resultId)
    aux <- aux %>% inner_join(select(get(paste("drivers_", as.character(season_year), sep="")),
                                     driverId, code), by = "driverId") %>% 
        select(-driverId)
    aux <- aux[, c(1, 17, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)]
    aux <- aux %>% inner_join(select(get(paste("constructors_", as.character(season_year), sep="")),
                                     constructorId, name),
                              by = "constructorId") %>% 
        select(-constructorId)
    aux <- aux[, c(1, 2, 17, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)]
    aux <- aux %>% rename(constructor = name)
    aux <- aux %>% inner_join(select(get(paste("races_", as.character(season_year),
                                               sep="")), raceId, name, round), by = "raceId")
    aux <- aux %>% select(-raceId)
    aux <- aux[, c(17, 18, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)]
    aux <- aux %>% rename(race = name)
    aux <- aux %>% inner_join(status, by = "statusId") %>% select(-statusId)
}

change_constructors_results_format <- function(season_year) {
    aux <- get(paste("constructors_results_", as.character(season_year), sep="")) %>% 
        select(-status, -constructorResultsId)
    aux <- aux %>% inner_join(select(get(paste("races_", as.character(season_year),
                                               sep="")), raceId, name, round), by = "raceId")
    aux <- aux %>% select(-raceId)
    aux <- aux %>% rename(race = name)
    aux <- aux[, c(3, 4, 1, 2)]
    aux <- aux %>% inner_join(select(get(paste("constructors_", as.character(season_year), sep="")),
                                     constructorId, name),
                              by = "constructorId") %>% 
        select(-constructorId)
    aux <- aux %>% rename(constructor = name)
    aux <- aux[, c(1, 2, 4, 3)]
}

change_constructors_standings_format <- function(season_year) {
    aux <- get(paste("constructors_standings_", as.character(season_year), sep="")) %>% 
        select(-constructorStandingsId)
    aux <- aux %>% inner_join(select(get(paste("races_", as.character(season_year),
                                               sep="")), raceId, name, round), by = "raceId")
    aux <- aux %>% select(-raceId)
    aux <- aux %>% rename(race = name)
    aux <- aux[, c(6, 7, 1, 3, 4, 2, 5)]
    aux <- aux %>% inner_join(select(get(paste("constructors_", as.character(season_year), sep="")),
                                     constructorId, name), by = "constructorId") %>% 
        select(-constructorId)
    aux <- aux %>% rename(constructor = name)
    aux <- aux[, c(1, 2, 3, 4, 7, 5, 6)]
    aux %>% arrange(round, position)
}

change_drivers_standings_format <- function(season_year) {
    aux <- get(paste("driver_standings_", as.character(season_year), sep="")) %>% 
        select(-driverStandingsId)
    aux <- aux %>% inner_join(select(get(paste("races_", as.character(season_year),
                                               sep="")), raceId, name, round), by = "raceId")
    aux <- aux %>% select(-raceId)
    aux <- aux %>% rename(race = name)
    aux <- aux[, c(6, 7, 3, 4, 1, 2, 5)]
    aux <- aux %>% inner_join(select(get(paste("drivers_", as.character(season_year), sep="")),
                                     driverId, code), by = "driverId") %>% 
        select(-driverId)
    aux <- aux[, c(1, 2, 3, 4, 7, 5, 6)]
    aux <- aux %>% inner_join(unique(select(get(paste("qualifying_", as.character(season_year),
                                                      sep="")), code, constructor)), by = "code")
    aux <- aux[, c(1, 2, 3, 4, 5, 8, 6, 7)]
    aux %>% arrange(round, position)
}

get_race_winners <- function(season_year) {
    aux <- get(paste("results_", as.character(season_year), sep="")) %>% 
        select(race, round, code, constructor, position) %>% 
        filter(position == 1) %>% 
        rename(winning = position) %>% 
        arrange(round)
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
    assign(paste("pit_stops_", as.character(x), sep=""),
           get_season_pit_stops(x))
    assign(paste("qualifying_", as.character(x), sep=""),
           change_qualifying_format(x))
    assign(paste("lap_times_", as.character(x), sep=""),
           change_lap_times_format(x))
    assign(paste("pit_stops_", as.character(x), sep=""),
           change_pit_stops_format(x))
    assign(paste("results_", as.character(x), sep=""),
           change_results_format(x))
    assign(paste("constructors_results_", as.character(x), sep=""),
           change_constructors_results_format(x))
    assign(paste("constructors_standings_", as.character(x), sep=""),
           change_constructors_standings_format(x))
    assign(paste("driver_standings_", as.character(x), sep=""),
           change_drivers_standings_format(x))
    assign(paste("race_winners_", as.character(x), sep=""),
           get_race_winners(x))
}

write.csv(constructors_standings_2021, "2021_season/constructors_standings_2021.csv", row.names = TRUE)
write.csv(driver_standings_2021, "2021_season/driver_standings_2021.csv", row.names = TRUE)
write.csv(race_winners_2021, "2021_season/race_winners_2021.csv", row.names = TRUE)

driver_standings_2021 <- read_csv("2021_season/driver_standings_2021.csv")
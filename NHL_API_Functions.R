library(httr)
library(jsonlite)
library(dplyr)

#' @title Get teams
#' @examples
#' teams <- get_teams(); print(teams)
get_teams <- function() {
    url <- "https://statsapi.web.nhl.com/api/v1/teams"
    response <- GET(url)

    if(http_status(response)$category != 'Success') {
        warning("Failed to fetch teams")
        return(data.frame())
    }

    teams_json <- content(response, "text", encoding = "UTF-8")
    teams_data <- fromJSON(teams_json, flatten = TRUE)

    # Processing response to create a dataframe
    teams_df <- teams_data$teams %>%
        # select(id, name, abbreviation, teamName, locationName, firstYearOfPlay) |>
        as_tibble()

    return(teams_df)
}

#' @title Get player information by player_id
#' @examples
#' # Replace [player_id] with a valid NHL player ID (e.g., 8477462)
#' get_player_info([player_id])
#'
get_player_info <- function(player_id) {
    url <- sprintf("https://statsapi.web.nhl.com/api/v1/people/%s", player_id)
    response <- GET(url)

    if(http_status(response)$category != 'Success') {
        warning(paste("Failed to fetch player info for player ID:", player_id))
        return(data.frame())
    }

    player_json <- content(response, "text", encoding = "UTF-8")
    player_data <- fromJSON(player_json, flatten = TRUE)

    # Processing response to create a dataframe
    player_df <- player_data$people %>% as_tibble()
        # select(id, fullName, nationality, birthCity, birthCountry, primaryPosition.code)

    return(player_df)
}


#' @title Function to get player ID by searching a player's full name
#' @details
#' Wrapper of nhlapi function nhl_players
#'
#' @examples
#' # Note: The player_name must be formatted in "Firstname Lastname" format (e.g., "Sidney Crosby")
#' get_player_id_by_name("Sidney Crosby")
#' get_player_id_by_name("Connor Mcdavid")
#'
get_player_id_by_name <- function(player_name) {
    require(nhlapi)

    players_df <- nhlapi::nhl_players(player_name) |> as_tibble()

    return(players_df)
}


#' @title Function to get team ID and info by searching a team's name
#' @param team_name City and/or team name
#' @examples
#' # example code
#' get_team_id_by_name("Pittsburgh Penguins")
#' get_team_id_by_name("Penguins")
#' get_team_id_by_name("Pittsburgh")
get_team_id_by_name <- function(team_name) {
    all_teams_df <- get_teams()

    if (nrow(all_teams_df) == 0) {
        warning("No team data available")
        return(data.frame())
    }

    teams_filtered_df <- all_teams_df %>%
        filter(grepl(team_name, name, ignore.case = TRUE) | grepl(team_name, teamName, ignore.case = TRUE) | grepl(team_name, locationName, ignore.case = TRUE))

    return(teams_filtered_df)
}


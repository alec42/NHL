library(shiny)
library(shinyWidgets) # for time slider
library(scales)
library(tidyverse)
library(hockeyR)
library(sportyR)

# ref for slider time: https://community.rstudio.com/t/time-in-sliderinput/148971/2
time_labels <- expand.grid(minutes = 0:60, seconds = 0:59) %>%
    # add leading 0's
    mutate_at(1:2, ~ifelse(nchar(.) == 1, paste0('0', .), .)) %>%
    # create label
    mutate(time_label = paste0(minutes, ':', seconds)) %>%
    arrange(time_label) %>%
    # remove anything over 60:00
    filter(time_label <= '60:00') %>%
    pull(time_label)

ui <- fluidPage(
    dateInput("game_date", "Select Game Date"),
    actionButton("load_button", "Load"),
    selectInput("game_id_slider", "Select Game", choices = NULL),
    actionButton("load_button2", "Load"),

    # tags$head(tags$script("
    # $(document).ready(function() {
    #     $('#time_slider').on('input', function() {
    #       var value = $(this).val();
    #       var minutes = Math.floor(value / 60);
    #       var seconds = value % 60;
    #       $('#time_label').text(minutes + ' minutes ' + seconds + ' seconds');
    #     });
    #   });
    # ")),
    # sliderInput("time_slider", "Elapsed Time", min = 0, max = 3600, value = 0, step = 1),
    # h4(id = "time_label", "0 minutes 0 seconds"),
    shinyWidgets::sliderTextInput("timeSlider", "Select time of game (minutes:seconds)", choices = time_labels),
    textOutput('timeSelection'),

    plotOutput("player_plot")
)

server <- function(input, output, session) {
    ## Time Slider
    nb_minutes <- reactive(as.numeric(substr(input$timeSlider, 1, 2)))
    nb_seconds <- reactive(as.numeric(substr(input$timeSlider, 4, 5)))
    total_seconds <- reactive(60 * nb_minutes() + nb_seconds())
    output$timeSelection = renderText({
        req(input$timeSlider)
        # total_seconds = 60 * nb_minutes() + nb_seconds()
        out = paste0('You selected ', nb_minutes(), ' min, ', nb_seconds(), ' sec (', scales::comma(total_seconds()), ' total seconds)')
        out
    })

    ## List of Games for Specified Date
    observeEvent(input$load_button, {
        game_id_df <- get_game_ids(day = as.Date(input$game_date, "%Y-%m-%d")) |>
            mutate(
                home_team = str_extract(home_name, "\\s(\\w+)", 1),
                away_team = str_extract(away_name, "\\s(\\w+)", 1),
                home_game = paste0(home_team, " @ ", away_team),
                .keep="unused"
            ) |> select(game_id, home_game)
        updateSelectInput(session, "game_id_slider", choices = setNames(game_id_df$game_id, game_id_df$home_game))
    })

    ## Game Information
    observeEvent(input$load_button2, {
        if (!is.null(input$game_id_slider)) {
            print(input$game_id_slider)
            player_positions_df <- get_game_shifts(game_id = input$game_id_slider) %>%
                filter(game_seconds == total_seconds()) %>%
                left_join(get_players(), by = c("player_id" = "player_id")) %>%
                select(player_id, x_coord, y_coord) %>%
                filter(!is.na(x_coord), !is.na(y_coord))

            output$player_plot <- renderPlot({
                player_positions_df %>%
                    ggplot() +
                    geom_point(aes(x = x_coord, y = y_coord)) +
                    labs(x = "X Coordinate", y = "Y Coordinate") +
                    theme_minimal()
            })
        }
    })

}

shinyApp(ui = ui, server = server)

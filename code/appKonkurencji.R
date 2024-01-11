library(dplyr)
library(ggplot2)
library(shinydashboard)
library(shiny)
library(shinycssloaders)
library(plotly)
library(lubridate)
library(spotifyr)
library(tidyr)
library(shinyWidgets)
library(shinyjs)
library(jsonlite)
library(fmsb)
library(cowplot)
library(extrafont)
library(showtext)


# loadfonts()
# showtext_auto()
# font_add("Gotham", "../dane/font/Gotham-Bold.otf")

####   Wczytanie Danych   ####

# Songs <- fromJSON("./dane/Songs.json")
# 
# SH <- fromJSON("./dane/SpotifyExtendedAll.json")
# 
# minutesPerWeek <- fromJSON("./dane/minutesPerWeek.json")
# 
# playlist <- fromJSON("./dane/playlistData.json")
#
# compatibility_data <- fromJSON("./dane/compatibility_data.json")

Songs <- fromJSON("data/Songs.json")

SH <- fromJSON("data/SpotifyExtendedAll.json")

playlist <- fromJSON("data/playlistData.json")

compatibility_data <- fromJSON("data/compatibility_data.json")

####   Style   ####

HTML_styles <- '
      * {
        font-family: "Gotham";
      }

      .main-header {
        position: fixed;
        width: 100%;
      }

      .skin-green .main-header .logo, .skin-green .main-header .navbar{
        background-color: #000;
      }

      .skin-green .main-header .logo:hover {
        background-color: #1DB954;
      }

      .skin-green .main-header .navbar .sidebar-toggle:hover {
        background-color: #1DB954;
      }

      body {
        color: #FFFFFF;
        background-color: #000;
        font-size: 3.2vh;
      }
      
      .content {
        margin-top: 50px;
        min-height: calc(88vh - 50px);
        margin-bottom: 12vh;
        background-color: #000;
        padding: 0;
      }
      
      .body-panel {
        min-height: calc(88vh - 50px);
        border: 6px solid #000;
        border-radius: 15px;
        background-color: #121212;
      }
      
      .content .wrapped {
        margin-bottom: 12vh;
        background-color: #121212;
      }
      
      .sidebar {
        display: block !important;
        width: 40vh;
        height: calc(88vh - 50px);
        padding: 0px;
        background-color: #000;
        position: fixed;
      }

      .sidebar-panel {
        display: block;
        width: 100%;
        height: 100%;
        flex-direction: column;
        justify-content: 
        background-color: #000;
        border: 1px solid #000;
      }

      .sidebar-panel-upper{
        display: flex;
        height: calc(20% - 10.5px);
        background-color: #121212;
        border-radius: 10px;
        margin: 7px;
        flex-direction: column;
        justify-content: center;
      }

      .sidebar-panel-lower {
        display: flex;
        height: calc(80% - 10.5px);
        background-color: #121212;
        border-radius: 10px;
        margin: 7px;
        flex-direction: column;
        justify-content: top;
      }

      html {
        font-size: 16px;
        color: #FFFFFF !important;
      }
      
      * {
          font-family: "Gotham", sans-serif;
          letter-spacing: -0.35px;
      }

      .top-text {
        display: inline-block;
        margin-left: 20px;
        margin-bottom: 55px;
      }

      .text-fav {
        margin-left:30px;
        font-weight:bold;
      }
      
      .pretty .state label {
        color: white;
        height: 36%;
        font-size: 18px;
        top: 0;
        margin-bottom: 18%;
        margin-top: 18%;
        font-weight:bold;
      }

      .pretty .state label:before {
        top: 10px;
      }

      .pretty .state label:after {
        top:10px;
      }

      .control-label{
        font-weight:600;
        margin-bottom: 20px;
      }
      
      .footer {
        display: flex;
        justify-content: space-between;
        background-color: #000;
        flex-direction: row;
        position: fixed;
        bottom: 0;
        width: 100%;
        height: 12vh;
        padding: 0 2vh;
        left: 0;
        z-index: 1000;
      }
      
      .footer-left-panel {
        width: 30%;
        display: flex;
        justify-content: left;
        height: 100%;
        align-items: center;
      }
             
      .our-cover {
        width: 8.5vh;
        height: 8.5vh;
        border-radius: 10%;
        margin-right: 2vh;
      }
      
      .footer-center-panel {
        margin-top: 2vh;
        width: 33%;
        display: flex;
        flex-direction: column;
        align-items: center;
        justify-content: center;
        z-index: 2000;
      }

      .icons {
        width: 50%;
        display: flex;
        justify-content: center;
        flex-direction: row;
      }
      
      .slider {
        margin-top: -50px;
        width: 100%;
      }
      
      .slider .irs-handle, .slider2 .irs-handle, .slider3 .irs-handle {
        background-color: transparent !important;
        height: 0px;
        width: 0px;
        top: 25px;
      }
      
      .slider:hover .irs-handle, .slider2:hover .irs-handle, .slider3:hover .irs-handle {
        background-color: white !important;
        height: 13px;
        width: 13px;
        top: 22px;
      }

      .slider .irs-line, .slider2 .irs-line, .slider3 .irs-line{
        background: #909090;
        height: 0.6vh;
        border: none;
      }

      .slider .irs-bar, .slider2 .irs-bar, .slider3 .irs-bar{
        top: 25px;
        height: 0.6vh;
        background: white;
      }

      
      .slider:hover .irs-bar, .slider2:hover .irs-bar, .slider3:hover .irs-bar{
        top: 25px;
        height: 0.6hv;
        width: 100%;
        border-radius: 1vh;
        background: #1DB954;
      }

      .slider .irs-from, .slider .irs-to, .slider .irs-single, .slider .irs-min, .slider .irs-max  {
        color: #909090;
        text-shadow: none;
        background-color:#000;
        border-radius: 1.5vh;
        font-size: 2vh;
      }

      .slider3 .irs-from, .slider3 .irs-to, .slider3 .irs-single {
        color: #909090;
        text-shadow: none;
        background-color: transparent;
        border-radius: 1.5vh;
        font-size: 2vh;
      }

      .slider .irs-min {
        left: - 10vh;
        top: 2vh;
      }

      .slider .irs-max {
        right: - 10vh;
        top: 2vh;
      }

      .footer-right-panel {
        display: flex;
        width: 30%;
        justify-content: right;
        flex-direction: row;
        height: 100%;
        align-items: center;
        padding-right: 20px;
      }

      .slider2 {
        width: 30%;
        border-radius: 10%;
      }

      .slider2 .irs-min, .slider2 .irs-max, .slider2 .irs-from, .slider2 .irs-to, .slider2 .irs-single, .slider3 .irs-min, .slider3 .irs-max {
        display: none;
      }

      .icons2 {
        display: flex;
        width: 40%;
        flex-direction: row;
      }

      .compatibility {
        display: flex;
        margin: 5.5vh 5vh 30vh 5vh;
        flex-direction: column;
      }

      .pretty_box {
        display: flex;
        flex-direction: row;
        justify-content: center;
        align-items: stretch;
        height: 10vh;
      }

      .compatibility_plot {
        display: flex;
        justify-content: center;
        height: 40vh;
        margin-bottom: 15vh;
      }

      .meter {
        height: 10vh;
        justify-content: center;
      }

      .playlist-panel {
        display: flex;
        width: 100%;
        flex-direction: column;
        margin: 5.5vh 5vh 7vh 5vh;
        justify-content: center;
      }

      .checkbox {
        display: flex;
        flex-direction: row;
        justify-content: flex-start;
        align-items: stretch;
        height: 7vh;
        font-family: "Gotham";
        font-size: 18px;
      }

      .slider3 {
        width: 200%;
        display: flex;
        flex-direction: row;
        justify-content: flex-start;
        align-items: center;
      }

      .slider-box {
        display: flex;
        width: 100%;
        flex-direction: row;
        justify-content: flex-start;
        align-items: center;
      }

      .skin-green .sidebar a {
        color: white;
      }
      
      .main-header .sidebar-toggle {
        font-size: 14px;
      }

 /*     .wrapped{
        display: flex;
        flex-direction: column;
      }
      
      .row1 {
        display: flex;
        flex-direstion: row;
        height: 45vh;
      }

      .11 {
        display: flex;
        flex-direction: column:
        width: 75%;
      }

      .12{
        display: flex;
        flex-direction: column:
        width: 25%;
      }

      .row2, row3, row4 {
        display: flex;
        flex-direstion: row;
        height: 45vh;
      }

      .21, .22, .31, .32, .41, .42 {
        display: flex;
        flex-direction: column:
        width: 50%;
*/
      }
      '


####   UI   ####

ui <- dashboardPage(
  title = "Jestem Akustyczny",
  skin = "green",
  dashboardHeader(
    title = img(src = "https://raw.githubusercontent.com/FylypO/DVT---Project/59d779c8d9e780649209a29821fac10dcf4f2db2/logo.png", style = 'height: 40px;'),
    titleWidth = '40vh'),
  dashboardSidebar(
    width = '40vh',
    div(
      class = 'sidebar',
      div(
        class = "sidebar-panel",
        div(
          class = "sidebar-panel-upper",
          prettyRadioButtons(
            inputId = "year",
            label = NULL,
            thick = TRUE,
            fill = TRUE,
            shape = "round",
            choices = c(2022,2023),
            selected = 2022,
            status = "default")),
        div(
          class = "sidebar-panel-lower",
          sidebarMenu(
            id = "tabs",
            h3("Features", style = "margin-left: 5%; font-size: 35px; font-weight: bold;"),
            menuItem("  Wrapped", tabName = "wrapped"),
            menuItem("  Compatibility", tabName = "compatibility"),
            menuItem("  Playlist", tabName = "playlist"),
            menuItem("  Summary", tabName = "summary")))))),
  dashboardBody(
    tags$head(tags$style(HTML_styles)),
    div(
      class = 'body-panel',
      tabItems(
        tabItem(
          tabName = "wrapped",
          fluidPage(
            # Tutaj kod dla wrapped
            uiOutput("wrapped_title"),
            fluidRow(
              div(
                class = 'wrapped',
                div(
                  class = 'row1',
                  div(
                    class = '11',
                    h3('Some stats', class = 'text-fav')),
                  div(
                    class = '12',
                    h3("Top Artists", class = "text-fav"),
                    plotlyOutput("topArtists"))),
                div(
                  class = 'row2',
                  div(
                    class = '21',
                    h3("Top Tracks by this artist", class = "text-fav"),
                    tableOutput("clickT")),
                  div(
                    class = '22',
                    h3("Artist's Songs Mean Features", class = "text-fav"),
                    plotOutput("clickP"))),
                div(
                  class = 'row3',
                  div(
                    class = '31',
                    h3("Top Tracks", class = "text-fav"),
                    plotlyOutput("topSongs")),
                  div(
                    class = '32',
                    h3("Features for listened tracks", class = "text-fav"),
                    selectInput(
                      inputId = "parameter",
                      label = "Parameter:",
                      choices = c(
                        "Danceability" = "danceability",
                        "Energy" = "energy",
                        "Liveness" = "liveness",
                        "Speechiness" = "speechiness",
                        "Valence" = "valence",
                        "Instrumentalness" = "instrumentalness",
                        "Acousticness" = "acousticness"
                      )),
                    plotlyOutput("violin"))),
                div(
                  class = 'row4',
                  div(
                    class = '41',
                    h3("Average minutes listened per day of the week", class = "text-fav"),
                    plotlyOutput("minutesPerDayOfWeek")),
                  div(
                    class = '42',
                    h3("Average listening through the day", class = "text-fav"),
                    plotlyOutput("listeningThroughDay"))))))),
        tabItem(
          tabName = "compatibility",
          fluidPage(
            # Compatibility
            div(
              class = 'compatibility',
              uiOutput("compatibility_title"),
              uiOutput("radio_button_label"),
              div(
                class = "pretty_box",
                prettyRadioButtons(
                  inputId = "person_to_compare",
                  choices = c("Karolina", "Filip", "Bartek"),
                  label = NULL,
                  selected = "Karolina",
                  inline = TRUE,
                  status = "default")),
              div(
                class = 'compatibility_plot',
                plotlyOutput("compatibility_analysis")),
              div(
                class = 'meter',
              uiOutput("compatibility_meter")))),
        ),
        tabItem(
          tabName = "playlist",
          fluidPage(
            div(
              class = "playlist-panel",
              uiOutput("playlist_title"),
              h3("Choose people and number of songs for your custom playlist:",
                 style = "text-align: left; font-family: 'Gotham', font-weight: bold;"),
              div(
                class = 'checkbox',
                checkboxGroupInput("selected_people",
                                     label = NULL,
                                     choices = c('Karolina', 'Bartek', 'Filip', 'Danonek1', 'Danonek2'),
                                     inline = TRUE,
                                     width = '100%',
                                     selected = "Karolina")),
              div(class = "slider-box",
                  div(class = 'slider3',
                  sliderInput("song_count_slider",
                              NULL,
                              min = 10,
                              max = 50,
                              value = 15,
                              ticks = FALSE))),
              uiOutput("song_list_output")))),
        tabItem(
          tabName = "summary",
          fluidPage(
            # Tutaj kod do podsumowania
            uiOutput("summary_title"))))),
    div(
      div(
        class = "footer-left-panel",
        uiOutput("our_cover"),
        uiOutput("left_caption"),
        uiOutput("heart")),
      div(
        class = "footer-center-panel",
        div(
          icon("shuffle", style = "margin: auto; font-size: 2.5vh; color: #909090"),
          actionButton(inputId = "backward", label = "", icon = icon("backward-step"), style = "z-index:4000; cursor:pointer; margin: 0 0 auto; border-radius: 5px; border: none; font-size: 2.5vh; color: #909090; background-color: #000;", class = "btn-xs"),
          icon("pause-circle", style = "margin: auto; font-size: 3.5vh;", class = "fas"),
          actionButton(inputId = "forward", label = "", icon = icon("forward-step"), style = "z-index:4000; cursor:pointer; margin: 0 0 auto; border-radius: 5px; border:none; font-size: 2.5vh; color: #909090; background-color: #000;", class = "btn-xs"),
          icon("repeat", style = "margin: auto; font-size: 2.5vh; color: #909090"),
          class = "icons"),
        div(
          class = "slider",
          sliderInput(
            "Months",
            "",
            min = 1,
            max = 12,
            value = c(1,12),
            width = '100%',
            ticks = FALSE))),
      div(
        class = "footer-right-panel",
        div(
          class = "icons2",
          icon("square-poll-vertical", style = "margin: auto; font-size: 2vh; color: #909090"),
          icon("microphone", style = "margin: auto; font-size: 2vh; color: #909090"),
          icon("bars", style = "margin: auto; font-size: 2vh; color: #909090"),
          icon("speaker-deck", style = "margin: auto; font-size: 2vh; color: #909090"),
          icon("volume-low", style = "margin: auto; font-size: 2vh; color: #909090"),),
        div(
          class = "slider2",
          sliderTextInput(
            "user",
            NULL,
            choices = c("Karolina", "Bartek", "Filip"),
            selected = "Karolina"))),
      class = "footer")))


####   Server   ####


server = function(input, output, session) {
  
  ####   Praca na Danych   ####
  
  #########################   WRAPPED   ######################################
  #### MINUTES PER WEEK ####
  mPWfiltered <- reactive({
    numOfDays <- data.frame(date = seq(as.Date("2022-01-01"), as.Date("2023-12-31"), by = "days")) %>% 
      mutate(dayOfWeek = weekdays(date), month = month(date), year = year(date)) %>% 
      group_by(year, month, dayOfWeek) %>% 
      summarise(count = n()) %>% 
      filter(year == input$year, as.numeric(month) >= input$Months[1] & as.numeric(month) <= input$Months[2])
    
    minutesPerWeek <- SH %>%
      filter(person == input$user, year == input$year, as.numeric(month) >= input$Months[1] & as.numeric(month) <= input$Months[2]) %>% 
      mutate(date = as.Date(ts)) %>% 
      mutate(month = month(date),
             week = week(date),
             dayOfWeek = weekdays(date)) %>%
      group_by(dayOfWeek, month, year) %>% 
      summarise(time = sum(ms_played) / 60000)
    
    minutesPerWeek <- inner_join(minutesPerWeek, numOfDays, by = c('dayOfWeek', 'month', 'year')) %>% 
      group_by(dayOfWeek) %>% 
      summarise(time = sum(time) / sum (count))
    
    return(minutesPerWeek)
  })
  
  output$minutesPerDayOfWeek <- renderPlotly({
    
    y_ticks <- pretty(c(max(mPWfiltered()$time), 0), n = 4)
    
    plot_ly(mPWfiltered(),
            x = ~dayOfWeek,
            y = ~time,
            type = "bar",
            marker = list(color = '#1DB954')) %>%
      animation_opts(1000, easing = "elastic", redraw = FALSE) %>%
      layout(
        yaxis = list(
          title = "Minutes listened",
          tickvals = y_ticks,
          gridcolor = '#606060',
          range = c(0, max(mPWfiltered()$time))),
        xaxis = list(
          title = 'Day',
          categoryorder = "array",
          categoryarray = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
        ),
        plot_bgcolor = "transparent",
        paper_bgcolor = "transparent",
        bargap = 0.1,
        font = list(color = 'white', family = "Gotham")
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  
  #### LISTENING THROUGH THE DAY ####
  lTDfiltered <- reactive({
    numOfDays <- data.frame(date = seq(as.Date("2022-01-01"), as.Date("2023-12-31"), by = "days")) %>% 
      mutate(day_of_week = weekdays(date), month = month(date), year = year(date)) %>% 
      group_by(year, month, day_of_week) %>% 
      summarise(count = n()) %>% 
      filter(year == input$year, as.numeric(month) >= input$Months[1] & as.numeric(month) <= input$Months[2]) %>%
      group_by(year) %>% 
      summarise(n = sum(count)) %>% 
      select(n)
    
    lTD <- SH %>% 
      filter(person == input$user, year == input$year, as.numeric(month) >= input$Months[1] & as.numeric(month) <= input$Months[2]) %>% 
      mutate(ts = ymd_hms(ts),
             hour = hour(ts)) %>% 
      group_by(hour) %>%
      summarise(time = sum(ms_played) / (60 * 1000)) %>%
      group_by(hour) %>% 
      summarise(time = time / as.numeric(numOfDays)) %>% 
      arrange(hour)
    return(lTD)
  })
  
  output$listeningThroughDay <- renderPlotly({
    y_ticks <- pretty(lTDfiltered()$time, n = 4)
    
    plot_ly(lTDfiltered(),
            x = ~hour,
            y = ~time,
            type = "bar",
            marker = list(color = '#1DB954')) %>%
      animation_opts(1000, easing = "elastic", redraw = FALSE) %>%
      layout(
        xaxis = list(
          title = "Hour",
          tickmode = "linear",
          tick0 = 0,    
          dtick = 1
        ),
        yaxis = list(
          title = "Minutes listened",
          tickvals = y_ticks,
          gridcolor = '#606060',
          range = c(0, max(lTDfiltered()$time))
        ),
        plot_bgcolor = "transparent",
        paper_bgcolor = "transparent",
        bargap = 0.1,
        font = list(color = 'white', family = "Gotham")
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  #### TOP ARTISTS #### 
  SHfilteredArtists <- reactive({
    SH %>% 
      filter(person == input$user, year == input$year, as.numeric(month) >= input$Months[1] & as.numeric(month) <= input$Months[2]) %>% 
      group_by(master_metadata_album_artist_name) %>% 
      summarise(time = sum(ms_played) / 60000) %>% 
      arrange(-time) %>% 
      head(10) %>% 
      na.omit() 
  })
  
  output$topArtists <- renderPlotly({
    
    x_ticks <- pretty(c(max(SHfilteredArtists()$time),0), n = 7)
    
    plot_ly(SHfilteredArtists(),
            x = ~time,
            y = ~reorder(master_metadata_album_artist_name, time),
            type = "bar",
            marker = list(color = '#1DB954'),
            orientation = 'h') %>%
      layout(
        xaxis = list(title = "Minutes Listened", ticks = x_ticks, gridcolor = "#606060"),
        yaxis = list(title = list(text = "Artist", standoff = 10)),
        plot_bgcolor = "transparent",
        paper_bgcolor = "transparent",
        bargap = 0.1,
        font = list(color = 'white', family = "Gotham")
      ) %>%
      config(displayModeBar = FALSE) %>% 
      event_register('plotly_click')
  })
  
  SHfilteredArtistsSongs <- reactive({
    SH %>%
      filter(person == input$user, year == input$year, as.numeric(month) >= input$Months[1] & as.numeric(month) <= input$Months[2]) %>%
      group_by(master_metadata_album_artist_name, master_metadata_track_name) %>%
      summarise(time = sum(ms_played)) %>%
      na.omit() %>%
      mutate(song_length = Songs$duration_ms.x[match(master_metadata_track_name, Songs$master_metadata_track_name)]) %>%
      group_by(master_metadata_album_artist_name, master_metadata_track_name) %>%
      summarise(times_played = round(time / song_length)) %>%
      arrange(-times_played)
  })
  
  ArtistFeaturesfiltered <- reactive({
    Songs %>% 
      filter(person == input$user) %>% 
      group_by(master_metadata_album_artist_name) %>% 
      select(master_metadata_album_artist_name, danceability:valence) %>% 
      summarise(across(danceability:valence, mean)) %>% 
      select(-c(mode, key, loudness)) %>% 
      na.omit()
  })
  
  observeEvent(c(input$person, input$backward, input$forward), {
    plotlyProxy("clickT") %>% 
      plotlyProxyInvoke("restyle", list(y = NULL))
    plotlyProxy("clickP") %>% 
      plotlyProxyInvoke("restyle", list(y = NULL))
  })
  
  output$clickT <- renderTable({
    selected_artist <- SHfilteredArtists()$master_metadata_album_artist_name[1]
    selected_data <- event_data("plotly_click")
    if (!is.null(selected_data)) {
      selected_artist <- selected_data$y
    }
    selected_songs <- SHfilteredArtistsSongs() %>%
      filter(master_metadata_album_artist_name == selected_artist) %>% 
      head(10) %>% 
      mutate(lp = seq_along(master_metadata_track_name)) %>% 
      select(lp, everything())
    colnames(selected_songs) <- c("  ", "Artist", "Track", "Times Played")
    return(selected_songs)
  })
  
  output$clickP <- renderPlot({
    selected_artist <- SHfilteredArtists()$master_metadata_album_artist_name[1]
    selected_data <- event_data("plotly_click")
    if (!is.null(selected_data)) {
      selected_artist <- selected_data$y
    }
    selected <- ArtistFeaturesfiltered() %>% 
      filter(master_metadata_album_artist_name == selected_artist) %>% 
      select(danceability:valence)
    selected <- rbind(0, 1, selected) 
    par(bg = "#121212", col = "white", family = "Gotham")
    rc <- radarchart(selected, 
                     axistype = 1, 
                     pcol = '#1db954',
                     pfcol = alpha('#1db954', 0.3),
                     plwd = 2,
                     plty = 1,
                     cglty = 2,
                     axislabcol = "white",
                     cglcol = "#b3b3b3",
                     col = "#b3b3b3",
                     caxislabels = c(0, 0.25, 0.5, 0.75, 1),
                     title = " ")
    return(rc)
  })
  
  
  #### TOP SONGS ####
  
  SHfilteredSongs <- reactive({
    SH %>% 
      filter(person == input$user, year == input$year, as.numeric(month) >= input$Months[1] & as.numeric(month) <= input$Months[2], ms_played > 30000) %>% 
      group_by(master_metadata_track_name) %>% 
      summarise(times_played = n()) %>% 
      arrange(-times_played) %>% 
      na.omit() %>% 
      head(10)
  })
  
  output$topSongs <- renderPlotly({
    
    x_ticks <- pretty(c(max(SHfilteredSongs()$times_played),0), n = 5)
    
    plot_ly(SHfilteredSongs(),
            x = ~times_played,
            y = ~reorder(substr(master_metadata_track_name, 1, 16), times_played),
            type = "bar",
            marker = list(color = '#1DB954'),
            orientation = 'h',
            hovertext = ~master_metadata_track_name) %>%
      layout(
        xaxis = list(title = "Times Played", gridcolor = '#606060', ticks = x_ticks),
        yaxis = list(title = list(text = "Track Name", standoff = 10)),
        plot_bgcolor = "transparent",
        paper_bgcolor = "transparent",
        bargap = 0.1,
        font = list(color = 'white', family = "Gotham")
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  #### VIOLIN FEATURES ####
  output$violin <- renderPlotly({
    songs <- SH %>% 
      filter(person == input$user, year == input$year, as.numeric(month) >= input$Months[1] & as.numeric(month) <= input$Months[2]) %>% 
      distinct(master_metadata_track_name)
    
    SongsFeaturesfiltered <- Songs %>% 
      left_join(songs, by = "master_metadata_track_name") %>% 
      select(danceability:valence, person) %>%
      select(-c(mode, key, loudness)) %>%
      filter(person == input$user) %>%
      na.omit()
    
    gv <- ggplot(SongsFeaturesfiltered, aes(x = person, y = !!sym(input$parameter))) +
      geom_violin(fill = "#1DB954", color = "#1DB954", alpha = 0.7) +
      coord_flip() +
      labs(title = paste("Distribution of songs by", input$parameter), y = input$parameter) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, colour = "white", family = "Gotham"),
        axis.text.x = element_text(colour = "white", size = 12, family = "Gotham"),         
        axis.text.y = element_text(colour = "white", size = 12, family = "Gotham"),         
        text = element_text(size = 13, colour = "white", family = "Gotham"),        
        legend.title = element_blank(),        
        panel.background = element_rect(fill = "#121212"),       
        plot.background = element_rect(fill = "#121212"),         
        legend.background = element_rect(fill = "#121212")     
      ) +
      scale_y_continuous(limits = c(0, 1))
    gv <- ggplotly(gv)
    gv <- gv %>% 
      config(displayModeBar = FALSE) 
    return(gv)
  })
  
  
  
  #########################  COMPATIBILITY  ##################################
  
  
  output$compatibility_analysis <- renderPlotly({
    data <- compatibility_data %>%
      filter(person %in% c(input$user, input$person_to_compare))
    
    background <- data.frame(
      person =  rep("Background", times = 8),
      feature = c("Valence", "Speechiness", "Popularity", "Liveness", "Instrumentalness", "Energy", "Danceability", "Acousticness"),
      value = rep(1, times = 8)
      
    )
    
    plot_left <- ggplot(background[background$person == "Background", ], aes(x = as.factor(feature), y = value)) +
      geom_bar(stat = "identity", fill = "#b3b3b3", alpha = 0.1) +
      geom_bar(data = data[data$person == input$person_to_compare, ], aes(x = as.factor(feature), y = value),
               stat = "identity", fill = "#1db954", alpha = 0.5) +
      geom_bar(data = data[data$person == input$user, ], aes(x = as.factor(feature), y = value),
               stat = "identity", fill = "#1db954") +
      coord_flip() +
      scale_y_reverse() +
      theme_minimal() +
      theme(
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none"
      )
    
    plot_left <- ggplotly(plot_left, tooltip = "all", dynamicTicks = TRUE) %>% 
      layout(xaxis = list(showgrid = FALSE, showline = FALSE),
             yaxis = list(showgrid = FALSE, showline = FALSE))
    
    plot_right <- ggplot(background[background$person == "Background", ], aes(x = as.factor(feature), y = value)) +
      geom_bar(stat = "identity", fill = "#b3b3b3", alpha = 0.1) +
      geom_bar(data = data[data$person == input$user, ], aes(x = as.factor(feature), y = value),
               stat = "identity", fill = "#1db954", alpha = 0.5) +
      geom_bar(data = data[data$person == input$person_to_compare, ], aes(x = as.factor(feature), y = value),
               stat = "identity", fill = "#1db954") +
      coord_flip() +
      theme_minimal() +
      theme(
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none"
      )
    
    plot_right <- ggplotly(plot_right, tooltip = "all", dynamicTicks = TRUE) %>% 
      layout(xaxis = list(showgrid = FALSE, showline = FALSE),
             yaxis = list(showgrid = FALSE, showline = FALSE))
    
    combined_plot <- subplot(plot_left, plot_right, nrows = 1, margin = 0.1)
    
    column_descriptions <- c("Valence", "Speechiness", "Popularity", "Liveness", "Instrumentalness", "Energy", "Danceability", "Acousticness")
    
    combined_plot <- combined_plot %>%
      add_annotations(
        text = column_descriptions,
        x = 0.49, 
        y = c(0.96, 0.84, 0.72, 0.56, 0.44, 0.28, 0.16, 0.04),
        showarrow = FALSE,
        xref = "paper",
        yref = "paper",
        font = list(color = 'white', family = "Gotham", weight = "bold")
      )
    
    combined_plot <- combined_plot %>%
      add_annotations(
        text = c(input$user, input$person_to_compare),
        x = c(0.13, 0.83),  
        y = 1.07, 
        showarrow = FALSE,
        xref = "paper",
        yref = "paper",
        font = list(color = 'white', family = "Gotham", size = 20, weight = "bold")
      )
    
    combined_plot <- combined_plot %>%
      animation_opts(1000, easing = "elastic", redraw = FALSE) %>%
      layout(
        showlegend = FALSE,
        plot_bgcolor = "transparent",
        paper_bgcolor = "transparent",
        font = list(color = 'white', family = "Gotham", size = 18)
      ) %>%
      config(displayModeBar = FALSE)
    
    combined_plot
  })
  
  output$compatibility_meter <- renderUI({
    
    artists <- SH %>% 
      select(master_metadata_album_artist_name, master_metadata_track_name, ms_played, person) %>% 
      group_by(master_metadata_album_artist_name, person) %>% 
      summarise(time = sum(ms_played / 60000),
                .groups = "keep") %>% 
      na.omit()
    
    left <- artists %>% 
      filter(person == input$user) %>% 
      arrange(desc(time)) %>% 
      head(100)
    
    left_time <- sum(left$time, na.rm = TRUE)
    
    left <- left %>% 
      mutate(points = time / left_time * 100)
    
    right <- artists %>% 
      filter(person == input$person_to_compare) %>% 
      arrange(desc(time)) %>% 
      head(100)
    
    right_time <- sum(right$time, na.rm = TRUE)
    
    right <- right %>% 
      mutate(points = time / right_time * 100)
    
    merged_left <- left %>%
      left_join(right, by = "master_metadata_album_artist_name", suffix = c("_left", "_right")) 
    
    merged_right <- right %>%
      left_join(left, by = "master_metadata_album_artist_name", suffix = c("_left", "_right"))
    
    merged_data <- rbind(merged_left, merged_right) %>% 
      distinct(master_metadata_album_artist_name, .keep_all = TRUE) %>% 
      na.omit()
    
    merged_data <- merged_data %>% 
      mutate(score = max(points_left, points_right))
    
    total_sum <- round(sum(merged_data$score, na.rm = TRUE), digits = 0)
    
    progress_width <- paste0(total_sum, "%")
    
    tags$div(
      style = "text-align: center; font-family: 'Gotham';", 
      tags$p("Your compatibility:", style = "font-size: 24px; font-weight: bold"),
      tags$div(
        style = "display: flex; align-items: center; justify-content: center;",
        tags$div(
          style = "background-color: #b3b3b3; height: 20px; width: 300px; position: relative; border-radius: 10px;",  
          tags$div(
            id = "animated_bar",
            style = sprintf("background-color: #1db954; height: 100%%; width: %s; position: absolute; animation: progressAnimation 2s forwards; border-radius: 10px;", progress_width)  # Adjust size and border-radius as needed
          ),
          tags$style(HTML("
            @keyframes progressAnimation {
              0% { width: 0; }
              100% { width: 100%%; }
            }
          "))
        ),
        tags$span(
          style = "margin-left: 10px; font-size: 16px;",
          paste0(total_sum, "%")
        )
      ),
      h3(paste("You have" , nrow(merged_data), "mutual artists in your TOP 100")),
      h3("Artists that connected you:"),
      h4(as.character(merged_data[1, "master_metadata_album_artist_name"])),
      h4(as.character(merged_data[2, "master_metadata_album_artist_name"])),
      h4(as.character(merged_data[3, "master_metadata_album_artist_name"]))
    )
  })
  
  observeEvent(c(input$user, input$person_to_compare), {
    total_sum <- input$total_sum
    progress_width <- paste0(total_sum, "%")
    
    shinyjs::enable("animated_bar")
    
    updateProgressBar(session, "animated_bar", value = total_sum)
  })
  
  
  
  ############################   PLAYLIST   #################################
  
  observe({
    updateCheckboxGroupInput(session, "selected_people", selected = input$user)
  })
  
  output$song_list_output <- renderUI({
    
    playlist_data <- playlist %>%
      filter(Who %in% input$selected_people) %>%
      group_by(artistName, trackName) %>%
      summarise(
        people = n(),
        avg_count = round(mean(count)),
        avg_time = mean(time),
        image = coalesce(first(image), first(na.omit(image)), "default_value"),
        .groups = "drop"
      ) %>%
      arrange(-people, -avg_count, -avg_time)
    
    selected_songs <- playlist_data[1:input$song_count_slider, c("artistName", "trackName", "people", "avg_count", "image")]
    
    formatted_songs <- character()
    
    for (i in seq_along(selected_songs$trackName)) {
      song_div <- paste0(
        "<div class='song-item' data-toggle='tooltip' data-placement='top' title='Listened ",
        round(selected_songs$avg_count[i], 2),
        " times on average, by ",
        selected_songs$people[i],
        " people'>",
        "<div style='display: flex; align-items: center;'>",
        "<span style='font-weight: bold; margin-bottom: 3vh;'>", i, ".</span>",
        "<div style='margin-left: 2vh'></div>",
        "<img src='", selected_songs$image[i], "' style='width: 7.5vh; height: 7.5vh; margin-bottom: 3vh; margin-right: 1.5vh; border-radius: 5px;'>",
        "<div style='text-align: left; display: flex; flex-direction: column; justify-content: center; margin-bottom: 3vh;'>",
        paste(selected_songs$artistName[i], selected_songs$trackName[i], sep = " - "),
        "</div>",
        "</div>",
        "</div>"
      )
      formatted_songs <- c(formatted_songs, song_div)
    }
    
    formatted_songs <- paste(
      "<div style='font-family: Gotham, sans-serif; color: #FFFFFF; cursor: pointer; font-size: 18px;'>",
      formatted_songs,
      "</div>",
      collapse = "\n"
    )
    
    HTML(formatted_songs)
        
    
  })
  
  ####   Animacja   ####
  
  change_year <- function(year) {
    updateCheckboxInput(session, inputId = "year", value = year)
  }
  
  observeEvent(input$backward,{
    new <- case_when(
      input$user == 'Karolina' ~ 'Filip',
      input$user == 'Bartek' ~ 'Karolina',
      input$user == 'Filip' ~ 'Bartek')
    updateSliderTextInput(
      session,
      "user",
      selected = new
    )
  })
  
  observeEvent(input$forward,{
    new <- case_when(
      input$user == 'Karolina' ~ 'Bartek',
      input$user == 'Bartek' ~ 'Filip',
      input$user == 'Filip' ~ 'Karolina')
    updateSliderTextInput(
      session,
      "user",
      selected = new
    )
  })
  
  
  ####   Nagłówki, opisy ####
  
  output$wrapped_title <- renderUI({
    div(
      h1(
        paste0("Custom Wrapped for: ", input$user),
        class = "text-fav"),
      h3(
        paste0("from ", input$Months[1], "-", input$year, " to ", input$Months[2], '-', input$year),
        class = "text-fav",
        style = "margin-top:-5px;"))
  })
  
  
  output$compatibility_title <- renderUI({
    div(h1("Compatibility"),
        class = "text-fav")
  })
  
  output$radio_button_label <- renderUI({
    div(h2("Choose person to compare:"),
        style = "text-align: center; margin-bottom: 4vh; font-family: 'Gotham'; font-size: 8vh;")
  })
  
  
  output$playlist_title <- renderUI({
    div(h1("Playlist"),
        style = "text-align: left; font-family: 'Gotham', font-weight: bold;")
  })
  
  output$summary_title <- renderUI({
    div(h1("Summary"),
        class = "text-fav")
  })
  
  
  ####   Footer   ####
  
  output$left_caption <- renderUI({
    user_caption <- case_when(
      input$user == 'Karolina' ~ 'karo',
      input$user == 'Bartek' ~ 'PiwoToMojePaliwo',
      input$user == 'Filip' ~ 'FylypO')
    div(
      h4(paste0(user_caption), style = "height: 100%; margin: 5px;"),
      h5(paste0(input$user), style = "height: 100%; margin: 5px; color: #909090;")
    )
  })
  
  
  output$heart <- renderUI({
    tags$a(style = "z-index:100; cursor:pointer; margin-left: 10px; color: white; font-size: 2vh;",
           href = "https://www.youtube.com/watch?v=ISoCbWBqsYI",
           target="_blank",
           icon(id = "heart", "heart")
    ) 
  })
  
  output$our_cover <- renderUI({
    user_image <- case_when(
      input$user == 'Karolina' ~ 'https://i.pinimg.com/originals/7d/53/38/7d5338d2a67464493b745c9a417aebf7.jpg',
      input$user == 'Bartek' ~ 'https://media.npr.org/assets/img/2015/09/23/ap_836720500193-13f1674f764e5180cf9f3349cfef258d181f2b32-s1100-c50.jpg',
      input$user == 'Filip' ~ 'https://i1.sndcdn.com/artworks-6vjkU2fWoGjyNCCs-toATmQ-t500x500.jpg')
    img(
      src = user_image,
      class = "our-cover",
      height = "8.5vh",
      width = "8.5vh"
    )
  })
  
  
}


shinyApp(ui, server)

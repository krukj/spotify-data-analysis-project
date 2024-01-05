library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(shinydashboard)
library(shinyWidgets)
library(dtplyr)
library(data.table)

julka_data <- read.csv("julka_filtered_data.csv")
tomek_data <- read.csv("tomek_filtered_data.csv")

all_data <- bind_rows(julka_data, tomek_data)

styles_file <- includeCSS("styles.css")

css_sidebar <- HTML("#sidebarCollapsed{
 background-color:#182f37;
}")

css_logo <- HTML(".wrapper .main-header .logo{
 background-color:#385a65;
}
")



header <- dashboardHeader(
  title = "Spotify dashboard"
)

sidebar <- dashboardSidebar(
  tags$style(css_sidebar, css_logo),
  sidebarMenu(
    menuItem(" Blend", tabName = "blend", icon = icon("dog")),
    menuItem(" Spotify", tabName = "one_person", icon = icon("dog")),
    prettyRadioButtons(
      inputId = "person",
      label = "Choose person:", 
      choices = c("  Julka", "  Nadia", "  Tomek"),
      fill = TRUE,
      animation = NULL
    ),
    splitLayout(cellWidths = c("50%", "50%"),  
                dateInput("datefrom", "Date from:", format = "dd/mm/yy", 
                          Sys.Date()-30, min = "2015-10-21"), # ustawic dobre min i max !!!
                dateInput("dateto", "Date to:", format = "dd/mm/yy", 
                          Sys.Date()))
  )
)

body <- dashboardBody(
  tags$head(tags$style(styles_file
  )),
  tabItems(
    tabItem(tabName = "one_person",
            htmlOutput("name"),
            fluidRow(
              column(width = 6,
                     box(
                       width = 12,
                       htmlOutput("text_songs"),
                       tableOutput("table"),
                       plotlyOutput("repart_plot")
                     )
              ),
              
              column(width = 2,
                     box(
                       width = 12,
                       htmlOutput("text_artist")
                     ) 
              ),
              
              column(width = 4,
                     box(
                       width = 12,
                       htmlOutput("artist_image")
                     )
              ),
              column(width = 6,
                     plotlyOutput("density_plot",
                                  width = "100%",
                                  height = "300px")
              )
            )
    ),
    tabItem(tabName = "blend",
            htmlOutput("top"),
            fluidRow(
              column(width = 4,
                     box(
                       width = 12,
                       htmlOutput("choice"),
                       selectInput(
                         inputId = "select",
                         choices = c("Listening time", 
                                     "Average tempo",
                                     "Number of artists",
                                     "Average followers number",
                                     "Favourite genre"),
                         label = NULL
                       ),
                       tableOutput("rank")
                     )
              ),
              column(width = 4,
                     box(
                       width = 12,
                       htmlOutput("mutual_artist")
                     )),
              column(width = 4,
                     box(
                       width = 12,
                       htmlOutput("mutual_song")
                     )),
              column(width = 12,
                     plotlyOutput("minutes_plot")))
    )
  )
)

# app
ui <- dashboardPage(
  header,
  sidebar,
  body
  
)

server <- function(input, output, session) {
  updateTabItems(session, "blend", T)
  filtered_data <- reactive({
    datefrom <- input$datefrom
    dateto <- input$dateto
    
    data <- switch(input$person,
                   "  Tomek" = tomek_data,
                   "  Julka" = julka_data,
                   "  Nadia" = nadia_data) 
    
    data %>%
      lazy_dt() %>%
      filter(time > datefrom & time < dateto) %>%
      collect()
  })
  
  person_name <- reactive({
    input$person
  })
  
  output$name <- renderText(
    HTML(paste("<span style = 'color: #ffffff; 
               line-height: 1.3em; 
               text-transform: uppercase; 
               text-align: left;
               text-decoration:none;
               text-shadow:none;
               white-space:normal;
               letter-spacing:4.1px;
               word-spacing:-1.1px;
               column-count:1;
               direction:ltr;
               top:-8px;'</span>", person_name()))
  )
  
  output$text_songs <- renderText({
    HTML(paste(
      "<span style=
    'font-size: 15px;
    color:#ecf0f1; 
    font-size:26px;
    text-align:right;
    font-weight:600;
    position:relative;
    top:-4px;'>Top 10 songs</span><br>"))})
  
  output$table <- renderTable({
    filtered_data() %>%
      lazy_dt() %>%
      filter(ms_played > 30000) %>% 
      group_by(track_name, artist_name, album_name) %>% 
      summarise(
        minutes_listened = sum(ms_played) / (1000 * 60),
        songs_played = n()
      ) %>%
      arrange(desc(songs_played)) %>% 
      collect() %>%
      na.omit() %>%
      head(10) %>% 
      rename(
        "Track" = track_name,
        "Artist" = artist_name,
        "Album" = album_name,
        "Minutes" = minutes_listened,
        "Times" = songs_played
      )
  },
  width = "100%")
  
  
  output$repart_plot <- renderPlotly({
    
    listening_repartition <- filtered_data() %>%
      lazy_dt() %>%
      group_by(hour) %>% 
      summarise(minutes_listened = sum(ms_played) / (1000 * 60)) %>% 
      mutate(total_time = sum(minutes_listened),
             repartition = (minutes_listened / total_time) * 100) %>%
      collect()
    
    # nie wiem co to robi ale dziala
    vals <- lapply(listening_repartition$repartition, 
                   function(y) seq(0, y, by = 0.01))
    y <- unlist(vals)
    mid <- rep(listening_repartition$hour, lengths(vals))
    d2 <- data.frame(
      x = as.numeric(mid) - 0.4,  
      xend = as.numeric(mid) + 0.4,  
      y = y,
      yend = y
    )
    
    # ggplot
    gg <- ggplot(data = d2, aes(x = x + 1, xend = xend + 1, y = y, yend = yend,
                                color = y,
                                text = paste("</br><b>Hour: </b>", ceiling(x),
                                             "</br><b>Repartition: </b>", 
                                             round(y, digits = 1), "%"))) +
      geom_segment(linewidth = 2) +
      scale_color_gradient2(low = "#EF5571", mid = "#A386C0", high = "#00B9F1", 
                            midpoint = max(d2$y)/2) +
      guides(color = "none") +
      scale_x_discrete(limits = sprintf("%02d", 0:23))
    
    
    # ggplot to plotly
    p <- ggplotly(gg, tooltip = "text")
    
    # layout 
    p <- p %>%
      layout(
        title = list(text = "Listening repartition over day", 
                     y = 0.98, x = 0.5, xanchor = 'center', yanchor =  'top',
                     font = list(size = 15, color = "#ecf0f1")),
        xaxis = list(title = list(text = "Hour",
                                  font = list(size = 15, color = "#ecf0f1")), 
                     tickfont = list(size = 14, color = '#ecf0f1'),
                     showgrid = TRUE, gridcolor = '#385a65', gridwidth = 0.05),
        yaxis = list(title = list(text = "Repartition (%)", 
                                  font = list(size = 15, color = "#ecf0f1")), 
                     tickfont = list(size = 14, color = '#ecf0f1'),
                     showgrid = TRUE, gridcolor = '#385a65', gridwidth = 0.05),
        plot_bgcolor = "#182f37",
        paper_bgcolor = "#182f37",
        showlegend = FALSE,
        hoverlabel = list(font = list(color = '#ecf0f1'))
      )
    p
  })
  
  
  fav_artist_df <- reactive({
    filtered_data() %>% 
      lazy_dt() %>%
      group_by(artist_name, image, genre, followers) %>% 
      summarise(
        minutes_listened = round(sum(ms_played) / (1000 * 60)),
        songs_played = n()
      ) %>%
      arrange(desc(songs_played)) %>%
      collect() %>%
      na.omit() %>%
      head(1)
  })
  output$text_artist <- renderText({
    
    # favourite artist info
    fav_artist_name <- fav_artist_df()$artist_name
    minutes_played <- fav_artist_df()$minutes_listened
    songs_played <- fav_artist_df()$songs_played
    fav_artist_genre <- fav_artist_df()$genre
    fav_artist_followers <- fav_artist_df()$followers
    
    html_text <- paste(
      "<span style='font-size: 15px; color:#ecf0f1'>Favourite artist:
      </span><br>",
      "<span style='font-size: 30px; font-weight: bold; color:#ecf0f1'>", 
      fav_artist_name, "</span><br>",
      "<span style='font-size: 15px; color:#ecf0f1'>Minutes listened:
      </span><br>",
      "<span style='font-size: 30px; font-weight: bold; color:#ecf0f1'>", 
      minutes_played, "</span><br>",
      "<span style='font-size: 15px; color:#ecf0f1'>Songs played:</span><br>",
      "<span style='font-size: 30px; font-weight: bold; color:#ecf0f1'>", 
      songs_played, "</span><br>",
      "<span style='font-size: 15px; color:#ecf0f1'>Genre:</span><br>",
      "<span style='font-size: 30px; font-weight: bold; color:#ecf0f1'>", 
      fav_artist_genre, "</span>"
    )
    # text
    HTML(html_text)
  })
  
  output$artist_image <- renderText({
    img_src <- fav_artist_df()$image
    img_tag <- paste('<div style="text-align:center;">', 
                     '<img src="', img_src, '"', 
                     'width="300px" height="300px">', sep = "")
    return(img_tag)
  })
  
  
  output$density_plot <- renderPlotly({
    
    fav_artist_name <- fav_artist_df()[[1]]
    
    dens_plot <- filtered_data() %>%
      lazy_dt() %>%
      filter(artist_name == fav_artist_name) %>% 
      collect() %>%
      ggplot(aes(x = artist_name, y = tempo, text = 
                   paste("</br><b>Artist: </b>", fav_artist_name[[1]]))) +
      geom_violin(fill = "#1DB954", color = "#1DB954", size = 0.5) +
      coord_flip()
    
    dens_plot <- ggplotly(dens_plot, tooltip = "text") %>%
      layout(
        title = list(text = paste("Distribution of listened songs tempo for",
                                  fav_artist_name[[1]]),
                     y = 0.95, x = 0.5, xanchor = 'center', yanchor =  'top',
                     font = list(size = 15, color = "#ecf0f1")),
        hoverlabel = list(font = list(color = '#ecf0f1')),
        xaxis = list(title = list(text = "Tempo",
                                  font = list(size = 15, color = "#ecf0f1")), 
                     tickfont = list(size = 14, color = '#ecf0f1'),
                     showgrid = TRUE, gridcolor = '#385a65', gridwidth = 0.05),
        yaxis = list(title = list(text = ""), 
                     tickfont = list(size = 14, color = 'transparent'),
                     showgrid = TRUE, gridcolor = '#385a65', gridwidth = 0.05),
        plot_bgcolor = "#182f37",
        paper_bgcolor = "#182f37",
        showlegend = FALSE,
        hoverlabel = list(font = list(color = '#ecf0f1'))
      )
    
    dens_plot
  })
  
  
  # BLEND
  output$top <- renderText(
    HTML(paste("<span style = 'color: #ffffff; 
               line-height: 1.3em; 
               text-transform: uppercase; 
               text-align: left;
               text-decoration:none;
               text-shadow:none;
               white-space:normal;
               letter-spacing:4.1px;
               word-spacing:-1.1px;
               column-count:1;
               direction:ltr;
               top:-8px;' >BLEND</span><br>"))
  )
  
  output$choice <- renderText({
    HTML(paste(
      "<span style=
    'font-size: 15px;
    color:#ecf0f1; 
    font-size:26px;
    text-align:right;
    font-weight:600;
    position:relative;
    top:-4px;'>Compare by</span><br>"))})
  
  
  output$rank <- renderTable({
    if (input$select == "Listening time") {
      all_data %>% 
        group_by(username) %>% 
        summarise(minutes_listened = sum(ms_played) / (60 * 1000)) %>% 
        arrange(-minutes_listened) %>% 
        rename(
          "Name" = username,
          "Minutes listened" = minutes_listened
        )
    } else if (input$select == "Average tempo") {
      all_data %>% 
        group_by(username) %>% 
        summarise(average_tempo = mean(tempo, na.rm = T)) %>% 
        arrange(-average_tempo) %>% 
        rename(
          "Name" = username,
          "Average tempo" = average_tempo
        )
    } else if (input$select == "Number of artists") {
      all_data %>% 
        group_by(username) %>% 
        summarise(artists_number = n_distinct(artist_name)) %>% 
        arrange(-artists_number) %>% 
        rename(
          "Name" = username,
          "Number of unique artists" = artists_number
        )
    } else if (input$select == "Average followers number") {
      all_data %>%
        group_by(username, artist_name) %>%
        summarise(average_followers_n = mean(followers, na.rm = TRUE)) %>%
        group_by(username) %>%
        summarise(average_followers_n = 
                    as.integer(round(mean(average_followers_n, 
                                          na.rm = TRUE)))) %>% 
        arrange(-average_followers_n) %>% 
        rename(
          "Name" = username,
          "Average number of artist's followers" = average_followers_n
        )
    } else if (input$select == "Favourite genre") {
      all_data %>% 
        group_by(username) %>% 
        summarise(fav_genre = 
                    levels(factor(genre))[which.max(table(genre))]) %>% 
        rename(
          "Name" = username,
          "Favourite genre" = fav_genre
        )
    }
  },
  width = "100%",
  align = "c"
  )
  
  output$mutual_artist <- renderText({
    
    mutual_artist <- all_data %>% 
      group_by(artist_name) %>% 
      summarise(n_j = sum(username == "Julka"), n_t = sum(username == "Tomek")) %>% 
      filter(n_j > 100, n_t > 100) %>% # tymczasowe kryterium
      slice_head(n = 1)
    
    mutual_artist_name <- mutual_artist$artist_name
    artist_times <- mutual_artist$n_j + mutual_artist$n_t
    
    
    HTML(paste(
      "<span style='font-size: 15px; color:#ecf0f1'>Mutual artist:
      </span><br>",
      "<span style='font-size: 30px; font-weight: bold; color:#ecf0f1'>", 
      mutual_artist_name, "</span><br>",
      "<span style='font-size: 15px; color:#ecf0f1'>Together we listened to this artist:
      </span><br>",
      "<span style='font-size: 30px; font-weight: bold; color:#ecf0f1'>", 
      artist_times, "</span>",
      "<span style='font-size: 15px; color:#ecf0f1'>times</span><br>"
    ))})
  
  output$mutual_song <- renderText({
    
    mutual_song <- all_data %>% 
      group_by(artist_name, track_name) %>% 
      summarise(n_j = sum(username == "Julka"), n_t = sum(username == "Tomek")) %>% 
      filter(n_j > 50, n_t > 50) %>% # tymczasowe kryterium
      slice_head(n = 1)
    
    mutual_song <- mutual_song$track_name
    song_times <- mutual_songs$n_j + mutual_songs$n_t
    
    HTML(paste(
      "<span style='font-size: 15px; color:#ecf0f1'>Mutual song:</span><br>",
      "<span style='font-size: 30px; font-weight: bold; color:#ecf0f1'>", 
      mutual_song, "</span><br>",
      "<span style='font-size: 15px; color:#ecf0f1'>Together we played this song :</span><br>",
      "<span style='font-size: 30px; font-weight: bold; color:#ecf0f1'>", 
      song_times, "</span>",
      "<span style='font-size: 15px; color:#ecf0f1'>times</span><br>"
    ))})
  
  
  output$minutes_plot <- renderPlotly({
    
    df1 <- julka_data %>% 
      mutate(date = as.Date(time, format = "%Y-%m-%d")) 
    
    df2 <- tomek_data %>% 
      mutate(date = as.Date(time, format = "%Y-%m-%d")) %>% 
      filter(date > "2019-01-01") # bo byla tylko jakas jedna wartosc przed 2020
    
    df1$date <- as.Date(df1$date)
    df2$date <- as.Date(df2$date)
    
    minutes_per_day_j <- df1 %>%
      group_by(date) %>%
      summarise(total_minutes = sum(ms_played) / (60 * 1000)) %>%
      arrange(date) %>%
      mutate(cumsum = cumsum(total_minutes)) %>% 
      mutate(name = "Julka")
    
    
    minutes_per_day_t <- df2 %>% 
      group_by(date) %>%
      summarise(total_minutes = sum(ms_played) / (60 * 1000)) %>%
      arrange(date) %>%
      mutate(cumsum = cumsum(total_minutes)) %>% 
      mutate(name = "Tomek")
    
    df <- bind_rows(minutes_per_day_j, minutes_per_day_t)
    
    
    minutes_plotly <- plot_ly(df, x = ~date, y = ~cumsum, color = ~name, 
                              hoverinfo ="text",
                              text = ~paste("</br><b>Name:</b>", name, 
                                            "</br><b>Date:</b> ", date, 
                                            "</br><b>Minutes:</b> ", round(cumsum)),
                              type = "scatter", mode = "lines",
                              colors = c("#EF5571", "#A386C0")) %>%
      layout(
        title = list(text = "Total minutes listened", 
                     y = 0.98, x = 0.5, xanchor = "center", yanchor =  "top",
                     font = list(size = 15, color = "#ecf0f1")),
        xaxis = list(title = list(text = "Date",
                                  font = list(size = 15, color = "#ecf0f1")), 
                     tickfont = list(size = 14, color = '#ecf0f1'),
                     showgrid = TRUE, gridcolor = '#385a65', gridwidth = 0.05,
                     rangeslider = list(type = "date")),
        yaxis = list(title = list(text = "Minutes", 
                                  font = list(size = 15, color = "#ecf0f1")), 
                     tickfont = list(size = 14, color = '#ecf0f1'),
                     showgrid = TRUE, gridcolor = '#385a65', gridwidth = 0.05),
        legend = list(
          title = list(text = "Person", font = list(color = "#ecf0f1")),
          font = list(size = 12, color = "#ecf0f1"),
          bgcolor = "#182f37"
        ),
        plot_bgcolor = "#182f37",
        paper_bgcolor = "#182f37",
        hoverlabel = list(font = list(color = '#ecf0f1'))
      )
    
    minutes_plotly
  })
  
}



shinyApp(ui, server)      

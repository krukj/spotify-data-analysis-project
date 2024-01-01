library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(shinydashboard)
library(shinyWidgets)
library(dtplyr)
library(data.table)

julka_data <- read.csv("julka_data_extended.csv")
tomek_data <- read.csv("tomek_data_extended.csv")

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
    menuItem(" Spotify", tabName = "one_person", icon = icon("dog")),
    menuItem(" Blend", tabName = "blend", icon = icon("dog")),
    prettyRadioButtons(
      inputId = "person",
      label = "Choose person:", 
      choices = c("  Julka", "  Nadia", "  Tomek"),
      fill = TRUE,
      animation = NULL
    ),
    splitLayout(cellWidths = c("50%", "50%"),
                dateInput("datefrom", "Date from:", format = "dd/mm/yy", 
                          Sys.Date()-30, min = "2015-10-21"),
                dateInput("dateto", "Date to:", format = "dd/mm/yy", 
                          Sys.Date()))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "one_person",
            tags$head(tags$style(styles_file
            )
            ),
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
                     plotlyOutput("density_plot")
              )
            )
    ),
    tabItem(tabName = "blend",
            fluidRow(
              column(width = 6,
                     box(
                       width = 12,
                       selectInput(
                         inputId = "select",
                         choices = c("Listening time", "Average tempo"),
                         label = "TOP"
                       ),
                       tableOutput("rank")
                     )
              ))
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
  })
  
  
  output$repart_plot <- renderPlotly({
    
    data_with_hour <- filtered_data()
    data_with_hour$hour <- format(as.POSIXlt(data_with_hour$time), "%H")
    
    listening_repartition <- data_with_hour %>%
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
    img_tag <- paste('<img src="', img_src, '"', 
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
  
  output$rank <- renderTable({
    if (input$select == "Listening time") {
      
    }
    
  })
  
}



shinyApp(ui, server)      

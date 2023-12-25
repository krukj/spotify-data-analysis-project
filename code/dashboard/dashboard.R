library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(shinydashboard)


julka_data <- read.csv("julka_extended.csv") 


# css
html_wrapper <- HTML(".wrapper .row{
    background-color:#182f37;}")
html_table <-  HTML(".shiny-table tr th{
 color:#ecf0f1;
}")
html_table_data <- HTML(".shiny-table tr td{
 color:#ecf0f1;
 background-color:#182f37;
}")
html_row <- HTML(".wrapper .row{
 background-color:#182f37;
 transform:translatex(0px) translatey(0px);
}")

html_navig <- HTML(".wrapper .main-header nav{
 background-color:#385a65 !important;
}")
html_sidebar <- HTML("#sidebarCollapsed{
 background-color:#182f37;
}")
html_logo <- HTML(".wrapper .main-header .logo{
 background-color:#385a65;
}")
html_content_div <- HTML(".wrapper .content{
 transform:translatex(0px) translatey(0px);
 background-color:#182f37;
}")

html_box_body <- HTML(".wrapper .box .box-body{
 background-color:#182f37;
}")

html_dens_plot <- HTML("#density_plot{
 transform:translatex(0px) translatey(0px);
 position:relative;
 top:53px;
}")

# app
ui <- dashboardPage(
  dashboardHeader(title = "Spotify dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem(" Julka", tabName = "dashboard", icon = icon("dog")),
      menuItem(" Nadia", tabName = "dashboard", icon = icon("cat")),
      menuItem(" Tomek", tabName = "dashboard", icon = icon("dog")),
      splitLayout(cellWidths = c("50%", "50%"),
                  dateInput("datefrom", "Date From:", format = "dd/mm/yy", 
                            Sys.Date()-30, min = "2015-10-21"),
                  dateInput("dateto", "Date To:", format = "dd/mm/yy", Sys.Date()))
    )
  ),
  
  dashboardBody(
    tags$head(tags$style(
      html_wrapper, html_table, html_table_data, html_row, html_navig, html_sidebar,
      html_logo, html_content_div, html_box_body, html_dens_plot
    )
    ),
    fluidRow(
      # left column
      box(
        htmlOutput("text_songs"),
        tableOutput("table"),
        plotlyOutput("repart_plot")
      ),
      # right column
      box(
        htmlOutput("text_artist"),
        plotlyOutput("density_plot")
      )
    )
  )
)

server <- function(input, output) { 
  
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
    julka_data %>% 
      filter(time > input$datefrom & time < input$dateto) %>% 
      group_by(track_name, artist_name, album_name) %>%
      summarise(
        minutes_listened = sum(ms_played) / (1000 * 60),
        songs_played = n()
      ) %>%
      arrange(desc(songs_played)) %>%
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
    
    data_with_hour <- julka_data
    data_with_hour$hour <- format(as.POSIXlt(data_with_hour$time), "%H")
    
    listening_repartition <- data_with_hour %>% 
      filter(time > input$datefrom & time < input$dateto) %>%
      group_by(hour) %>% 
      summarise(minutes_listened = sum(ms_played) / (1000 * 60)) %>% 
      mutate(total_time = sum(minutes_listened),
             repartition = (minutes_listened / total_time) * 100)
    
    # nie wiem co to robi ale dziala
    vals <- lapply(listening_repartition$repartition, function(y) seq(0, y, by = 0.01))
    y <- unlist(vals)
    mid <- rep(listening_repartition$hour, lengths(vals))
    d2 <- data.frame(
      x = as.numeric(mid) - 0.4,  
      xend = as.numeric(mid) + 0.4,  
      y = y,
      yend = y
    )
    
    # ggplot
    gg <- ggplot(data = d2, aes(x = x, xend = xend, y = y, yend = yend,
                                color = y,
                                text = paste("</br><b>Hour: </b>", ceiling(x),
                                             "</br><b>Repartition: </b>", 
                                             round(y, digits = 1), "%"))) +
      geom_segment(linewidth = 2) +
      scale_color_gradient2(low = "#EF5571", mid = "#A386C0", high = "#00B9F1", 
                            midpoint = max(d2$y)/2) +
      guides(color = "none")
    
    # ggplot to plotly
    p <- ggplotly(gg, tooltip = "text")
    
    # layout 
    p <- p %>%
      layout(
        title = list(text = "Listening repartition over day", 
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
    
    # print the plot
    p
  })
  
  
  output$text_artist <- renderText({
    
    # favourite artist info
    fav_artist_df <- julka_data %>% 
      filter(time > input$datefrom & time < input$dateto) %>%
      group_by(artist_name) %>% 
      summarise(
        minutes_listened = round(sum(ms_played) / (1000 * 60)),
        songs_played = n()
      ) %>%
      arrange(desc(songs_played)) %>%
      na.omit() %>%
      head(1)
    fav_artist_name <- fav_artist_df[[1]]
    minutes_played <- fav_artist_df[2]
    songs_played <- fav_artist_df[3]
    
    # text
    HTML(paste(
      "<span style=
      'font-size: 15px; 
      color:#ecf0f1'>
      Favourite artist:</span><br>",
      
      "<span style=
      'font-size: 30px; 
      font-weight: bold; 
      color:#ecf0f1'>", 
      fav_artist_name,
      "</span><br>",
      
      "<span style=
      'font-size: 15px; 
      color:#ecf0f1'>
      Minutes played:</span><br>",
      "<span style=
      'font-size: 30px; 
      font-weight: bold; 
      color:#ecf0f1'>",
      minutes_played, 
      "</span><br>",
      
      "<span style='
      font-size: 15px;
      color:#ecf0f1'>
      Songs played:
      </span><br>",
      
      "<span style=
      'font-size: 30px; 
      font-weight: bold; 
      color:#ecf0f1'>", 
      songs_played, 
      "</span>"
    ))
  })
  
  output$density_plot <- renderPlotly({
    
    fav_artist_df <- julka_data %>% 
      filter(time > input$datefrom & time < input$dateto) %>%
      group_by(artist_name) %>% 
      summarise(
        minutes_listened = round(sum(ms_played) / (1000 * 60)),
        songs_played = n()
      ) %>%
      arrange(desc(songs_played)) %>%
      na.omit() %>%
      head(1)
    fav_artist_name <- fav_artist_df[[1]]
    
    dens_plot <- julka_data %>%
      filter(artist_name == fav_artist_name) %>% 
      ggplot(aes(x = artist_name, y = tempo, text = 
                   paste("</br><b>Artist: </b>", fav_artist_name[[1]],
                         "</br><b>Tempo: </b>"))) +
      geom_violin(fill = "#1DB954", color = "#1DB954", size = 0.5) +
      coord_flip()
    
    dens_plot <- ggplotly(dens_plot, tooltip = "text") %>%
      layout(
        title = list(text = paste("Distribution of listened songs tempo for",
                                  fav_artist_name[[1]]),
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
}

shinyApp(ui, server)
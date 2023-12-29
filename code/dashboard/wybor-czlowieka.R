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



header <- dashboardHeader(
  title = "Spotify dashboard"
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(" Julka", tabName = "julka", icon = icon("dog")),
    menuItem(" Nadia", tabName = "nadia", icon = icon("cat")),
    menuItem(" Tomek", tabName = "tomek", icon = icon("dog"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "julka",
            tags$head(tags$style(
              html_wrapper, html_table, html_table_data, html_row, html_navig, html_sidebar,
              html_logo, html_content_div, html_box_body
            )
            ),
            
            h2("Julka",
               style = "color: #ffffff; 
               line-height: 1.3em; 
               text-transform: uppercase; 
               text-align: left;
               text-decoration:none;
               text-shadow:none;
               white-space:normal;
               letter-spacing:4.1px;
               word-spacing:-1.1px;
               column-count:1;
               direction:ltr;"),
            
            fluidRow(
              # left column
              box(
                htmlOutput("text_songs_julka"),
                tableOutput("table_julka"),
                plotlyOutput("repart_plot_julka")
              ),
              # right column
              box(
                htmlOutput("text_artist_julka"),
                plotlyOutput("density_plot_julka")
              )
            )
    ),
    
    tabItem(tabName = "nadia",
            h2("Nadia",
               style = "color: #ffffff; 
               line-height: 1.3em; 
               text-transform: uppercase; 
               text-align: left;
               text-decoration:none;
               text-shadow:none;
               white-space:normal;
               letter-spacing:4.1px;
               word-spacing:-1.1px;
               column-count:1;
               direction:ltr;")
    ),
    
    tabItem(tabName = "tomek",
            h2("Tomek",
               style = "color: #ffffff; 
               line-height: 1.3em; 
               text-transform: uppercase; 
               text-align: left;
               text-decoration:none;
               text-shadow:none;
               white-space:normal;
               letter-spacing:4.1px;
               word-spacing:-1.1px;
               column-count:1;
               direction:ltr;")
    )
  )
)

# app
ui <- dashboardPage(
  header,
  sidebar,
  body
  
)

server <- function(input, output) {
  
  # JULKA <3
  
  output$text_songs_julka <- renderText({
    HTML(paste(
      "<span style=
    'font-size: 15px;
    color:#ecf0f1; 
    font-size:26px;
    text-align:right;
    font-weight:600;
    position:relative;
    top:-4px;'>Top 10 songs</span><br>"))})
  
  output$table_julka <- renderTable({
    julka_data %>% 
      # filter time ...
      group_by(track_name, artist_name, album_name) %>%
      summarise(
        minutes_listened = sum(ms_played) / (1000 * 60),
        songs_played = n()
      ) %>%
      arrange(desc(songs_played)) %>%
      na.omit() %>%
      head(10)
  })
  
  output$repart_plot_julka <- renderPlotly({
    
    data_with_hour <- julka_data
    data_with_hour$time <- strptime(data_with_hour$time, 
                                    format ="%Y-%m-%d %H:%M:%S")
    data_with_hour$hour <- format(data_with_hour$time, "%H")
    
    
    listening_repartition <- data_with_hour %>% 
      # filter time ...
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
                     showgrid = TRUE, gridcolor = '#ecf0f1', gridwidth = 0.05),
        yaxis = list(title = list(text = "Repartition (%)", 
                                  font = list(size = 15, color = "#ecf0f1")), 
                     tickfont = list(size = 14, color = '#ecf0f1'),
                     showgrid = TRUE, gridcolor = '#ecf0f1', gridwidth = 0.05),
        plot_bgcolor = "#182f37",
        paper_bgcolor = "#182f37",
        showlegend = FALSE,
        hoverlabel = list(font = list(color = '#ecf0f1'))
      )
    
    # print the plot
    p
  })
  
  # favourite artist info
  julka_fav_artist_df <- julka_data %>% 
    # filter time ...
    group_by(artist_name) %>% 
    summarise(
      minutes_listened = round(sum(ms_played) / (1000 * 60)),
      songs_played = n()
    ) %>%
    arrange(desc(songs_played)) %>%
    na.omit() %>%
    head(1)
  julka_fav_artist_name <- julka_fav_artist_df[[1]]
  julka_minutes_played <- julka_fav_artist_df[2]
  julka_songs_played <- julka_fav_artist_df[3]
  
  output$text_artist_julka <- renderText({
    HTML(paste(
      "<span style=
      'font-size: 15px; 
      color:#ecf0f1'>
      Favourite artist:</span><br>",
      
      "<span style=
      'font-size: 30px; 
      font-weight: bold; 
      color:#ecf0f1'>", 
      julka_fav_artist_name,
      "</span><br>",
      
      "<span style=
      'font-size: 15px; 
      color:#ecf0f1'>
      Minutes played:</span><br>",
      "<span style=
      'font-size: 30px; 
      font-weight: bold; 
      color:#ecf0f1'>",
      julka_minutes_played, 
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
      julka_songs_played, 
      "</span>"
    ))
  })
  
  output$density_plot_julka <- renderPlotly({
    julka_data %>%
      filter(artist_name == julka_fav_artist_name[[1]]) %>% 
      ggplot(aes(x = artist_name, y = tempo)) +
      geom_violin(fill = "#1DB954", color = "#1DB954", size = 0.5) +
      scale_y_continuous(limits = c(50, 210), expand = c(0, 0)) +
      coord_flip() +
      labs(x = "artist",
           y = "tempo",
           title = paste("Distribution of listened songs tempo for", julka_fav_artist_name[[1]])) +
      theme(
        plot.title = element_text(size = 15, hjust = 0.5, color = '#ecf0f1'),
        panel.background = element_rect(fill = "#182f37"),
        plot.background = element_rect(fill = "#182f37"), 
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 10, color = '#ecf0f1'),
        axis.title.x = element_text(size = 10, color = '#ecf0f1'),
        axis.title.y = element_blank(),
        panel.border = element_rect(color = "#ecf0f1", fill = NA, size = 0.5),
        panel.grid.major = element_line(color = "#ecf0f1", size = 0.1),
        panel.grid.minor = element_line(color = "#ecf0f1", size = 0.1)
      ) -> dens_plot
    dens_plot <- ggplotly(dens_plot)
    dens_plot
  })
 
}



shinyApp(ui, server)      
 
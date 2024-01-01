julka_data <- read.csv("julka_data_extended.csv")
tomek_data <- read.csv("tomek_data_extended.csv")

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




# na wszelki wypadek
# minutes_per_month_j <- df1 %>% 
#   group_by(month = format(date, "%Y-%m"), username) %>% 
#   summarise(total_minutes = sum(ms_played) / (60 * 1000)) 
# 
# minutes_per_month_t <- df2 %>% 
#   group_by(month = format(date, "%Y-%m"), username) %>% 
#   summarise(total_minutes = sum(ms_played) / (60 * 1000))
# 
# df <- bind_rows(minutes_per_month_j, minutes_per_month_t)
# 
# minutes <- ggplot(df, aes(x = month, y = total_minutes, group = username,
#                           color = username,
#                           text = paste("</br><b>Name:</b>", 
#                                        username, "</br><b>Month:</b> ",
#                                        month, "</br><b>Minutes:</b> ", 
#                                        round(total_minutes)))) +
#   geom_line() +
#   scale_color_manual(values = c("#EF5571", "#A386C0"))
# 
# minutes_plotly <- ggplotly(minutes, tooltip = "text")
# 
# minutes_plotly %>% 
#   layout(
#     title = list(text = "Minutes listened per month", 
#                  y = 0.98, x = 0.5, xanchor = 'center', yanchor =  'top',
#                  font = list(size = 15, color = "#ecf0f1")),
#     xaxis = list(title = list(text = "Month",
#                               font = list(size = 15, color = "#ecf0f1")), 
#                  tickfont = list(size = 14, color = '#ecf0f1'),
#                  showgrid = TRUE, gridcolor = '#385a65', gridwidth = 0.05),
#     yaxis = list(title = list(text = "Minutes", 
#                               font = list(size = 15, color = "#ecf0f1")), 
#                  tickfont = list(size = 14, color = '#ecf0f1'),
#                  showgrid = TRUE, gridcolor = '#385a65', gridwidth = 0.05),
#     legend = list(
#       title = list(text = "Name", font = list(color = "#ecf0f1")),
#       font = list(size = 12, color = "#ecf0f1"),
#       bgcolor = "#182f37"
#     ),
#     plot_bgcolor = "#182f37",
#     paper_bgcolor = "#182f37",
#     hoverlabel = list(font = list(color = '#ecf0f1')
#     )) %>% 
#   layout(xaxis = list(rangeslider = list(
#     type = "date")))
# 
# 
# 

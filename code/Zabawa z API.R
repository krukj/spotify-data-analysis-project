library(httr)


client_id <- "aa337ae22ca74c18b67667dfa9e50403"
client_secret <- "dce7d9eed7e247a8bcbb0fb18d8fcae3"

token_url <- "https://accounts.spotify.com/api/token"
token_request <- POST(
  token_url,
  body = list(
    grant_type = "client_credentials"
  ),
  authenticate(client_id, client_secret),
  encode = "form"
)
token_response <- content(token_request)
access_token <- token_response$access_token


################################################################################

get_and_update_track_features <- function(df, row_index, access_token) {
  error_flag = FALSE
  track_uri <- df$spotify_track_uri[row_index]
  
  audio_features_url <- paste0("https://api.spotify.com/v1/audio-features/",
                               gsub("spotify:track:", "", track_uri))
  audio_features_response <- GET(
    audio_features_url,
    add_headers(Authorization = paste("Bearer", access_token))
  )
  response_status <- audio_features_response$status_code
  if (response_status == 429) {
    error_flag <- TRUE
    cat("Error 429: Too many request.")
    return(list(audio_features_data, error_flag))
  }
 
  audio_features_data <- fromJSON(rawToChar(audio_features_response$content))
  
  return(list(audio_features_data, error_flag))
}
################################################################################

tomek_ext %>%
  distinct(master_metadata_track_name, .keep_all = TRUE) -> tracks_ext

################################################################################

count = 1
for (i in 1:nrow(tracks_ext)) { ## zmieniać zakres jak wywali error 429
  get_and_update_track_features(tracks_ext, i, access_token) -> wynik
  dane <- wynik[[1]]
  flag <- wynik[[2]]
  
  if (flag) {
    cat(" Liczba iteracji:", count)
    break
  }
  
  features_list <- c("danceability", "energy", "key", "loudness", "mode", "speechiness", 
                     "acousticness", "instrumentalness", "liveness", "valence", "tempo")
  
  for (feature in features_list) {
    if (!is.null(dane[[feature]])) {
      tracks_ext[[feature]][i] <- dane[[feature]]
    } else {
      tracks_ext[[feature]][i] <- NA
    }
  }
  
  count = count + 1
  if (count %% 100 == 0) {
    cat("Sleep. Liczba iteracji: ", count, "\n")
    Sys.sleep(10)
  }
}

################################################################################

tomek_extended <- left_join(tomek_ext,
                            tracks_ext[, c(
                              "master_metadata_track_name",
                              "danceability",
                              "energy",
                              "key",
                              "loudness",
                              "mode",
                              "speechiness",
                              "acousticness",
                              "instrumentalness",
                              "liveness",
                              "valence",
                              "tempo"
                            )],
                            by = "master_metadata_track_name")

write.csv(tomek_extended, "tomek_extended.csv", row.names = FALSE)

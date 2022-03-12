# Custom function based on 'get_track_audio_analysis' from the package 'spotifyr'
# to retrieve the confidence of tempo, time signature, key and mode features
get_track_audio_analysis_confidence <- function (url, authorization = get_spotify_access_token()) {
    
    params <- list(access_token = authorization)
    
    res <- httr::RETRY("GET", url, query = params, encode = "json")
    
    if (is.character(httr::warn_for_status(res))) {
        
        return(tibble(tempo_confidence = NA, time_signature_confidence = NA,
                      key_confidence = NA, mode_confidence = NA))
        
    } else {
        
        res <- jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8"),
                                  flatten = TRUE)
        
        if (is.null(res$track)) {
            res <- as_tibble(res) %>%
                select(tempo_confidence, time_signature_confidence, key_confidence,
                       mode_confidence)
        } else {
            res <- as_tibble(res$track) %>%
                select(tempo_confidence, time_signature_confidence, key_confidence,
                       mode_confidence)
        }
    }
}
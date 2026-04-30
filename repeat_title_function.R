# takes in a csv file name
# returns a count of how many times the title of a song repeats in the lyrics
repeat.title <- function(csv) {
  
  # read the csv file
  hot100.top10 <- read_csv(csv)
  
  # ensure columns exist
  required.cols <- c("year", "rank", "title", "artist", 
                     "wiki.url", "songlyrics.url", "lyrics")
  missing.cols <- setdiff(required.cols, names(hot100.top10))
  if (length(missing.cols) > 0) {
    stop(paste("Missing required columns:", paste(missing.cols, collapse = ", ")))
  }
  
  # helper function to clean and standardize text, make lowercase and remove punctuation
  clean.text <- function(x) {
    x |> tolower() |> 
      gsub("[[:punct:]]", " ", x = _) |> 
      gsub("\\s+", " ", x = _) |> 
      trimws()
  }
  
  # count occurrences for each row, return 0 if no lyrics found
  hot100.top10$title.repeats <- mapply(function(title, lyrics) {
    if (is.na(lyrics) || lyrics == "") return(0)
    
    # use helper function to clean title and lyrics
    title.clean  <- clean.text(title)
    lyrics.clean <- clean.text(lyrics)
    
    # count occurrences of title in lyrics
    length(gregexpr(title.clean, lyrics.clean, fixed = TRUE)[[1]])
  }, hot100.top10$title, hot100.top10$lyrics)
  
  # remove titles that do not repeat or only say it once
  hot100.top10 <- hot100.top10 |> filter(title.repeats > 1)
  
  return(hot100.top10)
}

repeat.title("hot100_top10.csv")
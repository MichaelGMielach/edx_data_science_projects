#Dataset preparation:

    library(tidyverse)
    library(caret)
    
    # MovieLens 10M dataset:
    # https://grouplens.org/datasets/movielens/10m/
    # http://files.grouplens.org/datasets/movielens/ml-10m.zip
    
    options(timeout = 120)
    
    dl <- "ml-10M100K.zip"
    download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
      
    ratings_file <- "ml-10M100K/ratings.dat"
    if(!file.exists(ratings_file))
      unzip(dl, ratings_file)
    
    movies_file <- "ml-10M100K/movies.dat"
    if(!file.exists(movies_file))
      unzip(dl, movies_file)
    
    ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),stringsAsFactors = FALSE)
    colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
    ratings <- ratings %>%
      mutate(userId = as.integer(userId),
             movieId = as.integer(movieId),
             rating = as.numeric(rating),
             timestamp = as.integer(timestamp))
    
    movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                            stringsAsFactors = FALSE)
    colnames(movies) <- c("movieId", "title", "genres")
    movies <- movies %>%
      mutate(movieId = as.integer(movieId))
    
    movielens <- left_join(ratings, movies, by = "movieId")
    
    # Final hold-out test set will be 10% of MovieLens data
    set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
    # set.seed(1) # if using R 3.5 or earlier
    test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
    edx <- movielens[-test_index,]
    temp <- movielens[test_index,]
    
    # Make sure userId and movieId in final hold-out test set are also in edx set
    final_holdout_test <- temp %>% 
      semi_join(edx, by = "movieId") %>%
      semi_join(edx, by = "userId")
    
    # Add rows removed from final hold-out test set back into edx set
    removed <- anti_join(temp, final_holdout_test)
    edx <- rbind(edx, removed)
    
    rm(dl, ratings, movies, test_index, temp, movielens, removed)


#Questions:
  #Q1: 
    dim(edx)
  #Q2:
    edx %>% filter(rating == 0) %>% tally()
    edx %>% filter(edx$rating == 3) %>% summarize(n=n())
  #Q3:
    edx %>% group_by(movieId) %>% tally()
    n_distinct(edx$movieId)
  #Q4:    
    n_distinct(edx$userId)
  #Q5:
    library(stringr)
    sum(str_detect(edx$genres,"Drama",negate = FALSE))
    sum(str_detect(edx$genres,"Comedy",negate = FALSE))
    sum(str_detect(edx$genres,"Thriller",negate = FALSE))
    
    # str_detect
    genres = c("Drama", "Comedy", "Thriller", "Romance")
    sapply(genres, function(g) {
      sum(str_detect(edx$genres, g))
    })
    
    # separate_rows, much slower!
    edx %>% separate_rows(genres, sep = "\\|") %>%
      group_by(genres) %>%
      summarize(count = n()) %>%
      arrange(desc(count))
#Q6:
    edx %>% group_by(movieId, title) %>%
      summarize(count = n()) %>%
      arrange(desc(count))

#Q7:
    edx %>% group_by(rating) %>%
      summarize(count = n()) %>%
      top_n(5) %>%
      arrange(desc(count))
    
#Q8:
    edx %>%
      group_by(rating) %>%
      summarize(count = n()) %>%
      ggplot(aes(x = rating, y = count)) +
      geom_line()
    

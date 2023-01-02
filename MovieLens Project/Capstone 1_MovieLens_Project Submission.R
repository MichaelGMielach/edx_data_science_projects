# 1) Dataset preparation -----------------------------------------------------

    #load packages & data    
    
        library(tidyverse)
        library(caret)
        library(lubridate)
        options(timeout = 120)
    
        dl <- "ml-10M100K.zip"
        download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
        
        ratings_file <- "ml-10M100K/ratings.dat"
        if(!file.exists(ratings_file))
          unzip(dl, ratings_file)
        
        movies_file <- "ml-10M100K/movies.dat"
        if(!file.exists(movies_file))
          unzip(dl, movies_file)
        
    # Create Movielens data frame
    
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
    
    
    # Create final hold-out/validation test set
        
        # Slice 10% out of MovieLens data to create final hold-out/validation test set
        
            set.seed(1, sample.kind="Rounding")
       
            test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
            edx <- movielens[-test_index,]
            temp <- movielens[test_index,]
        
            final_holdout_test <- temp %>% 
              semi_join(edx, by = "movieId") %>%
              semi_join(edx, by = "userId")
            
        # Add rows removed from final hold-out/validation test set back into edx set
        
          removed <- anti_join(temp, final_holdout_test)
          edx <- rbind(edx, removed)
      
        # Clean up data environment
          
          rm(dl, ratings, movies, test_index, temp, movielens, removed)

        # Check for any NA value
          
          anyNA(edx)
    


# 2) Exploratory Data Analysis --------------------------------------------

    #Analysis of the data structure (test & validation set):
          
        summary(edx)
        str(edx)
        
        summary(final_holdout_test)
        str(final_holdout_test)
        
        edx %>% summarize(unique_users = length(unique(userId)),
                          unique_movies = length(unique(movieId)),
                          unique_genres = length(unique(genres)),
                          unique_rating = length(unique(rating)))
    
    #Analysing rating data
    
        range(edx$rating)
    
        edx %>% group_by(rating) %>% summarize(count = n()) %>% top_n(5) %>%
          arrange(desc(count)) 
  
        edx %>%  
          ggplot(aes(x= rating)) +
          geom_histogram(bins = 10, binwidth = 0.2) +
          scale_x_continuous(breaks=seq(0, 5, by= 0.5)) +
          labs(x="rating category", y="ratings given", caption = "source data: MovieLens 10M Dataset") +
          ggtitle("Count of ratings per category")
    
    # histogram of number of ratings by movieId
    
        edx %>% 
          count(movieId) %>% 
          ggplot(aes(n)) + 
          geom_histogram(bins = 30, binwidth=0.2, color="black", ) + 
          scale_x_log10() + 
          labs(x="n", y="number of ratings", caption = "source data: MovieLens 10M Dataset") +
          ggtitle("Number of ratings per movie")

    # histogram of number of ratings by userId
    
        edx %>% 
          count(userId) %>% 
          ggplot(aes(n)) + 
          geom_histogram(bins = 30, binwidth=0.2, color="black", ) + 
          scale_x_log10() + 
          labs(x="n", y="number of ratings", caption = "source data: MovieLens 10M Dataset") +
          ggtitle("Number of ratings per user")
    
    
    #inspection of titles
    
        edx %>%
          group_by(title) %>%
          summarize(count=n(), rating = mean(rating)) %>%
          top_n(10,count) %>%
          arrange(desc(count))
        
    #inspection of genre
    
        edx %>% group_by(genres) %>%
          summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
          filter(n >= 100000) %>% 
          mutate(genres = reorder(genres, avg)) %>%
          ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
          geom_point() +
          geom_errorbar() + 
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          labs(title = "Error bar plots by genres" , caption = "source data : MovieLens 10M Dataset")
        
        
        edx %>% separate_rows(genres, sep = "\\|") %>%
          group_by(genres) %>%
          summarize(count = n()) %>%
          arrange(desc(count))
  
    #inspection of time effects
    
        edx %>% 
          mutate(date = round_date(as_datetime(timestamp), unit = "week")) %>%
          group_by(date) %>%
          summarize(rating = mean(rating)) %>%
          ggplot(aes(date, rating)) +
          geom_point() +
          geom_smooth() +
          ggtitle("Ratings over time")+
          labs(caption = "source data : MovieLens 10M Dataset")
    
# 3) Prediction Modelling --------------------------------------------
    
    #Model 1 (Baseline Model):
    
        mu <- mean(edx$rating)
        model_baseline_rmse <- RMSE(mu,final_holdout_test$rating)
    
        
    #Model 2 (Multivariate Regression Models):
        
    #Model 2a (Movie and user effects):
    
        #Calculate movie effect bias
        
        b_i <- edx %>% 
          group_by(movieId) %>%
          summarize(b_i = mean(rating - mu))
    
        #Calculate user effect bias
        
        b_u <- edx %>% 
          left_join(b_i, by="movieId") %>%
          group_by(userId) %>%
          summarize(b_u = mean(rating - b_i - mu))
        
        #predict ratings using movie and user bias
        
        predicted_ratings_m2a <- 
          final_holdout_test %>% 
          left_join(b_i, by = "movieId") %>%
          left_join(b_u, by = "userId") %>%
          mutate(pred = mu + b_i + b_u) %>%
          pull(pred)
       
        #Calculate RMSE and improvement ratio
         
        model_2a_rmse <- RMSE(predicted_ratings_m2a, final_holdout_test$rating)
        model_2a_rmse
        
        impr_2a <- model_2a_rmse/model_baseline_rmse-1

        
     #Model 2b (Movie, User and Genre effects):
        
        #Calculate genre effect bias
        
        b_g <- edx %>%
          left_join(b_i, by='movieId') %>%
          left_join(b_u, by='userId') %>%
          group_by(genres) %>%
          summarize(b_g = mean(rating - mu - b_i - b_u))
        
        #Add genre effect to predict ratings in the training set
        
        predicted_ratings_m2b <- 
          final_holdout_test %>% 
          left_join(b_i, by = "movieId") %>%
          left_join(b_u, by = "userId") %>%
          left_join(b_g, by = "genres") %>%
          mutate(pred = mu + b_i + b_u + b_g) %>%
          pull(pred)
        
        #Calculate RMSE and improvement ratio
        
        model_2b_rmse <- RMSE(predicted_ratings_m2b, final_holdout_test$rating)
        model_2b_rmse        
        
        impr_2b <- model_2b_rmse/model_2a_rmse-1

        
    #Model 3 (Penalized Least Squares):
    
    #Extending the model from 2a with the optimization parameter lambda
    
        lambdas <- seq(0, 10, 0.25)
        
        rmses <- sapply(lambdas, function(l){
          
          #Calculate the mean of ratings from the edx training set
          mu_reg <- mean(edx$rating)
          
          #Adjust mean by movie effect and penalize low number on ratings
          b_i_reg <- edx %>% 
            group_by(movieId) %>%
            summarize(b_i_reg = sum(rating - mu_reg)/(n()+l))
          
          #Adjust mean by user and movie effect and penalize low number of ratings
          b_u_reg <- edx %>% 
            left_join(b_i_reg, by="movieId") %>%
            group_by(userId) %>%
            summarize(b_u_reg = sum(rating - b_i_reg - mu_reg)/(n()+l))
          
          #predict ratings in the training set to derive optimal penalty value 'lambda'
          predicted_ratings_m3 <- 
            final_holdout_test %>% 
            left_join(b_i_reg, by = "movieId") %>%
            left_join(b_u_reg, by = "userId") %>%
            mutate(pred = mu_reg + b_i_reg + b_u_reg) %>%
            pull(pred)
          
          return(RMSE(final_holdout_test$rating,predicted_ratings_m3))
        })
        
        
        #Optimize lambda factor (to minimize RMSE)
        
        qplot(lambdas, rmses) 
        
        lambda <- lambdas[which.min(rmses)]
        lambda
        
        #Calculate RMSE and improvement ratio
        
        model_3_rmse <- min(rmses)
        model_3_rmse
        
        impr_3 <- model_3_rmse/model_2b_rmse-1
    
    
  #Comparing Results
     
        rmse_results <- data_frame(Method = c("Model 1: Baseline",
                                              "Model 2a: Movie + User Effects",
                                              "Model 2b: Movie + User + Genre Effects",
                                              "Model 3: Penalized least squares"), 
                                   RMSE = c(model_baseline_rmse,
                                            model_2a_rmse,
                                            model_2b_rmse,
                                            model_3_rmse),
                                   Improvement = c(0,
                                                   impr_2a,
                                                   impr_2b,
                                                   impr_3))
                                                           
        rmse_results %>% knitr::kable()

    
    
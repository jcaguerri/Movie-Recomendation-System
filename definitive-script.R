                      #PREPARING DATA

# Create edx set, validation set

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))
movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)
# Save both data sets
save(edx, file= "rda/edx.rda")
save(validation, file= "rda/validation.rda")


                  #EXPLORING DATA
#Structure
str(edx)
head(edx)

#How mny different movies there are, and how many differents ussers
edx %>% summarise(n_movies = n_distinct(movieId),
                  n_users = n_distinct(userId))

               #ANALYSIS
#Our goal is to find bias that could help us to adjust or algorihtm.
# Rating values
tab_rat_dist <- edx %>% 
  group_by(rating) %>%
  summarise(count = n()) %>% arrange(desc(count)) #get the distribution table of rating
mean(edx$rating)

# Rating per movies
edx %>% group_by(title) %>% summarise(count= n()) %>% arrange(desc(count))
edx %>% group_by(title) %>% summarise(count= n()) %>% arrange(count)
      #there are movies with a lot of ratings and other with just one
edx %>% 
  count(movieId) %>%
  ggplot(aes(n))+
  geom_histogram(bins = 50, color = "black") +
  scale_x_log10() +
  ggtitle("Movies(grouped) by rating frequency") +
  xlab("nº ratings")+
  ylab("movies")#visualizing this difference

#Rating per user
edx %>%
  count(userId) %>%
  ggplot(aes(n))+
  geom_histogram(bins = 50, color = "black")+
  scale_x_log10() +
  ggtitle("Number of ratings given by users")+
  xlab("nºratings")+
  ylab("users") #There are user who have done a lot of ratings, others just a few.

      #And there are users who are more critical than others
edx %>%
  group_by(userId) %>%
  filter(n() >= 20) %>% #just take user with more tahn 20 ratings
  summarize(b_u = mean(rating)) %>%
  ggplot(aes(b_u)) +
  geom_histogram(bins = 30, color = "black") +
  xlab("Mean rating") +
  ylab("users") +
  ggtitle("Mean movie ratings by users")


                #METHODS
#Function to compute RMSE
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#METHOD 1: JUST THE AVERAGE
mu <- mean(edx$rating)
  #Compute the RMSE
mu_rmse <- RMSE(validation$rating, mu)
  #Build the data frame where the resoults will be saved
rmse_results <- tibble(method = "Just Average",
                           RMSE = mu_rmse)
rmse_results %>% knitr::kable()

#METHOD 2: MOVIE EFECT
  #calculate b_i: the movie bias
movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))
  #predicting and showing the resoults
predicted_ratings <- mu + validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i
model_2_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Movie Effect Model",
                                     RMSE = model_2_rmse ))
rmse_results %>% knitr::kable()

#METHOD 3: MOVIE AND USER EFECT
  #Calculate b_u: the user bias
user_avgs <- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))
  #Predicting and showing the resoults
predicted_ratings <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred
model_3_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",  
                                     RMSE = model_3_rmse ))
rmse_results %>% knitr::kable()

#METHOD 4: "Movie + User Effects Model" regularizated (FINAL MODEL)
lambdas <- seq(0, 10, 0.25) # test differnt lambdas

  #We can´t choose the best lambda using the validation set. We have to get the lambda
  #using crossvalidation, so we will create a partition and we will select the best
  #lambda in this partition. We will use "repeated random sub-sampling validation",
  #so we will repeat this proces 5 times.
  #Finaly we will use as lambda the average of the 5 lambdas.
edx_l <- edx %>% select(userId, movieId, rating)
set.seed(1)

best_lambda <- replicate (5, simplify = FALSE, {
  test_index_l <- createDataPartition(edx_l$rating, times = 1, p = .2, list = F)
  # Create the index
  train <- edx[-test_index_l, ] # Create Train set
  test <- edx[test_index_l, ] # Create Test set
  test <- test %>% 
    semi_join(train, by = "movieId") %>%# The same movieId and usersId appears in both sets
    semi_join(train, by = "userId")
  #Now, we pick the lambda
  rmses_l <- sapply(lambdas, function(l){
    mu <- mean(train$rating)
    b_i <- train %>% 
      group_by(movieId) %>%
      summarize(b_i = sum(rating - mu)/(n()+l))
    b_u <- train %>% 
      left_join(b_i, by="movieId") %>%
      group_by(userId) %>%
      summarize(b_u = sum(rating - b_i - mu)/(n()+l))
    predicted_ratings <- test %>%
      left_join(b_i, by = "movieId") %>%
      left_join(b_u, by = "userId") %>%
      mutate(pred = mu + b_i + b_u) %>%
      pull(pred)
    return(RMSE(predicted_ratings, test$rating))
  })
  lambdas[which.min(rmses_l)]
})

true_best_lambda <- mean(as.numeric(best_lambda)) #the average of lambdas

  #Run the model with the validation set and using the lambda choosen
rmses_l <- sapply(true_best_lambda, function(l){
  mu <- mean(edx$rating)
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  predicted_ratings <- 
    validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  return(RMSE(predicted_ratings, validation$rating))
})

#Predict and showing the FINAL resoults
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie + User Effect Model",  
                                     RMSE = min(rmses_l)))
rmse_results %>% knitr::kable()


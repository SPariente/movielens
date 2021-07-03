#Develop your algorithm using the edx set. 
#For a final test of your final algorithm, predict movie ratings in the validation set (the final hold-out test set) as if they were unknown. 
#RMSE will be used to evaluate how close your predictions are to the true values in the validation set (the final hold-out test set).

##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
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

save(movielens, file = "movielens.RData")
save(edx, file = "edx.RData")
save(validation, file = "validation.RData")

rm(dl, ratings, movies, test_index, temp, removed)

#Start of the analysis
if(!require(dslabs)) install.packages("dslabs")
if(!require(lubridate)) install.packages("lubridate")

library(dslabs)
library(lubridate)

load(file = "movielens.RData")
load(file = "edx.RData")
load(file = "validation.RData")

RNGkind(sample.kind = "default")

#Contents
head(movielens)

nmovies <- n_distinct(movielens$movieId)
nusers <-  n_distinct(movielens$userId)

sparsity <- nrow(movielens) / (nusers * nmovies)

#Age range
range_m <- movielens %>%
  mutate(year = as.integer(
    str_remove_all(
      str_extract(title, pattern = "\\(\\d+\\)$"), 
      pattern = "\\D"))) %>%
  pull(year) %>%
  range()

range_r <- movielens %>%
  mutate(date = as_datetime(timestamp),
         year_rat = year(date)) %>%
  pull(year_rat) %>%
  range()

#Ratings overview
movielens %>%
  ggplot(aes(x = rating)) +
  geom_bar() +
  labs(x = "Rating", 
       y = "Number of observations")

#Ratings split between before and after 2002
label_y <- c("FALSE" = "2002 and earlier", "TRUE" = "After 2002")

movielens %>%
  mutate(date = as_datetime(timestamp),
         year_rat = year(date),
         year_sep = (year(date) > 2002)) %>%
  ggplot(aes(x = rating)) +
  geom_bar() +
  facet_wrap(~ year_sep,
             labeller = labeller(year_sep = label_y)) +
  labs(x = "Rating", 
       y = "Number of observations")

#Mean ratings
r_mean <- mean(movielens$rating)

#Table of ratings
movielens %>%
  group_by(userId) %>%
  summarize(mean = mean(rating), 
         n = n()) %>%
  slice_max(n = 25, order_by = n) %>%
  rename("User ID" = userId,
         "Average rating" = mean,
         "Number of ratings" = n) %>%
  knitr::kable()

movielens %>%
  group_by(movieId, title) %>%
  summarize(mean = mean(rating), 
         n = n())  %>%
  ungroup() %>%
  slice_max(n = 25, order_by = n) %>%
  rename("Movie ID" = movieId,
         "Title" = title,
         "Average rating" = mean,
         "Number of ratings" = n) %>%
  knitr::kable()

#Other variability sources
p1 <- movielens %>%
  mutate(year = as.integer(
    str_remove_all(
      str_extract(title, pattern = "\\(\\d+\\)$"), 
      pattern = "\\D"))) %>%
  ggplot(aes(x = year)) +
  geom_bar() +
  labs(x = "Movie release year",
       y = "Number of ratings")

p2 <- movielens %>%
  mutate(year = as.integer(
    str_remove_all(
      str_extract(title, pattern = "\\(\\d+\\)$"), 
      pattern = "\\D"))) %>%
  group_by(year) %>%
  summarize(mean = mean(rating)) %>%
  ggplot(aes(x = year, y = mean)) +
  geom_point() +
  labs(x = "Movie release year",
       y = "Average rating given")

gridExtra::grid.arrange(p1, p2)
rm(p1, p2)

#Spread of number of ratings by user
movielens %>%
  group_by(userId) %>%
  summarize(n = n()) %>%
  summarize(mean = mean(n), med = median(n), min = min(n), max = max(n)) 

#Exploring genres
movielens_genres <- movielens %>% 
  separate_rows(genres, sep = "\\|")

save(movielens_genres, file = "movielens_genres.RData")

load(file = "movielens_genres.RData")

movielens_genres %>%
  filter(genres != "(no genres listed)") %>%
  group_by(genres) %>%
  summarize(mean = mean(rating)) %>%
  ggplot(aes(x = reorder(genres, mean), y = mean)) + 
  geom_point() +
  geom_hline(yintercept = r_mean, col = "red") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Movie genre",
       y = "Average rating") +
  geom_text(aes(x = 3, y = 3.55, label = "Overall average"), col = "red") 

movielens_genres %>%
  filter(genres != "(no genres listed)")%>%
  group_by(genres) %>%
  summarize(n = n(),
            mean = mean(rating)) %>%
  ggplot(aes(x = reorder(genres, mean), y = n)) + 
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Movie genre",
       y = "Number of ratings")

#Further explore genres
top_5_u <- movielens %>%
  group_by(userId) %>%
  summarize(n = n()) %>%
  slice_max(n = 5, order_by = n) %>%
  pull(userId)

movielens_genres %>%
  filter(userId %in% top_5_u,
         genres != "(no genres listed)")%>%
  group_by(userId, genres) %>%
  summarize(mean = mean(rating)) %>%
  mutate(userId = as.factor(userId)) %>%
  ggplot(aes(x = genres, y = mean, col = userId)) + 
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Movie genre",
       y = "Average rating",
       color = "User ID")

rm(movielens_genres)
invisible(gc())

#Explore timestamps
top_5_m <- movielens %>%
  group_by(movieId) %>%
  summarize(n = n()) %>%
  slice_max(n = 5, order_by = n) %>%
  pull(movieId)

movielens %>% 
  mutate(date = as_datetime(timestamp)) %>% 
  mutate(month = round_date(date, unit = "6 months")) %>%
  group_by(month) %>%
  summarise(avg_rating = mean(rating)) %>%
  ggplot(aes(x = month, y = avg_rating)) +
  geom_point() 

movielens %>% 
  mutate(date = as_datetime(timestamp)) %>% 
  filter(movieId %in% top_5_m) %>%
  mutate(month = round_date(date, unit = "6 month"),
          movieId = as.factor(movieId)) %>%
  group_by(movieId, month) %>%
  summarise(avg_rating = mean(rating)) %>%
  ggplot(aes(x = month, y = avg_rating, col = movieId)) +
  geom_point() +
  geom_smooth(formula = y ~ x, method = "loess", span = 0.4) +
  labs(x = "Rating date",
       y = "Average rating",
       color = "Movie ID")

#Defining the error metric
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#Naive approach
mu <- mean(edx$rating)
mu

naive_rmse <- RMSE(validation$rating, mu)

rmse_results <- tibble(method = "Just the average", RMSE = naive_rmse)

#Modeling movie effects
movie_avgs <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))

movie_avgs %>%
  ggplot(aes(x = b_i, col = "black")) +
  geom_histogram(bins = 10, color = "black") +
  labs(x = "Estimated movie effect",
       y = "Number of movies")

predicted_ratings <- mu + validation %>%
  left_join(movie_avgs, by="movieId") %>%
  pull(b_i)

meffect_rmse <- RMSE(predicted_ratings, validation$rating)

rmse_results <- bind_rows(rmse_results, tibble(method = "Movie effect model", RMSE = meffect_rmse))

#Modeling user effects
user_avgs <- edx %>%
  left_join(movie_avgs, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

user_avgs %>%
  ggplot(aes(b_u)) +
  geom_histogram(bins = 10, color = "black") +
  labs(x = "Estimated user effect",
       y = "Number of users")

predicted_ratings <- validation %>%
  left_join(movie_avgs, by="movieId") %>%
  left_join(user_avgs, by="userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

mueffect_rmse <- RMSE(predicted_ratings, validation$rating)

rmse_results <- bind_rows(rmse_results, tibble(method = "Movie + user effect model", RMSE = mueffect_rmse))

#Penalized least squares for movie and user effect
less_than_5 <- movielens %>%
  group_by(movieId, title) %>%
  summarize(n = n()) %>%
  filter(n <= 5) %>%
  nrow()

less_than_5 / nmovies

rm(movielens)

lambdas <- seq(0, 1, length.out = 11)

rmses <- sapply(lambdas, function(l){
    b_i <- edx %>%
      group_by(movieId) %>%
      summarize(b_i = sum(rating - mu)/(n()+l))
    
    b_u <- edx %>%
      left_join(b_i, by="movieId") %>%
      group_by(userId) %>%
      summarize(b_u = sum(rating - b_i - mu)/(n()+l))
    
    predicted_ratings <- edx %>%
      left_join(b_i, by = "movieId") %>%
      left_join(b_u, by = "userId") %>%
      mutate(pred = mu + b_i + b_u) %>%
      pull(pred)
    
    return(RMSE(predicted_ratings, edx$rating))
})

l <- lambdas[which.min(rmses)] 

qplot(x = lambdas, y = rmses) +
  labs(x = "Lambdas", y = "RMSE on training set")

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

mpeffect_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results, tibble(method = "Movie + user effect model, penalised, single lambda", RMSE = mpeffect_rmse))

#Separate lambdas
lambdas_i <- seq(0, 5, length.out = 6)
lambdas_u <- seq(0, 1, length.out = 6)
lambdas_sep <- expand.grid(l_i = lambdas_i, l_u = lambdas_u)
nrow(lambdas_sep)

rmses <- apply(lambdas_sep, 1, function(l){
  movie_avgs <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l[[1]]))
  
  user_avgs <- edx %>%
    left_join(movie_avgs, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l[[2]]))
  
  predicted_ratings <- edx %>%
    left_join(movie_avgs, by = "movieId") %>%
    left_join(user_avgs, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, edx$rating))
})

l_sep <- lambdas_sep[which.min(rmses),]

movie_avgs <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+l_sep[[1]]))

user_avgs <- edx %>%
  left_join(movie_avgs, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+l_sep[[2]]))

predicted_ratings <- validation %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

mupeffect_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results, tibble(method = "Movie + user effect model, penalised, separate lambda", RMSE = mupeffect_rmse))

#Modeling movie effects with time
time_b_table <- sapply(3:9, function(n){
  edx %>%
    mutate(month = round_date(as_datetime(timestamp), unit = paste(n, "months"))) %>%
    group_by(movieId, month) %>%
    summarize(n = n()) %>%
    ungroup() %>%
    summarize(mean = round(mean(n), 3),
              median = median(n))
})

colnames(time_b_table) <- 3:9

time_b_table %>% knitr::kable(caption = "Mean and median number of observations per movie and per time bucket",
                             col.names = paste(3:9, "months"))


edx_resids <- edx %>%
  left_join(movie_avgs, by = "movieId") %>%
  mutate(pred = mu + b_i,
         resid = rating - pred)

time_l <- "6 months"

edx_resids %>%
  filter(movieId %in% top_5_m) %>%
  mutate(month = round_date(as_datetime(timestamp), unit = time_l),
         movieId = as.factor(movieId)) %>%
  group_by(movieId, month) %>%
  summarize(mean = mean(resid)) %>%
  ggplot(aes(x = month, y = mean, col = movieId)) +
  geom_point() +
  labs(x = "Review date", 
       y = "Average residual rating")

movie_t_avg <- edx_resids %>%
  mutate(month = round_date(as_datetime(timestamp), unit = time_l)) %>%
  group_by(movieId, month) %>%
  summarize(b_i_t = mean(rating - mu - b_i)) %>%
  ungroup()

predicted_ratings <- validation %>%
  mutate(month = round_date(as_datetime(timestamp), unit = time_l)) %>%
  left_join(movie_avgs, by="movieId") %>%
  left_join(movie_t_avg, by=c("movieId", "month")) %>%
  mutate(b_i_t = ifelse(is.na(b_i_t), 0, b_i_t),
         pred = mu + b_i + b_i_t) %>%
  pull(pred)

mteffect_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results, tibble(method = "Movie effect model, penalised, with time effect", RMSE = mteffect_rmse))

#Combining movie effects and user effect
user_avgs <- edx_resids %>%
  mutate(month = round_date(as_datetime(timestamp), unit = time_l))%>%
  left_join(movie_t_avg, by=c("movieId", "month")) %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu - b_i - b_i_t)/(n()+l_sep[[2]]))

predicted_ratings <- validation %>%
  mutate(month = round_date(as_datetime(timestamp), unit = time_l)) %>%
  left_join(movie_avgs, by= "movieId") %>%
  left_join(movie_t_avg, by=c("movieId", "month")) %>%
  left_join(user_avgs, by = "userId") %>%
  mutate(b_i_t = ifelse(is.na(b_i_t), 0, b_i_t),
         pred = mu + b_i + b_i_t + b_u) %>%
  pull(pred)

mtueffect_rmse <- RMSE(predicted_ratings, validation$rating)

rmse_results <- bind_rows(rmse_results, tibble(method = "Movie (with time effect) + user effect model, penalised, separate lambdas", RMSE = mtueffect_rmse))

#Model without time impact
movie_avgs <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+l_sep[[1]]))

user_avgs <- edx %>%
  left_join(movie_avgs, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+l_sep[[2]]))

#Modeling genre impact
edx_genres <- edx %>% 
  left_join(movie_avgs, by= "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  mutate(resid = rating - mu - b_i - b_u) %>% 
  separate_rows(genres, sep = "\\|")

top_3_u <- edx %>% group_by(userId) %>%
  summarize(n = n()) %>%
  slice_max(n = 3, order_by = n) %>%
  pull(userId)

edx_genres %>% 
  filter(userId %in% top_3_u) %>%
  group_by(userId, genres) %>%
  summarize(mean = mean(resid)) %>%
  mutate(userId = as.factor(userId)) %>%
  ggplot(aes(x = genres, y = mean, col = userId)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Movie genre",
       y = "Average residual ratings")

edx_genres %>% 
  filter(userId %in% top_3_u & 
           genres != "(no genres listed)") %>%
  group_by(userId, genres) %>%
  mutate(userId = as.factor(userId)) %>%
  ggplot(aes(x = genres, y = resid, col = userId)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Movie genre",
       y = "Average residual ratings")

genre_u_avg <- edx_genres %>%
  group_by(userId, genres) %>%
  summarize(b_g = mean(resid))

validation_genres <- validation %>% 
  left_join(movie_avgs, by="movieId") %>%
  left_join(user_avgs, by = "userId") %>% 
  separate_rows(genres, sep = "\\|")

validation_genre_avg <- validation_genres %>%
  left_join(genre_u_avg, by = c("userId", "genres")) %>%
  mutate(b_g = ifelse(is.na(b_g), 0, b_g)) %>%
  group_by(userId, movieId, timestamp) %>%
  summarize(pred = mean(b_g)) %>%
  pull(pred)

predicted_ratings <- validation %>% 
  left_join(movie_avgs, by="movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  mutate(b_g = validation_genre_avg, 
         pred = mu + b_i + b_u + b_g) %>%
  pull(pred)

mugeffect_rmse <- RMSE(predicted_ratings, validation$rating)

rmse_results <- bind_rows(rmse_results, tibble(method = "Movie + user effect model + genre effect, penalised, separate lambdas", RMSE = mugeffect_rmse))

rm(edx_genres, validation_genres, edx_resids)

#Flooring and capping predicted ratings
predicted_ratings <- ifelse(predicted_ratings < 0, 0,
                            ifelse(predicted_ratings > 5, 5, 
                                   predicted_ratings))

mugceffect_rmse <- RMSE(predicted_ratings, validation$rating)

rmse_results <- bind_rows(rmse_results, tibble(method = "Movie + user effect model + genre effect, penalised, separate lambdas, constrained", RMSE = mugceffect_rmse))

rmse_results %>%
  knitr::kable(caption = "RMSE for each model previously exposed",
               col.names = c("Model",
                             "RMSE"))

#Plotting the results
validation %>%
  bind_cols(pred = predicted_ratings) %>%
  ggplot(aes(x = rating, y = pred, group = rating)) +
  geom_boxplot() +
  geom_hline(yintercept = mu, col = "red") +
  labs(x = "Actual rating",
       y = "Predicted rating")


#Matrix factorization
if(!require(recosystem)) install.packages("recosystem")

library(recosystem)
invisible(gc())

edx_fact <- edx %>%
  select(userId, movieId, rating)

validation_fact <- validation %>%
  select(userId, movieId, rating)

edx_set <- data_memory(user_index = edx_fact$userId,
                       item_index = edx_fact$movieId,
                       rating = edx_fact$rating)

validation_set <- data_memory(user_index = validation_fact$userId,
                              item_index = validation_fact$movieId,
                              rating = validation_fact$rating)

set.seed(5)

#Setup a RecoSys object
r <- Reco()

#Train the recosystem
tuned_set <- r$tune(edx_set,
                    opts = list(dim = seq(10, 40, 10),
                                costp_l1 = 0, 
                                costq_l1 = 0,
                                lrate = c(0.1, 0.2),
                                nthread = 8)) #12 threads available on the machine used

r$train(edx_set,
        opts = c(tuned_set$min,
                 nthread = 8, #12 threads available on the machine used
                 niter = 100,
                 verbose = F))

#Create the results vector and compare to actuals
predicted_ratings <- r$predict(validation_set,
                               out_memory())  

predicted_ratings <- ifelse(predicted_ratings < 0, 0,
                            ifelse(predicted_ratings > 5, 5, 
                                   predicted_ratings))

fmatrix_rmse <- RMSE(predicted_ratings, validation$rating)


rmse_results <- bind_rows(tibble(method = "Movie + user effect model + genre effect, penalised, separate lambdas", RMSE = mugeffect_rmse),
                          tibble(method = "Matrix factorisation", RMSE = fmatrix_rmse))

rm(tuned_set, validation_set, edx_set, validation_fact, edx_fact)

#Plotting the results
validation %>%
  bind_cols(pred = predicted_ratings) %>%
  ggplot(aes(x = rating, y = pred, group = rating)) +
  geom_boxplot() +
  geom_hline(yintercept = mu, col = "red") +
  labs(x = "Actual rating",
       y = "Predicted rating")

#Combined results table
rmse_results %>%
  knitr::kable(caption = "RMSE for each model previously exposed",
               col.names = c("Model",
                             "RMSE"))

save(rmse_results, file = "rmse_results.RData")

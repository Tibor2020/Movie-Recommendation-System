
#This code was created on R version 4.0.3 (2020-10-10)

# List of packages for session
.packages = c("tidyverse", "knitr", "caret", "data.table", "lubridate")

# Install missing packages
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

# Load packages  
lapply(.packages, require, character.only=TRUE)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
# movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
#      title = as.character(title),
#      genres = as.character(genres))
# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
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

###########################
#Exploratory Data Analysis#
###########################

#Table 1: Dataset Dimensions
Table_1 <- data.frame(
  Dataset = c("edx", "validation"),
  No_of_Rows = c(nrow(edx), nrow(validation)),
  No_of_Cols = c(ncol(edx), ncol(validation)))

knitr::kable(
  Table_1,
  format = "pipe",
  caption = "Dataset Dimensions",
  col.names = c("Dataset", "No. of Rows", "No of Columns"))


#Table 2: Missing Data
sapply(edx, {function(x) sum(is.na(x))}) %>%  
  knitr::kable(format = "pipe", caption = "Number of missing data in each columns")


#Table 3: Preview of the dataset
edx %>%
  head() %>% 
  knitr::kable(format = "pipe", caption = "Preview of the dataset")


#Plot 1: Distribution of Ratings
edx %>% group_by(rating) %>%
  summarise(number = n())%>%
  arrange(desc(number)) %>%
  ggplot(aes(rating, number)) +
  geom_bar(stat = "identity", fill = "turquoise4") +
  labs(title="Number of Ratings", x ="Rating", y = "Number")

#Plot 2: Ratings before and after 2003
edx %>% 
  filter(format(as_datetime(timestamp), "%Y") < 2003) %>%
  group_by(rating) %>%
  summarise(number = n())%>%
  arrange(desc(number)) %>%
  ggplot(aes(rating, number)) +
  geom_bar(stat = "identity", fill = "tomato3") +
  labs(title="Number of Ratings before 2003", x ="Rating", y = "Number")

edx %>% 
  filter(format(as_datetime(timestamp), "%Y") >= 2003) %>%
  group_by(rating) %>%
  summarise(number = n())%>%
  arrange(desc(number)) %>%
  ggplot(aes(rating, number)) +
  geom_bar(stat = "identity", fill = "tomato3") +
  labs(title="Number of Ratings from 2003", x ="Rating", y = "Number")

#List of genres
separated_genres <- edx %>%
  separate_rows(genres, sep = "\\|") 

#Plot 3: Distribution of Genres
separated_genres %>%
  group_by(genres) %>%
  summarise(number = n()) %>%
  ggplot(aes(x = reorder(genres, number), y = number)) +
  geom_bar(stat = "identity", fill = "lightslateblue") +
  labs(title="Number of Ratings per Genre", x = "Genre", y = "Number of Ratings") +
  coord_flip()

#Table 4: 10 Best Movies
edx %>%
  group_by(title) %>%
  summarise(Rating = round(mean(rating), digits = 1), No_of_Ratings = n()) %>%
  arrange(desc(Rating)) %>%
  slice(1:10) %>% 
  knitr::kable(format = "pipe", 
               caption = "10 Best movies",
               col.names = c("Title", "Mean Rating", "Number of Ratings"))

#Table 5: 10 Worst Movies
edx %>%
  group_by(title) %>%
  summarise(Rating = round(mean(rating), digits = 1), No_of_Ratings = n()) %>%
  arrange(Rating) %>%
  slice(1:10) %>% 
  knitr::kable(format = "pipe", 
               caption = "10 Worst movies", 
               col.names = c("Title", "Mean Rating", "Number of Ratings"))


#################
#Building Models#
#################

#Setup for building models
##Dividing edx to test and train sets
set.seed(1, sample.kind="Rounding") 
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.2, list = FALSE)
train_set <- edx[-test_index,]
test_set <- edx[test_index,]

##We remove the users and movies in the test set that do not appear in the training set
test_set <- test_set %>%
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

##Computation of the average
mu <- mean(train_set$rating)

##RMSE function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))}



#Average Rating Model############################################################
avg_rmse <- RMSE(test_set$rating, mu)

rmse_summary <- tibble(method = "Average Rating Model", RMSE = round(avg_rmse, digits = 4))

##Table 6: Summary RMSE table
rmse_summary %>% 
  knitr::kable(format = "pipe", caption = "Model accuracy", col.names = c("Method", "RMSE"))
  


#Movie Effect Model############################################################
b_i <- train_set %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))

##Plot 4: Distribution of b_i
b_i %>% ggplot(aes(b_i)) +
  geom_density(fill = "rosybrown1", alpha = 0.5) +
  labs(title="Movie Effect", x = "b_i", y = "Density")

predicted_ratings <- test_set %>%
  left_join(b_i, by='movieId') %>%
  mutate(pred = mu + b_i) %>%
  pull(pred)

b_i_rmse <- RMSE(test_set$rating,  predicted_ratings)

rmse_summary <-  rmse_summary %>% add_row(method = "Movie Effect Model", RMSE = round(b_i_rmse, digits = 4))

##Table 7: RMSE Summary2
rmse_summary %>% 
  knitr::kable(format = "pipe", caption = "Model accuracy", col.names = c("Method", "RMSE"))


#Movie and User Effect Model#########################################################
##Plot 5:Distribution of b_u
train_set %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating)) %>%
  ggplot(aes(b_u)) +
  geom_density(fill = "darkslategray2", alpha = 0.5) +
  labs(title="User Effect", x = "b_u", y = "Density")

b_u <- train_set %>%
  left_join(b_i, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

predicted_ratings <- test_set %>%
  left_join(b_i, by='movieId') %>%
  left_join(b_u, by='userId') %>%
  mutate(pred  = mu + b_i + b_u) %>%
  pull(pred)

b_u_rmse <- RMSE(test_set$rating,  predicted_ratings)

rmse_summary <-  rmse_summary %>% add_row(method = "Movie and User Effect Model", RMSE = round(b_u_rmse, digits = 4))

##Table 8: RMSE Summary 3
rmse_summary %>% 
  knitr::kable(format = "pipe", caption = "Model accuracy", col.names = c("Method", "RMSE"))


#Movie, User and Time Effect Model#########################################################
##Plot 6:Distribution of b_t
train_set %>%
  mutate(year = year(as_datetime(timestamp))) %>%
  group_by(year) %>%
  summarize(b_t = mean(rating)) %>%
  ggplot(aes(b_t)) +
  geom_density(fill = "cornsilk1", alpha = 0.5) +
  labs(title="Time Effect", x = "b_t", y = "Density")


b_t <- train_set %>%
  mutate(year = year(as_datetime(timestamp))) %>%
  left_join(b_i, by='movieId') %>%
  left_join(b_u, by = "userId") %>%
  group_by(year) %>%
  summarize(b_t = mean(rating - mu - b_i - b_u))

predicted_ratings <- test_set %>%
  mutate(year = year(as_datetime(timestamp))) %>%
  left_join(b_i, by='movieId') %>%
  left_join(b_u, by='userId') %>%
  left_join(b_t, by='year') %>%
  mutate(pred = mu + b_i + b_u + b_t) %>%
  pull(pred)

b_t_rmse <- RMSE(test_set$rating,  predicted_ratings)

rmse_summary <-  rmse_summary %>% add_row(method = "Movie, User and Time Eff. Model", RMSE = round(b_t_rmse, digits = 4))

#Table 9: RMSE Summary 4
rmse_summary %>% 
  knitr::kable(format = "pipe", caption = "Model accuracy", col.names = c("Method", "RMSE"))


#Regularized Model#########################################################
##Search for lambda that minimizes RMSE step 1: Trying lambdas on wide range with big increments
lambdas <- seq(0:10)

lambda_tuning_function <-  function(l){
  
  b_i_reg <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i_reg = sum(rating - mu)/(n()+l))
  
  b_u_reg <- train_set %>%
    left_join(b_i_reg, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u_reg = sum(rating - b_i_reg - mu)/(n()+l))
  
  b_t_reg <- train_set %>%
    mutate(year = year(as_datetime(timestamp))) %>%
    left_join(b_i_reg, by='movieId') %>%
    left_join(b_u_reg, by = "userId") %>%
    group_by(year) %>%
    summarize(b_t_reg = mean(rating - b_i_reg - b_u_reg - mu))
  
  predicted_ratings <-
    test_set %>%
    mutate(year = year(as_datetime(timestamp))) %>%
    left_join(b_i_reg, by = "movieId") %>%
    left_join(b_u_reg, by = "userId") %>%
    left_join(b_t_reg, by = "year") %>%
    mutate(pred = mu + b_i_reg + b_u_reg +b_t_reg) %>%
    pull(pred)
  return(RMSE(predicted_ratings, test_set$rating))}

rmse_results <- sapply(lambdas, lambda_tuning_function)


##lambda that minimizes the RMSE
lambda_opt <- lambdas[which.min(rmse_results)] 

##Plot 7:Lambda search step1
qplot(lambdas, rmse_results)


##Search for lambda that minimizes RMSE step 2: Trying lambdas on narrowed range with small increments

lambdas <- seq(lambda_opt-1, lambda_opt+1, 0.2)

rmse_results <- sapply(lambdas, lambda_tuning_function)

lambda_opt <- lambdas[which.min(rmse_results)]

reg_rmse <- min(rmse_results)

rmse_summary <-  rmse_summary %>% add_row(method = "Regularized Model", RMSE = round(reg_rmse, digits = 4))

##Plot 8:Lambda search step2
qplot(lambdas, rmse_results)

#Table 10: RMSE Summary 5
rmse_summary %>% 
  knitr::kable(format = "pipe", caption = "Model accuracy", col.names = c("Method", "RMSE"))


#Final Touch####################################################################
##The regularized model is needed to be run on different datasets during test and validation, so we create a function
regularized_model <- function(train_df, test_df, l){
  b_i_reg <- train_df %>%
    group_by(movieId) %>%
    summarize(b_i_reg = sum(rating - mu)/(n()+l))
  
  b_u_reg <- train_df %>%
    left_join(b_i_reg, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u_reg = sum(rating - b_i_reg - mu)/(n()+l))
  
  b_t_reg <- train_df %>%
    mutate(year = year(as_datetime(timestamp))) %>%
    left_join(b_i_reg, by='movieId') %>%
    left_join(b_u_reg, by = "userId") %>%
    group_by(year) %>%
    summarize(b_t_reg = mean(rating - b_i_reg - b_u_reg - mu))
  
  predicted_ratings <-
    test_df %>%
    mutate(year = year(as_datetime(timestamp))) %>%
    left_join(b_i_reg, by = "movieId") %>%
    left_join(b_u_reg, by = "userId") %>%
    left_join(b_t_reg, by = "year") %>%
    mutate(pred = mu + b_i_reg + b_u_reg + b_t_reg) %>%
    pull(pred)}

predicted_ratings <- regularized_model(train_set, test_set, lambda_opt)

##Search for outliers
outliers <- tibble(too_small = sum(predicted_ratings < 0.5), too_large = sum(predicted_ratings > 5))

##Table 11: Outliers
outliers %>% 
  knitr::kable(format = "pipe", caption = "Prediction", col.names = c("< 0.5", "> 5"))

##Capping
predicted_ratings <- ifelse(predicted_ratings < 0.5, 0.5, predicted_ratings)
predicted_ratings <- ifelse(predicted_ratings > 5, 5, predicted_ratings)

capped_rmse <- RMSE(predicted_ratings, test_set$rating)

rmse_summary <-  rmse_summary %>% add_row(method = "Regularized Model (Capped)", RMSE = round(capped_rmse, digits = 4))

#Table 12: RMSE Summary 5
rmse_summary %>% 
  knitr::kable(format = "pipe", caption = "Model accuracy", col.names = c("Method", "RMSE"))


################################
#Final result on validation set#
################################

##We remove the users and movies in the validation that do not appear in the training set
validation <- validation %>%
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

mu <- mean(edx$rating)

##Validation
predicted_ratings <- regularized_model(edx, validation, lambda_opt)

predicted_ratings <- ifelse(predicted_ratings < 0.5, 0.5, predicted_ratings)
predicted_ratings <- ifelse(predicted_ratings > 5, 5, predicted_ratings)

final_rmse <- RMSE(predicted_ratings, validation$rating)

rmse_summary <-  rmse_summary %>% add_row(method = "Final RMSE", RMSE = round(final_rmse, digits = 4))


#Table 13: RMSE Summary 6
rmse_summary %>% 
  knitr::kable(format = "pipe", caption = "Model accuracy", col.names = c("Method", "RMSE"))



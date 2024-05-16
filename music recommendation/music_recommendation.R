# Build a recommendation engine that recommends movies to users.
# Item Based Collaborative Filter recommendation system
library(recommenderlab)
library(ggplot2)                       
library(data.table)
library(reshape2)
# Retrieve and display data
setwd("/Users/sejalkumari/Downloads/spotify_songs.csv")
song_data <- read.csv("/Users/sejalkumari/R Programming/songs.csv",stringsAsFactors=FALSE)
rating_data <- read.csv("/Users/sejalkumari/R Programming/rating.csv")
str(song_data)
# Overview the summary 
summary(song_data)
head(song_data)
summary(rating_data)
head(rating_data)
# Data pre-processing
# Creating a one-hot encoding to create a matrix that comprises of corresponding genres for each of the films.
song_genre <- as.data.frame(song_data$genres, stringsAsFactors=FALSE)
library(data.table)
song_genre2 <- as.data.frame(tstrsplit(song_genre[,1], '[|]', 
                                        type.convert=TRUE), 
                              stringsAsFactors=FALSE) 
colnames(song_genre2) <- c(1:10)
list_genre <- c("Pop", "Dance Pop", "Post-Teen Pop", "Electro Pop", 
                "Indie Poptism", "Rap","Southern Hip-Hop", "Gangster Rap", "Rock",
                "Classic Rock", "Hard Rock")
genre_mat1 <- matrix(0,10330,11)
genre_mat1[1,] <- list_genre
colnames(genre_mat1) <- list_genre
for (index in 1:nrow(song_genre2)) {
  for (col in 1:ncol(song_genre2)) {
    gen_col = which(genre_mat1[1,] == song_genre2[index,col]) 
    genre_mat1[index+1,gen_col] <- 1
  }
}
genre_mat2 <- as.data.frame(genre_mat1[-1,], stringsAsFactors=FALSE) #remove first row, which was the genre list
for (col in 1:ncol(genre_mat2)) {
  genre_mat2[,col] <- as.integer(genre_mat2[,col]) #convert from characters to integers
} 
str(genre_mat2)
# Creating a ‘search matrix’ - searching films by specifying the genre
SearchMatrix <- cbind(song_data[,1:2], genre_mat2[])
head(SearchMatrix)
ratingMatrix <- dcast(rating_data, userId~songid, value.var = "rating", na.rm=FALSE)
ratingMatrix <- as.matrix(ratingMatrix[,-1]) #remove userIds
#Convert rating matrix into a recommenderlab sparse matrix
ratingMatrix <- as(ratingMatrix, "realRatingMatrix")
ratingMatrix
# Overview some important parameters for building recommendation systems for movies
recommendation_model <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
names(recommendation_model)
lapply(recommendation_model, "[[", "description")
# Implementing a single model in the R project – Item Based Collaborative Filtering
recommendation_model$IBCF_realRatingMatrix$parameters
# Collaborative Filtering involves suggesting movies to the users that are based on collecting preferences from many other users.
# With the help of recommenderlab, we can compute similarities between users
similarity_mat <- similarity(ratingMatrix[1:4, ],
                             method = "cosine",
                             which = "users")
as.matrix(similarity_mat)
image(as.matrix(similarity_mat), main = "User's Similarities")
# Portray the similarity that is shared between the films
song_similarity <- similarity(ratingMatrix[, 1:4], method =
                                 "cosine", which = "items")
as.matrix(song_similarity)
image(as.matrix(song_similarity), main = "Songs similarity")
rating_values <- as.vector(ratingMatrix@data)
unique(rating_values) # extracting unique ratings
Table_of_Ratings <- table(rating_values) # creating a count of movie ratings
Table_of_Ratings
# Most viewed movies visualization
library(ggplot2)
song_plays <- colCounts(ratingMatrix) # count views for each movie
table_plays <- data.frame(song = names(song_plays),
                          plays =song_plays) # create dataframe of views
table_plays <- table_plays[order(table_plays$plays,
                                 decreasing = TRUE), ] # sort by number of views
table_plays$title <- NA
for (index in 1:10325){
  table_plays[index,3] <- as.character(subset(song_data,
                                              song_data$songid == table_plays[index,1])$title)
}
table_plays[1:6,]
# Visualize a bar plot for the total number of views of the top films
ggplot(table_plays[1:6, ], aes(x = title, y = plays)) +
  geom_bar(stat="identity", fill = 'steelblue') +
  geom_text(aes(label=plays), vjust=-0.3, size=3.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Total Plays of the Top Songs")
# Heatmap of Movie Ratings
# Visualize a heatmap of the movie ratings
image(ratingMatrix[1:20, 1:25], axes = FALSE, main = "Heatmap of the first 25 rows and 25 columns")
# Data Preparation
song_ratings <- ratingMatrix[rowCounts(ratingMatrix) > 50,
                              colCounts(ratingMatrix) > 50]
song_ratings
# describing matrix of relevant users
minimum_songs<- quantile(rowCounts(song_ratings), 0.98)
minimum_users <- quantile(colCounts(song_ratings), 0.98)
image(soong_ratings[rowCounts(song_ratings) > minimum_songs,
                    colCounts(song_ratings) > minimum_users],
      main = "Heatmap of the top users and songs")
# Visualizing the distribution of the average ratings per user
average_ratings <- rowMeans(song_ratings)
qplot(average_ratings, fill=I("steelblue"), col=I("red")) +
  ggtitle("Distribution of the average rating per user")
# Data Normalization
normalized_ratings <- normalize(song_ratings)
sum(rowMeans(normalized_ratings) > 0.00001)
image(normalized_ratings[rowCounts(normalized_ratings) > minimum_songs,
                         colCounts(normalized_ratings) > minimum_users],
      main = "Normalized Ratings of the Top Users")
# Data Binarization
binary_minimum_songs <- quantile(rowCounts(song_ratings), 0.95)
binary_minimum_users <- quantile(colCounts(song_ratings), 0.95)
#movies_watched <- binarize(movie_ratings, minRating = 1)
good_rated_songs <- binarize(song_ratings, minRating = 3)
image(good_rated_songs[rowCounts(song_ratings) > binary_minimum_songs,
                       colCounts(song_ratings) > binary_minimum_users],
      main = "Heatmap of the top users and songs")
# Collaborative Filtering System
# Splitting the dataset into 80% training set and 20% test set
sampled_data<- sample(x = c(TRUE, FALSE),
                      size = nrow(song_ratings),
                      replace = TRUE,
                      prob = c(0.8, 0.2))
training_data <- song_ratings[sampled_data, ]
testing_data <- song_ratings[!sampled_data, ]
# Building the Recommendation System
recommendation_system <- recommenderRegistry$get_entries(dataType ="realRatingMatrix")
recommendation_system$IBCF_realRatingMatrix$parameters
recommen_model <- Recommender(data = training_data,
                              method = "IBCF",
                              parameter = list(k = 30))
recommen_model
class(recommen_model)
# Exploring the data science recommendation system model
model_info <- getModel(recommen_model)
class(model_info$sim)
dim(model_info$sim)
top_items <- 20
image(model_info$sim[1:top_items, 1:top_items],
      main = "Heatmap of the first rows and columns")
# Visualize sum of rows and columns with the similarity of the objects above 0
sum_rows <- rowSums(model_info$sim > 0)
table(sum_rows)
sum_cols <- colSums(model_info$sim > 0)
qplot(sum_cols, fill=I("steelblue"), col=I("red"))+ ggtitle("Distribution of the column count")
# the number of items to recommend to each user
top_recommendations <- 10 
predicted_recommendations <- predict(object = recommen_model,
                                     newdata = testing_data,
                                     n = top_recommendations)
predicted_recommendations
# recommendation for the first user
user1 <- predicted_recommendations@items[[1]] 
songs_user1 <- predicted_recommendations@itemLabels[user1]
songs_user2 <- songs_user1
for (index in 1:10){
  songs_user2[index] <- as.character(subset(song_data,
                                             song_data$songid == songs_user1[index])$title)
}
songs_user2
# matrix with the recommendations for each user
recommendation_matrix <- sapply(predicted_recommendations@items,
                                function(x){ as.integer(colnames(song_ratings)[x]) }) 
#dim(recc_matrix)
recommendation_matrix[,1:4]
# Distribution of the Number of Items for IBCF
number_of_items <- factor(table(recommendation_matrix))
chart_title <- "Distribution of the Number of Items for IBCF"
qplot(number_of_items, fill=I("steelblue"), col=I("red")) + ggtitle(chart_title)

number_of_items_sorted <- sort(number_of_items, decreasing = TRUE)
number_of_items_top <- head(number_of_items_sorted, n = 4)
table_top <- data.frame(as.integer(names(number_of_items_top)),
                        number_of_items_top)
for(i in 1:4) {
  table_top[i,1] <- as.character(subset(song_data,
                                        song_data$songid == table_top[i,1])$title)
}

colnames(table_top) <- c("Song Name", "No. of Items")
head(table_top)


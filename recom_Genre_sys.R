library(gdata)
library(recommenderlab)
library(reshape2)
library(proxy)
library(data.table)
setwd("C:/Users/Prashanth/Desktop/internship/resources/ml-latest-small")
movies <- read.csv("movies.csv")
ratings <- read.csv("ratings.csv")
genres <- as.data.frame(movies$genres, stringsAsFactors = FALSE)
genres2 <-as.data.frame(tstrsplit(genres[, 1], '[|]', type.convert = TRUE),stringsAsFactors = FALSE)
colnames(genres2) <- c(1:7)
genre_list <-
  c(
    "Action",
    "Adventure",
    "Animation",
    "Children",
    "Comedy",
    "Crime",
    "Documentary",
    "Drama",
    "Fantasy",
    "Film-Noir",
    "Horror",
    "Musical",
    "Mystery",
    "Romance",
    "Sci-Fi",
    "Thriller",
    "War",
    "Western"
  )

genre_matrix <- matrix(0, 9743, 18) #empty matrix
genre_matrix[1, ] <- genre_list #set first row to genre list
colnames(genre_matrix) <- genre_list #set column names to genre list

#iterate through matrix
for (i in 1:nrow(genres2)) {
  for (c in 1:ncol(genres2)) {
    genmat_col = which(genre_matrix[1, ] == genres2[i, c])
     genre_matrix[i + 1, genmat_col] <- 1
  }
}
nrow(genres2)
#convert into dataframe
genre_matrix2 <-as.data.frame(genre_matrix[-1, ], stringsAsFactors = FALSE) #remove first row, which was the genre list
for (c in 1:ncol(genre_matrix2)) {
  genre_matrix2[, c] <- as.integer(genre_matrix2[, c])
}
#convert from characters to integers
binaryratings <- ratings
for (i in 1:nrow(binaryratings)) {
  if (binaryratings[i, 3] > 3) {
    binaryratings[i, 3] <- 1
  }
  else{
    binaryratings[i, 3] <- -1
  }
}
binaryratings2 <-
  dcast(binaryratings,
        movieId ~ userId,
        value.var = "rating",
        na.rm = FALSE)#item profile
for (i in 1:ncol(binaryratings2)) {
  binaryratings2[which(is.na(binaryratings2[, i]) == TRUE), i] <- 0
}
binaryratings2 = binaryratings2[, -1] #remove movieIds col. Rows are movieIds, cols are userIds
#Remove rows that are not rated from movies dataset
movieIds <- length(unique(movies$movieId)) #9742
ratingmovieIds <- length(unique(ratings$movieId)) #9724
movies2 <-
  movies[-which((movieIds %in% ratingmovieIds) == FALSE), ]#9741
rownames(movies2) <- NULL
#Remove rows that are not rated from genre_matrix2
genre_matrix3 <-
  genre_matrix2[-which((movieIds %in% ratingmovieIds) == FALSE), ]
rownames(genre_matrix3) <- NULL
#Calculate dot product for User Profiles
result = matrix(0, 18, 610)
for (c in 1:ncol(binaryratings2)){#columns 
  for (i in 1:ncol(genre_matrix3)){#rows 
    result[i, c] <- sum((genre_matrix3[, i]) * (binaryratings2[, c]))
  }
}

#Convert to Binary scale
for (i in 1:nrow(result)) {
  for (c in 1:ncol(result)) {
    if (result[i, c] > 0) {
      result[i, c] <- 1
    }
    else {
      result[i, c] <- 0
    }
  }
}
#s=0
#sum_1<-matrix(0,nrow=9741,ncol=1)
#for (s in c(1:nrow(genre_matrix3))){
#  val<-genre_matrix3[s,]
#  sum_1[s,1]<-sum(val)
#}
#for (s in c(1:nrow(genre_matrix3))){
#  for(s1 in c(1:ncol(genre_matrix3))){
#    genre_matrix3[s,s1]<-genre_matrix3[s,s1]/round(sqrt(sum_1[s,]))
#  }
#}
#s1=0
result2 <- result[,5]
sim_mat <- rbind.data.frame(result2, genre_matrix3)
sim_mat <- data.frame(lapply(sim_mat, function(x) {as.integer(x)}))#convert data to type integer
#Calculate Jaccard distance between user profile and all movies
sim_results <- dist(sim_mat, method = "Jaccard",by_rows = TRUE)
sim_results <- as.data.frame(as.matrix(sim_results[1:9724]))
rows <- which(sim_results == min(sim_results)) 
row2 <- rows[1:5]
#Recommended movies
print(movies[row2, ])
#genre_list[[10]]
genre_matrix2 <- as.matrix(genre_matrix2)
genre_matrix2 <- as(genre_matrix2, "realRatingMatrix")
genre_nom <- normalize(genre_matrix2)
evaluation_sch <- evaluationScheme(genre_matrix2, method="split", train=0.75, given=3,goodRating=1) 
evaluation_res <- evaluate(evaluation_sch, method="IBCF", n=1)
eval_res <- getConfusionMatrix(evaluation_res)
matr<-as.matrix(eval_res)
View(matr)
Tp<-matr[[1]][1]
Fp<-matr[[1]][2]
Fn<-matr[[1]][3]
Tn<-matr[[1]][4]
acc1=(Tp+Tn)
acc2=(Tp+Fp+Tn+Fn)
accuracy_1=(acc1/acc2)*100
sprintf("accuracy is %0.2f",accuracy_1)
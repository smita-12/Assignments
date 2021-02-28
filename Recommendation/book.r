library("recommenderlab")
library("caTools")
library(Matrix)
library(ggplot2)
library(reshape2)

summary(book)
books <- book[,-1]
View(books)
sum(is.na(books))

str(books)

attach(books)

books$Book.Title<-as.factor(books$Book.Title)
str(books)

#Plot
hist(books$Book.Rating)
hist(books$User.ID)
boxplot(books$Book.Rating)

#Building reccomendtion model
rating_matrix<- as.matrix(acast(books,User.ID~Book.Rating,fun.aggregate = mean))

#creating real rating matrix for rating#
rating_matrix_r<-as(rating_matrix,"realRatingMatrix")

##applying different methods(populare,ubcf,ibcf)
rating_popular_model<-Recommender(rating_matrix_r,method="POPULAR") 
rating_ubcf_model<-Recommender(rating_matrix_r, method="UBCF") 
rating_ibcf_model<-Recommender(rating_matrix_r,method="IBCF") 

#recommend book by ratings
recommended_book_popular <- predict(rating_popular_model,rating_matrix_r[1501:1506], n=5) #recommended by popular method
as(recommended_book_popular, "list")

recommended_book_ubcf <- predict(rating_ubcf_model,rating_matrix_r[1], n=5)    #5 recommendation by UBCF
as(recommended_book_ubcf, "list")

recommended_book_ibcf <- predict(rating_ibcf_model,rating_matrix_r[1])   #5 recommendation by IBCF#
as(recommended_book_ibcf, "list")

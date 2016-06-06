library("recommenderlab")

#creating a small articial data set as a matrix
m <- matrix(sample(c(as.numeric(0:5), NA), 5000,  
                   replace=TRUE, prob=c(rep(.9/6,6),.6)), ncol=10,  
            dimnames=list(user=paste("u", 1:500, sep=''),  
                          item=paste("i", 1:10, sep=''))) 

m

#Converting into a realRatingMatrix
r <- as(m, "realRatingMatrix")  
r


#turning into dataframe
head(as(r, "data.frame"))

#normalizing rating matrix
r_m <- normalize(r)
r_m
as(r_m, "list")
image(r, main = "Raw Ratings")
image(r_m, main = "Normalized Ratings")



# turning matrix into a 0-1 binary matrix
r_b <- binarize(r, minRating=1)
as(r_b, "matrix")


#creating a recommender
#rec=Recommender(r[1:4],method="POPULAR")
rec=Recommender(r[1:400],method="UBCF", param=list(normalize = "Z-score",method="Cosine",nn=5, minRating=1))
#rec=Recommender(r[1:400],method="UBCF", param=list(normalize = "Z-score",method="Jaccard",nn=5, minRating=1))
#rec=Recommender(r[1:100],method="IBCF", param=list(normalize = "Z-score",method="Jaccard",minRating=1))
print(rec)

names(getModel(rec))
getModel(rec)$nn

# creating predictions
recom <- predict(rec, r[1:100], type="ratings")

recom
as(recom, "list")
m[401]



as(r[5],"list")
as(recom, "matrix")[,1:10]


#EVALUATION PREDICTIONS
#scheme <- evaluationScheme(r[1:490], method="split", train = .9, given=1, goodRating=2)
scheme <- evaluationScheme(r[1:200,], method="bootstrap", given=2, goodRating=2)
#scheme <- evaluationScheme(r[1:490,], method="cross-validation", given=2, goodRating=2, k=4)
#scheme <- evaluationScheme(r[1:490,], method="split", train=0.9, given=2)


r1 <- Recommender(getData(scheme, "train"), method="UBCF",
                  param=list(normalize = "Z-score",method="Cosine",nn=5, minRating=3))
r1

r2 <- Recommender(getData(scheme, "train"), method="POPULAR")
r2

#r3 <- Recommender(getData(scheme, "train"), method="IBCF")
#r3


#computing predicted ratings for the known part of the test data (15 items for each user)
p1 <- predict(r1, getData(scheme, "known"), type="ratings")
p1

p2 <- predict(r2, getData(scheme, "known"), type="ratings")
p2
as(p2, "list")


#p3 <- predict(r3, getData(scheme, "known"), type="ratings")
#p3
#as(p3, "list")


#Average error between the prediction and the unknown part of the test data
error <- rbind(
  calcPredictionAccuracy(p1, getData(scheme, "unknown")),
  calcPredictionAccuracy(p2, getData(scheme, "unknown"))
)


rownames(error) <- c("UBCF","POPULAR")
error




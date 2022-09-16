setwd("D:/anime-list.csv")

library(reshape2)
library(dplyr)
library(coop)
library(lubridate)

data<- read.csv("OnlineRetail.csv")

#handling NA values in the Customer-ID field
sum(is.na(data$CustomerID))
#133 361 NA in Customer-ID

#let's omit NA values
data <- na.omit(data)

#building a customer-item matrix
#reshape our data frame
CustomerItemMatxix <-dcast(data, CustomerID ~ StockCode, value.var = "Quantity")

encode <- function(x){as.integer(x>0)}
CustomerItemMatxix <- CustomerItemMatxix %>%
  mutate_at(vars(-CustomerID), funs(encode))

######################################################################################################################

#user-based approach
head(CustomerItemMatxix)

#computing cosine similarities between users
UserToUser <- cosine(as.matrix(t(CustomerItemMatxix[, 2:dim(CustomerItemMatxix)[2]])))

colnames(UserToUser) <- CustomerItemMatxix$CustomerID
head(UserToUser)

#ranking the most similar customers to our customer with ID 12350
Top10Similar <- CustomerItemMatxix$CustomerID[
  order(UserToUser[, "12350"], decreasing = TRUE)[1:11]]

#let's find what the customer A(12350) bought
boughtbyA <- CustomerItemMatxix %>%
  filter(CustomerID == "12350")
boughtbyA <- colnames(CustomerItemMatxix)[which(boughtbyA !=0)]

#let's find what bought the B customer and a customer with ID=17935 from Top10Similar
boughtbyB <- CustomerItemMatxix %>%
  filter(CustomerID == "17935")
boughtbyB <- colnames(CustomerItemMatxix)[which(boughtbyB !=0)]

#finding the items that the customer B didn't buy so we can recommend these items to buy for B
RecommendToB <-setdiff(boughtbyA, boughtbyB)

#finding the descriptions of these items
RecommendToBDescription <- unique(
  data[which(data$StockCode %in% RecommendToB),
       c("StockCode", "Description")])

RecommendToBDescription <- RecommendToBDescription[match(RecommendToB, RecommendToBDescription$StockCode),]

#here is the list of the items descriptions as a recommendation to B
View(RecommendToBDescription)

################################################################################################################

#item-to-item similarity matrix
ItemToItemMatrix <-cosine(
  as.matrix(CustomerItemMatxix[, 2:dim(CustomerItemMatxix)[2]]))
head(ItemToItemMatrix)

#find top10 most similar products to the product with StockCode 23166
Top10SimilarItems <- colnames(ItemToItemMatrix)[
  order(ItemToItemMatrix[, "23166"], decreasing = TRUE)[1:11]]

#get descriptions
Top10SimilarItemsDescriptions <- unique(
  data[which(data$StockCode %in% Top10SimilarItems), c("StockCode", "Description")])

Top10SimilarItemsDescriptions <- Top10SimilarItemsDescriptions[
  match(Top10SimilarItems, Top10SimilarItemsDescriptions$StockCode),]

View(Top10SimilarItemsDescriptions)

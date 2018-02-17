rm(list = ls())
getwd()
#setwd("/Users/Aabhas/Desktop/RStudio")

install.packages("tibble")
require("ggplot2")
require(dplyr)
require(readxl)
require("tibble")
library(plyr)
library(rpart)
library(rpart.plot)
library(lubridate) 
library(dplyr)
library(plotly)
library(data.table)
library(arules)
library(arulesViz)
library(stats)

#PART 1
movies <- as_data_frame(read_excel("IMDB.xlsx"))
movies$X__1 <-  NULL
movies$title__1 <- NULL

### Imputing missing values
## Metascore
movies$metascore[is.na(movies$metascore)] <- (movies$imdb_score*10)-5  ## A general trend that the critics rating will be a little less than the users rating

## Country Of Production
getmode <- function(v) {                                               ## Function to find out the mode 
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getmode(movies$CountryofProduction)   ## Mode for CountryofProduction column       

movies$CountryofProduction[is.na(movies$CountryofProduction)] <- "United States of America"


movies$profit <- movies$revenue - movies$budget                                   ## Profit Column
movies$boxoffice <- ifelse((movies$revenue > 1.5*movies$budget), "HIT", "FLOP")   ## Hit/Flop (Rule)
 
cor(movies$metascore, movies$imdb_score, use="complete.obs")
cor(movies$metascore,movies$profit, use = "complete.obs")
cor(movies$SocialMedia_Likes,movies$revenue, use = "complete.obs")
cor(movies$budget, movies$revenue, use="complete.obs")
cor(movies$budget, movies$profit, use="complete.obs")
cor(movies$imdb_score,movies$profit, use = "complete.obs")


movies1 <- subset(movies, movies$metascore > 70)
movies2 <- subset(movies, movies$imdb_score > 7)


### Decision Tree
dtree <- movies
dtree$id <- NULL
dtree$profit <- NULL
dtree$original_title <- NULL
dtree$LanguageID <- NULL
dtree$tagline <- NULL
dtree$status <- NULL
dtree$Keyword1 <- NULL
dtree$Keyword2 <- NULL
dtree$Keyword3 <- NULL
dtree$Keyword4 <- NULL
dtree$Keyword5 <- NULL
dtree$Genre2 <- NULL
dtree$Genre3 <- NULL
dtree$Studio3 <- NULL
dtree$Studio2 <- NULL
dtree$Actor2 <- NULL


print(paste(sum(complete.cases(dtree)),"Complete cases!")) # Rows without any missing values

require(data.table)
freq_director <- setDT(dtree)[, freq := .N, by = .(Director)][order(-freq)]
freq_director <- freq_director[freq > 4]
freq_director$freq <- NULL

freq_studio <- setDT(freq_director)[, freq := .N, by = .(Studio1)][order(-freq)]
freq_studio <- freq_studio[freq > 4]
freq_studio$freq <- NULL
dtree_2 <- freq_studio

write.csv(dtree_2,"subset_imdb.csv")

sub2 <- read_excel("subset_imdb2.xlsx")
sub2$title <- NULL
sub2$Director <- NULL
sub2$Actor1 <- NULL
sub2$Studio1 <- NULL
sub2$CountryofProduction <- NULL
sub2$revenue <- NULL
sub2$country_code <- NULL
sub2$release_date <- NULL
sub2$`Director Code` <- as.factor(sub2$`Director Code`)
sub2$Actor_Code <- as.factor(sub2$Actor_Code)


set.seed(444)
rand <- runif(nrow(sub2))
random <- sub2[order(rand),]
head(random)


print(paste(sum(complete.cases(random)),"Complete cases!")) # Rows without any missing values

train <- random[1:(nrow(random)*0.60),]         #60%
test <-  random[(nrow(random)*0.40):nrow(random),]      #40%


sub2_dtree <- rpart(boxoffice ~ ., data = train, method="class")
sub2_dtree
rpart.plot(sub2_dtree, cex = 0.7, type = 2,extra = 106, yesno=2, fallen.leaves = 0, gap = 0, compress = 1, ycompress =1, nn=1)


train$pred <- predict(sub2_dtree, train, type = "class")
table(CorrectlyPredicted = train$boxoffice == train$pred)
table(Actual = train$boxoffice, Predicted = train$pred) #create a confusion matrix

### Error rate and Accuracy for the Train Dataset

train$correct <- train$boxoffice == train$pred #create a new colum, TRUE if predicted = actual, otherwise FALSE
correctcount <- length(which(train$correct))
correctcount
incorrectcount <- nrow(train) - correctcount
incorrectcount
errorrate <- incorrectcount/nrow(train)
errorrate
accuracy <- 1-errorrate
accuracy


test$pred <- predict(sub2_dtree, test, type = "class") #create a prediction using our tree
table(Actual = test$boxoffice, Predicted = test$pred) #create a confusion matrix
table(CorrectlyPredicted = test$boxoffice == test$pred)


### Error rate and Accuracy for the Test Dataset
test$correct <- test$boxoffice == test$pred #create a new colum, TRUE if predicted = actual, otherwise FALSE
testcorrectcount <- length(which(test$correct))
testcorrectcount
testincorrectcount <- nrow(test) - testcorrectcount
testincorrectcount
testerrorrate <- testincorrectcount/nrow(test)
testerrorrate
testaccuracy <- 1-testerrorrate
testaccuracy


### Director's Movie Count and Average Score
colnames(movies)


director_8ormore <- movies%>%select(Director,imdb_score)%>% 
  group_by(Director)%>%summarise(movie_count=n(),avg_score=mean(imdb_score))%>% 
  arrange(desc(movie_count))%>%filter(!Director ==""& movie_count>=8)

movies %>%select(Director,imdb_score)%>% 
  group_by(Director)%>%summarise(movie_count=n(),avg_score=mean(imdb_score))%>% 
  arrange(desc(movie_count))%>%filter(!Director ==""& movie_count>=8)%>%
  ggplot(aes(x=factor(Director,level=Director),group=1))+geom_bar(aes(y=movie_count),
  stat="identity",fill="red2")+geom_line(aes(y=avg_score),
  col="yellow3",size=1.5)+theme(axis.text.x = element_text(angle=90, face = "bold"))+
  theme(axis.text.y = element_text(angle=90, face = "bold"))+
  labs(title="Directors Movie Count and avg Score",x="Director")



### List of Actors having a movie count of 8 and above and their average imdb score 
actor_8ormore <- movies %>% select(Actor1,imdb_score) %>% group_by(Actor1) %>% 
  summarise(movie_count=n(), avg_score=mean(imdb_score)) %>% filter(!Actor1 =="" & movie_count>=8)



### Actor's Movie Count and Average Score
movies %>% select(Actor1,imdb_score) %>% group_by(Actor1) %>% 
  summarise(movie_count=n(), avg_score=mean(imdb_score)) %>% arrange(desc(movie_count)) %>% 
  filter(!Actor1 =="" & movie_count>=8) %>%
  ggplot(aes(x=factor(Actor1,level=Actor1),group=1))+geom_bar(aes(y=movie_count),
  stat="identity",fill="red2")+geom_line(aes(y=avg_score),
  col="yellow3",size=1.5)+theme(axis.text.x = element_text(angle=90, face = "bold"))+
  theme(axis.text.y = element_text(face = "bold"))+
  labs(title="Actors Movie Count and avg Score",x="Actor")


### Actor/Director Combination with movie count greater than 4 and their respective IMDB score
m <- movies %>% select(Director,Actor1,imdb_score,profit) %>% group_by(director_name,Actor1) %>% 
  summarise(movie_count=n(), avg_score=mean(imdb_score), prof = sum(profit)) %>% filter(!Actor1 =="" & movie_count>=4)



Movie.Data.directors.20 <- movies %>% 
  group_by(Director) %>%
  select(Director, budget, revenue, profit) %>%
  na.omit() %>% 
  summarise(films = n(), budget = sum(as.numeric(budget)), revenue = sum(as.numeric(revenue)), profit = sum(as.numeric(profit))) %>% 
  arrange(desc(profit)) %>% 
  top_n(20, profit)

#Plot
install.packages("ggrepel")
require(ggrepel)

ggplot(Movie.Data.directors.20, aes(x=films, y=profit/1000000)) + geom_point(size = 1) + 
  geom_text_repel(aes(label = Director), size = 2.5) + xlab("Number of Films") + 
  ylab("Profit $millions") + ggtitle("Most Profitable Directors")


#Top 20 most profitable directors - average per film
Movie.Data.directors.20 <- Movie.Data.directors.20 %>% 
  mutate(avg_per_film = profit/films) %>%
  arrange(desc(avg_per_film)) %>% 
  top_n(20, avg_per_film)

#Plot
ggplot(Movie.Data.directors.20, aes(x=films, y=avg_per_film/1000000)) + geom_point(size = 1) + 
  geom_text_repel(aes(label = Director), size = 3.0) + xlab("Number of Films") + 
  ylab("Avg Profit per Film $millions") + ggtitle("Most Profitable Directors - Avg per Film")


#PART 2

#Extracting year
movies$yr <- year(as.Date(movies$release_date, origin = '1900-1-1')) 
movies$yr

#Profit Calculation
movies$profit <- (movies$revenue - (1.5 *movies$budget))
movies$profit

#Successful Movies
movies$success <- ifelse(movies$profit > 0, 1, 0)

#Which studio produces the most pictures?
sort(table(movies$Studio1))

#Successful movies subset to perform analysis 
successful <- subset(movies,movies$success == 1)
successful$profit <- (successful$revenue - (1.5 *successful$budget))

#Used for association rule mining
write.csv(successful, "successful_movies.csv")

#Most Frequent studios with atleast 10 movies
freq_studio <- setDT(successful)[, freq := .N, by = .(Studio1)][order(-freq)]
freq_studio <- freq_studio[freq > 10]

#Most Frequent directors with atleast 6 movies
freq_director <- setDT(successful)[, freq := .N, by = .(Director)][order(-freq)]
freq_director <- freq_director[freq > 2]

#Most Frequent actors with atleast 3 movies
freq_actor <- setDT(freq_director)[, freq := .N, by = .(Actor1)][order(-freq)]
freq_actor <- freq_actor[freq > 2]

#Freq Studios > 10, Directors > 6, Actors > 6
write.csv(freq_actor, "successful_movies_direc.csv")

#Top 50 Profitable movies
sorted <- successful %>%
  arrange(desc(profit)) %>%
  slice(1:50)
sorted


#HEAT MAP on the basis of the most frequent studios

#Studio vs Rating
studio_count <- freq_studio %>%
  subset(Studio1 != "") %>%
  subset(Studio1 != "New Line") %>%
  na.omit() %>%
  group_by(Studio1,imdb_score) %>%
  summarise(count=n())

colnames(studio_count)[3] <- "Count"

ggplot(studio_count,aes(imdb_score,Studio1))+
  geom_tile(aes(fill=Count),colour="white")+
  scale_fill_gradient(low="yellow",high = "red")+
  xlab("IMDB Rating")+
  ylab("Studio")+
  ggtitle("Studio vs IMDB Rating")+
  guides(fill=FALSE) +
  theme(axis.title=element_text(face="bold"),
        axis.text=element_text(size=10,face="bold")) 

#Studio vs Year
studio_count <- freq_studio %>%
  subset(Studio1 != "") %>%
  subset(Studio1 != "New Line") %>%
  na.omit() %>%
  group_by(Studio1,yr) %>%
  summarise(count=n())

colnames(studio_count)[3] <- "Count"

ggplot(studio_count,aes(yr,Studio1))+
  geom_tile(aes(fill=Count),colour="white")+
  scale_fill_gradient(low="yellow",high = "red")+
  xlab("Year of movie release")+
  ylab("Studio")+
  ggtitle("Studio vs Movie Release Year")+
  guides(fill=FALSE)+
  theme(axis.title=element_text(face="bold"),
        axis.text=element_text(size=10, face="bold")) 

#Association Rule on Studios - Combination of Studios
assocs = read.transactions("ActorDirectorAssoc2.csv", format = "single", sep = ",", cols = c("id", "Director"), rm.duplicates = TRUE)

# Plot frequency of 20 top products
itemFrequencyPlot(assocs,topN=20,type="absolute")

# Generate rules and sort by lift
rules <- apriori(assocs, parameter = list(supp = 0.005, conf = 0.2, minlen = 2))
rules <- sort(rules, by="support", decreasing=TRUE)

#Plot rules
plot(rules, method="graph", shading=NA)

# Limit output to 2 digits
options(digits=2)

# Show rules and summary
inspect(rules)
summary(rules)

# Remove duplicate rules
redundant_index <- is.redundant(rules)
pruned_rules <- rules[!redundant_index]
inspect(pruned_rules[1:8])
summary(pruned_rules)
summary(assocs)

attach(movies)
#Budget vs Profit
dp <- ggplot(data=movies, aes(x=budget/1000000, y=profit/1000000)) + geom_line(color="blue")+
  xlab("Budget")+
  ylab("Profit")+
  ggtitle("Profit vs Budget in Millions")+
  guides(fill=FALSE)+
  theme(axis.title=element_text(face="bold"),
        axis.text=element_text(size=10, face="bold")) 
dp

#IMDB Score vs Profit
dp <- ggplot(data=movies, aes(x=imdb_score , y=profit/1000000)) + geom_line(color="blue")+
  xlab("IMDB Score")+
  ylab("Profit")+
  ggtitle("Profit(in Millions) vs IMDB Score")+
  guides(fill=FALSE)+
  theme(axis.title=element_text(face="bold"),
        axis.text=element_text(size=10, face="bold")) 
dp

#Critic Score vs Profit
dp <- ggplot(data=movies, aes(x=metascore/10 , y=profit/1000000)) + geom_line(color="red")+
  xlab("Critic Rating")+
  ylab("Profit")+
  ggtitle("Profit (in millions) vs Critic Rating")+
  guides(fill=FALSE)+
  theme(axis.title=element_text(face="bold"),
        axis.text=element_text(size=10, face="bold")) 
dp

#Decision Tree

### Test Model Performance 
testModelPerformance <- function(model, dataset, target, prediction) {
  if(missing(prediction))
  {
    print("here")
    dataset$pred <- predict(model, dataset, type = "class")
  }
  else
  {
    print("here2")
    dataset$pred <- prediction
  }
  
  writeLines("PERFORMANCE EVALUATION FOR")
  writeLines(paste("Model:", deparse(substitute(model))))
  writeLines(paste("Target:", deparse(substitute(target))))
  
  writeLines("\n\nConfusion Matrix:")
  confMatrix <- table(Actual = target, Predicted = dataset$pred)
  truePos <- confMatrix[2,2]
  falseNeg <- confMatrix[2,1]
  falsePos <- confMatrix[1,2]
  trueNeg <- confMatrix[1,1]
  print(confMatrix)
  writeLines("\n\n")
  
  accuracy <- (truePos + trueNeg)/(truePos + falseNeg + falsePos + trueNeg)
  sensitivity <- truePos/(truePos + falseNeg)
  specificity <- trueNeg/(falsePos + trueNeg)
  falsePosRate <- falsePos/(falsePos + trueNeg)
  falseNegRate <- falseNeg/(truePos + falseNeg)
  precision <- truePos/(truePos + falsePos)
  
  writeLines(paste("Accuracy:", round(accuracy, digits = 4)))
  writeLines(paste("Sensitivity:", round(sensitivity, digits = 4)))
  writeLines(paste("Specificity:", round(specificity, digits = 4)))
  writeLines(paste("False Positive Rate:", round(falsePosRate, digits = 4)))
  writeLines(paste("False Negative Rate:", round(falseNegRate, digits = 4)))
  writeLines(paste("Precision:", round(precision, digits = 4)))
  
  dataset
}


#Dataset
dtree <- movies
#Removing values not needed for the decision tree
dtree$id <- NULL
dtree$original_title <- NULL
dtree$CountryofProduction <- NULL
dtree$Director<- NULL
dtree$Actor1<- NULL
dtree$Actor2<- NULL
dtree$LanguageID<- NULL
#dtree$Genre1<- NULL
dtree$Genre2<- NULL
dtree$Genre3<- NULL
dtree$Keyword1<- NULL
dtree$Keyword2<- NULL
dtree$Keyword3<- NULL
dtree$Keyword4<- NULL
dtree$Keyword5<- NULL
dtree$Studio1<- NULL
dtree$Studio2<- NULL
dtree$Studio3<- NULL
dtree$status<- NULL
dtree$tagline<- NULL
dtree$title<- NULL
dtree$release_date <-NULL
dtree$revenue <- NULL
dtree$profit <- NULL
dtree$hit_or_flop <-NULL
dtree$vote_count <- dtree$vote_count * 100
#Null values
dtree <- subset(dtree,!is.na(dtree$SocialMedia_Likes))
#Removing one row from that dataset that contains Genre Foreign as it is the only value
dtree <- subset(dtree,(dtree$Genre1 != "Foreign"))
## Metascore
dtree$metascore[is.na(dtree$metascore)] <- (dtree$imdb_score*10)-5 
colSums(is.na(dtree))

#Creating a decision tree on the training dataset 
set.seed(42)
rand <- runif(nrow(dtree))
movierand <- dtree[order(rand),]
head(movierand)
#Setting partition
#training set - 50%
train <- movierand[1:(nrow(dtree)*0.5),]
#test data - 50%
test <- movierand[(nrow(dtree)*0.5):(nrow(dtree)),]
#tree
tree <- rpart(success~.,data = train, method = "class")
rpart.plot(tree, cex=0.4, type = 1,ycompress =1)
#test performance of model on training dataset
train$pred <- predict(tree,  train, type = "class")
table(Actual =  train$success, Predicted =  train$pred)
#values on the diagonal show correctly predicted data
train$correct <-  train$success ==  train$pred
traincorrectcount <- length((which( train$correct)))
trainincorrectcount <- nrow( train) - traincorrectcount
trainerrorrate <- trainincorrectcount/nrow( train)
trainaccuracy <- 1 - trainerrorrate

#test performance of model on test dataset
test$pred <- predict( tree,  test, type = "class")
table(Actual =  test$success, Predicted=  test$pred)
test$correct <-  test$success ==  test$pred
testcorrectcount <- length((which( test$correct)))
testincorrectcount <- nrow( train) - testcorrectcount
testerrorrate <- testincorrectcount/nrow( train)
testaccuracy <- 1 - testerrorrate
#Compare
paste("TRAIN: Error Rate (", trainerrorrate, ") Accuracy (", trainaccuracy, ")")
paste("TEST: Error Rate (", testerrorrate, ") Accuracy (", testaccuracy, ")")

#Testing models performance
trainmodel <- testModelPerformance("decision tree", train, train$success, train$pred)
testmodel <- testModelPerformance("decision tree", test, test$success, test$pred)

#Part3
#movie trends over time
library(Hmisc)
library(leaps)
library(ggplot2)
library(Rcpp)
setwd("C:\\Users\\mukth\\Desktop\\UTD_Courses\\Fall 2017\\BUAN 6356 R\\project")
movies<-read.csv("IMDB.csv", header=T,na.string=c("","NA"))
summary(movies)

#renamed colums for ease
movies$a1likes<-movies$SocialMedia_Likes
movies$genre<-movies$Genre1
movies$IMDB_rating<-movies$imdb_score
movies$critic_rating<-movies$metascore/10

#converting actor socialmesdia likes to numeric
movies$a1likes<-as.character(movies$a1likes)
movies$a1likes<-as.numeric(movies$a1likes)

#creating release year column
movies$year<-as.Date(movies$release_date, "%m/%d/%Y")
movies$year<-as.numeric(format(movies$year, "%Y"))

#plot of genre by movie count
ggplot(movies)+geom_bar(aes(x=genre),fill="#004C99")+
  labs(title = "Count of Genre", x = "Genre", y = "Count")+coord_flip()

#creating decades for movie release
decade <- rep(NA, nrow(movies))
decade[which(movies$year>=2000)]="2000s"
decade[which(movies$year>=1990 & movies$year<2000)]="1990s"
decade[which(movies$year>=1980 & movies$year<1990)]="1980s"
decade[which(movies$year>=1970 & movies$year<1980)]="1970s"
decade[which(movies$year>=1960 & movies$year<1970)]="1960s"
decade[which( movies$year<1960)]="1950s"
movies$decade<-as.factor(decade)
decade<-na.omit(decade)

#plotting the score distribution by genre
ggplot(aes(x = genre, y = IMDB_rating), data = movies) + geom_boxplot() + ggtitle("Distribution of Ratings for different Genres")


#subset movies beyond 1980s
movies80s<-subset(movies,year>=1980)


#IMDB rating by release date (1980-2016)
ggplot(movies80s, aes(x=decade, y=IMDB_rating,colour=genre,group=genre)) + stat_summary(fun.y="mean", geom="smooth")+ 
  labs(title="IMDB rating by release date (1980-2016)")

ggplot(movies80s, aes(x=decade, y=critic_rating,colour=genre,group=genre)) + stat_summary(fun.y="mean", geom="smooth")+ 
  labs(title="IMDB critic rating by release date (1980-2016)")


#subset movies before 1980s
movies5070<-subset(movies,year<1980)


ggplot(movies5070, aes(x=decade, y=IMDB_rating,colour=genre,group=genre)) + stat_summary(fun.y="mean", geom="smooth")+ 
  labs(title="IMDB rating by release date (1950-1980)")

ggplot(movies5070, aes(x=decade, y=critic_rating,colour=genre,group=genre)) + stat_summary(fun.y="mean", geom="smooth")+ 
  labs(title="IMDB Critic rating by release date (1950-1980)")

#subset movies 2000s
movies2000<-subset(movies,year>=2000)

#select only top20 movies by revenue
movies2000<-movies2000[order(-movies2000$revenue)[1:15],]

is.factor(movies2000$critic_rating)
movies2000$critic_rating<-as.numeric(movies2000$critic_rating)

#order in descending
movies2000[order(-movies2000$revenue)]

ggplot(data = movies2000, aes(x =Actor1, y = revenue, fill = title)) + 
  geom_bar(stat="identity")+ggtitle("Actors of top grossing movies between 2000-2016")

ggplot(data = movies2000actor, aes(x =Actor1, y = a1likes, fill= title)) + 
  geom_bar(stat="identity")+ggtitle("Actors of top grossing movies between 2000-2016")

#directors of top 20 grossing movies
ggplot(data = movies2000, aes(x =Director, y = revenue, fill = title)) + 
  geom_bar(stat="identity")+ggtitle("Directors of top grossing movies between 2000-2016")

options(scipen=999)

#90s Movies
movies90<-subset(movies,year>=1990 & year<=1999)

#select only top15 movies by revenue
movies90<-movies90[order(-movies90$revenue)[1:15],]

movies90[order(-movies90$revenue)]


ggplot(data = movies90, aes(x =Actor1, y = revenue, fill = title)) + 
  geom_bar(stat="identity")+ggtitle("Actors of top grossing movies between 1990-1999")

ggplot(data = movies90, aes(x =Actor1, y = a1likes, fill= title)) + 
  geom_bar(stat="identity")+ggtitle("Actors of top grossing movies between 1990-1999")

#directors of top 20 grossing movies
ggplot(data = movies90, aes(x =Director, y = revenue, fill = title)) + 
  geom_bar(stat="identity")+ggtitle("Directors of top grossing movies between 1990-1999")


#Actor socialmedia likes affecting movie revenue
actors<-movies[!duplicated(movies$Actor1),]

actors1<-actors[order(-actors$a1likes)[1:20],]

ggplot(data = actors1, aes(x =Actor1, y = a1likes)) + 
  geom_bar(stat="identity")+ggtitle("Most popular actors on social media")

actors2<-actors[order(-actors$a1likes)[21:40],]

ggplot(data = actors2, aes(x =Actor1, y = a1likes)) + 
  geom_bar(stat="identity")+ggtitle("Most popular actors on social media")

actors3<-actors[order(-actors$a1likes)[40:50],]

ggplot(data = actors3, aes(x =Actor1, y = a1likes)) + 
  geom_bar(stat="identity")+ggtitle("Most popular actors on social media")

#Linear regression model
model<-lm(IMDB_rating~ budget + critic_rating + vote_count + runtime, data=movies)
summary(model)


#########################################################################
#SENTIMENT ANALYSIS
########################################################################

install.packages("syuzhet")
library('syuzhet')
install.packages("DBI")
library(DBI)

setwd("C:\\Users\\Aishvarya\\Documents\\Fall 2017 Semester\\BUAN 6356 - Prof Bardhan\\BA with R Project\\iMDB dataset")

#######################Overview of movie#####################################

moviedata=read.csv('overview.csv',stringsAsFactors = FALSE)

df<-data.frame(moviedata)

word<-get_nrc_sentiment(df$description)
td<-data.frame(t(word))

td_new <- data.frame(rowSums(td[2:3000]))
#The function rowSums computes column sums across rows for each level of a grouping variable.

#Transformation and  cleaning
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]

library("ggplot2")
qplot(sentiment, data=td_new2, weight=count, geom="bar",fill=sentiment)+ggtitle("sentiments")

######################tagline of movie####################################

moviedata=read.csv('overview.csv',stringsAsFactors = FALSE)

df<-data.frame(moviedata)

word<-get_nrc_sentiment(df$tagline)
td<-data.frame(t(word))

td_new <- data.frame(rowSums(td[2:2000]))
#The function rowSums computes column sums across rows for each level of a grouping variable.

#Transformation and  cleaning
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]

library("ggplot2")
qplot(sentiment, data=td_new2, weight=count, geom="bar",fill=sentiment)+ggtitle("sentiments")

##############################################################
##########Text Mining#########################################

install.packages("Rserve")
library(Rserve)
library(readxl)
Rserve()
Needed <- c("tm", "NLP", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", 
            "cluster", "igraph", "fpc")
install.packages(Needed, dependencies = TRUE)

install.packages("readtext")
library(readtext)

DATA_DIR <- system.file("extdata/", package = "readtext")

setwd("C:\\Users\\Aishvarya\\Documents\\Fall 2017 Semester\\BUAN 6356 - Prof Bardhan\\BA with R Project\\iMDB dataset")
library(tm)

#################################################################################
#Creating A WORDCLOUD FROM THE TAGLINES USED FOR THE MOVIES
#################################################################################

tags<-readLines("tags.txt")
tags
dir(tags)

cloud <-Corpus(VectorSource(tags))
summary(cloud)


# Convert the text to lower case
cloud<-tm_map(cloud,content_transformer(tolower))
# Remove numbers
cloud<-tm_map(cloud,removeNumbers)
# Remove english common stopwords
cloud<-tm_map(cloud, removeWords,stopwords("english"))
# Remove punctuations
cloud<-tm_map(cloud, removePunctuation)
# Eliminate extra white spaces
cloud <- tm_map(cloud, stripWhitespace)

# Text stemming (reduces words to their root form)
library("SnowballC")
#cloud <- tm_map(cloud, stemDocument)
# Remove additional stopwords
cloud <- tm_map(cloud, removeWords, c("movies","film","one","will","can","come","man","will","back","get","never","just"))

#create the term-document matrix
dtm <- TermDocumentMatrix(cloud)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
d
head(d, 10)

# Generate the WordCloud
library("wordcloud")
library("RColorBrewer")
par(bg="#000000")
png(file="Taglines.png",width=1000,height=700, bg="#000000")
wordcloud(d$word, d$freq, col=terrain.colors(length(d$word), alpha=0.9), random.order=FALSE, rot.per=0.3,max.words = 150 )
title(main = "Most Used Words in the Tagline", font.main = 1, col.main = "cornsilk4", cex.main = 1.5)
dev.off()


#################################################################################
#Creating A WORDCLOUD FROM THE KEYWORDS USED FOR THE MOVIES
#################################################################################

tags<-readLines("key.txt")
tags
dir(tags)

cloud <-Corpus(VectorSource(tags))
summary(cloud)


# Convert the text to lower case
cloud<-tm_map(cloud,content_transformer(tolower))
# Remove numbers
cloud<-tm_map(cloud,removeNumbers)
# Remove english common stopwords
cloud<-tm_map(cloud, removeWords,stopwords("english"))
# Remove punctuations
cloud<-tm_map(cloud, removePunctuation)
# Eliminate extra white spaces
cloud <- tm_map(cloud, stripWhitespace)

# Text stemming (reduces words to their root form)
library("SnowballC")
#cloud <- tm_map(cloud, stemDocument)
# Remove additional stopwords
cloud <- tm_map(cloud, removeWords, c("movies","film","one","will","duringcreditsstinger","can","come","man","will","back","based","get","novel","nudity","england"))

#create the term-document matrix
dtm <- TermDocumentMatrix(cloud)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
d
head(d, 10)

# Generate the WordCloud
library("wordcloud")
library("RColorBrewer")
par(bg="#000000")
png(file="Keywords.png",width=1000,height=700, bg="#000000")
wordcloud(d$word, d$freq, col=terrain.colors(length(d$word), alpha=0.9), random.order=FALSE, rot.per=0.3,max.words = 150)
title(main = "Most Used Keywords in the Movies", font.main = 1, col.main = "cornsilk4", cex.main = 1.5)
dev.off()



#################################################################################
#Linear Regression
#################################################################################
library(readxl)

setwd("C:/Users/minoc/Desktop/BAR")
getwd()
mydata<-read_xlsx("IMDB.xlsx")

mydata$metascore <- ifelse(is.na(mydata$metascore), mean(mydata$metascore, na.rm=TRUE), mydata$metascore)
sum(is.na(mydata$metascore))
sum(is.na(mydata$SocialMedia_Likes))
sum(is.na(mydata$budget))
sum(is.na(mydata$vote_count))
sum(is.na(mydata$runtime))

mydata$budget <- log(mydata$budget)
mydata$runtime <- log(mydata$runtime)

hist(mydata$budget)
hist(mydata$revenue)
hist(mydata$metascore)
hist(mydata$runtime)
hist(mydata$vote_count)
hist(mydata$SocialMedia_Likes)



model <- lm(imdb_score ~ SocialMedia_Likes + budget  + metascore + vote_count + runtime, data = mydata)
summary(model)













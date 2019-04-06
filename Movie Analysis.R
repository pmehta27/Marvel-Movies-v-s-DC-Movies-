setwd("D:/Sem 3/ankit/Final Project")
rm(list = ls())
# Load packages
library(ggplot2) # visualization
library(ggrepel)
library(ggthemes) # visualization
library(scales) # visualization
library(dplyr) # data manipulation
library(VIM)
library(data.table)
library(formattable)
library(plotly)
library(corrplot)
library(GGally)
library(caret)
library(car)
library(psych)
library(BSDA)
library(caret)

IMDB.data <- read.csv("D:/Sem 3/ankit/Final Project/movie_metadata.csv")
str(IMDB.data)
summary(IMDB.data)
head(IMDB.data)

x = IMDB.data$imdb_score
score_data = IMDB.data %>% sample_frac(0.1) # 10% sample data
score = score_data$imdb_score
describe(score)
mean(score)
#mean(x)
length(x)
z.test(x,NULL,alternative="greater",mu=6.44, sigma.x = sd(x),conf.level = 0.95)
# z test on sample
z.test(score,NULL,alternative="greater",mu=6.44, sigma.x = sd(score),conf.level = 0.95)

# To check hypothesis, z test has been done where p value is greater than 0.05. 
#Thus, at 95% confidence interval, 
# there is enough evidence to accept null hypothesis

boxplot(x,col="red", main ="IMDb Scores")


# Number of duplicates in dataset
sum(duplicated(IMDB.data))

# Delete 45 duplicate rows
IMDB.data <-IMDB.data[!duplicated(IMDB.data), ]
nrow(IMDB.data)
head(IMDB.data$movie_title)

# movie titles have a special character (Â) at the end and some have whitespaces too. 
# Hence, removing

#install.packages("stringr")
library(stringr)
IMDB.data$movie_title <- gsub("Â", "", as.character(factor(IMDB.data$movie_title)))
str_trim(IMDB.data$movie_title, side = "right")
head(IMDB.data$movie_title)

# Split Genres
head(IMDB.data$genres)
#First, we want to know if genre is related to imdb score. We divide the string
#into several substrings by the separator '|', and save each substring along with 
#its correspongding imdb score in the other data frame genres.df. Then we plot a 
#histogram for the score and genres to see if they are relative or not.

# create a new data frame
genres.df <- as.data.frame(IMDB.data[,c("genres","imdb_score")])
head(genres.df)

# separate different genres into new columns
unique(genres.df$genres)

# There are 914 unique combinations of genres amd 23 different values for genres 
#in this dataset
genres.df$Action <- sapply(1:length(genres.df$genres),
                           function(x) if(genres.df[x,1] %like% "Action") 1 else 0)
genres.df$Adventure <- sapply(1:length(genres.df$genres), 
                              function(x) if (genres.df[x,1] %like% "Adventure") 1 else 0)
genres.df$Animation <- sapply(1:length(genres.df$genres), 
                              function(x) if (genres.df[x,1] %like% "Animation") 1 else 0)
genres.df$Biography <- sapply(1:length(genres.df$genres), 
                              function(x) if (genres.df[x,1] %like% "Biography") 1 else 0)
genres.df$Comedy <- sapply(1:length(genres.df$genres),
                           function(x) if (genres.df[x,1] %like% "Comedy") 1 else 0)
genres.df$Crime <- sapply(1:length(genres.df$genres), 
                          function(x) if (genres.df[x,1] %like% "Crime") 1 else 0)
genres.df$Documentary <- sapply(1:length(genres.df$genres), 
                                function(x) if (genres.df[x,1] %like% "Documentary") 1 else 0)
genres.df$Drama <- sapply(1:length(genres.df$genres), 
                          function(x) if (genres.df[x,1] %like% "Drama") 1 else 0)
genres.df$Family <- sapply(1:length(genres.df$genres), 
                           function(x) if (genres.df[x,1] %like% "Family") 1 else 0)
genres.df$Fantasy <- sapply(1:length(genres.df$genres), 
                            function(x) if (genres.df[x,1] %like% "Fantasy") 1 else 0)
genres.df$`Film-Noir` <- sapply(1:length(genres.df$genres), 
                                function(x) if (genres.df[x,1] %like% "Film-Noir") 1 else 0)
genres.df$History <- sapply(1:length(genres.df$genres), 
                            function(x) if (genres.df[x,1] %like% "History") 1 else 0)
genres.df$Horror <- sapply(1:length(genres.df$genres), 
                           function(x) if (genres.df[x,1] %like% "Horror") 1 else 0)
genres.df$Musical <- sapply(1:length(genres.df$genres), 
                            function(x) if (genres.df[x,1] %like% "Musical") 1 else 0)
genres.df$Mystery <- sapply(1:length(genres.df$genres), 
                            function(x) if (genres.df[x,1] %like% "Mystery") 1 else 0)
genres.df$News <- sapply(1:length(genres.df$genres), 
                         function(x) if (genres.df[x,1] %like% "News") 1 else 0)
genres.df$Romance <- sapply(1:length(genres.df$genres), 
                            function(x) if (genres.df[x,1] %like% "Romance") 1 else 0)
genres.df$`Sci-Fi` <- sapply(1:length(genres.df$genres), 
                             function(x) if (genres.df[x,1] %like% "Sci-Fi") 1 else 0)
genres.df$Short <- sapply(1:length(genres.df$genres), 
                          function(x) if (genres.df[x,1] %like% "Short") 1 else 0)
genres.df$Sport <- sapply(1:length(genres.df$genres), 
                          function(x) if (genres.df[x,1] %like% "Sport") 1 else 0)
genres.df$Thriller <- sapply(1:length(genres.df$genres), 
                             function(x) if (genres.df[x,1] %like% "Thriller") 1 else 0)
genres.df$War <- sapply(1:length(genres.df$genres), 
                        function(x) if (genres.df[x,1] %like% "War") 1 else 0)
genres.df$Western <- sapply(1:length(genres.df$genres), 
                            function(x) if (genres.df[x,1] %like% "Western") 1 else 0)
# head(genres.df)
# get the mean of imdb score for different genres

ncol(genres.df)
means <- rep(0:23)
for(i in 1:23)
{
  means[i] <- mean(genres.df$imdb_score[genres.df[i+2]==1])
}
# plot the means on y axis and x axis represnts each type of genre, for ex. 
#action has mean 6.24 and so on
# barplot(means, main = "Average imdb scores for different genres")
#library(xlsx)
#write.xlsx(genres.df, "D:/Sem 3/ankit/Final Project/genres.xlsx")

# Data Cleaning
# Find missing values
colSums(sapply(IMDB.data,is.na))
# using heatmap to visualize missing values.
missing.values <- aggr(IMDB.data, sortVars = T, prop = T, sortCombs = T, cex.lab = 1.5, cex.axis = .6, 
                       cex.numbers = 5, combined = F, gap = -.2)
gross.unique<-unique(IMDB.data$gross)
length(gross.unique)

budget.unique<- unique(IMDB.data$budget)
length(budget.unique)
# create a data frame for gross 
gross.df <- data.frame(IMDB.data$gross)
head(gross.df)
hist(IMDB.data$gross,breaks = 50)

mean(gross.df$IMDB.data.gross, na.rm = TRUE)
median(gross.df$IMDB.data.gross, na.rm = TRUE)
head(gross.df)


# create a data frame for budget 
budget.df <- data.frame(IMDB.data$budget)
head(budget.df)
hist(IMDB.data$budget, breaks = 150)
mean(budget.df$IMDB.data.budget, na.rm = TRUE)
median(budget.df$IMDB.data.budget,na.rm = TRUE)

bud <- budget.df/1000000
head(bud)
hist(budget.df$IMDB.data.budget/1000000, breaks = 100)

#since the data is skewed, we shall use median to impute the missing values

# impute missing value with column median
IMDB.data$gross[is.na(IMDB.data$gross)]<- round(median(IMDB.data$gross,na.rm = TRUE))
head(IMDB.data$gross)       

IMDB.data$budget[is.na(IMDB.data$budget)]<- round(median(IMDB.data$budget,na.rm = TRUE))
# total number of rows in IMDB dataset
dim(IMDB.data)

#hist(IMDB.data$budget/1000000, breaks = 100)
#hist(IMDB.data$gross, breaks = 100)

# total number of rows without na's
sum(complete.cases(IMDB.data))

# Determine number of NA's in each column
colSums(sapply(IMDB.data, is.na))

#aspect_ratio has the highest number of missing values. 
#Before trying to impute the missing values, we want to check how 
#important is this variable.
table(IMDB.data$aspect_ratio)


IMDB.data$aspect_ratio[is.na(IMDB.data$aspect_ratio)] <- 0
mean(IMDB.data$imdb_score[IMDB.data$aspect_ratio == 1.85])
mean(IMDB.data$imdb_score[IMDB.data$aspect_ratio == 2.35])
mean(IMDB.data$imdb_score[IMDB.data$aspect_ratio != 1.85 & IMDB.data$aspect_ratio != 2.35])

hist(IMDB.data$aspect_ratio,breaks=70)

# perfect aspect ratio is 16:9 which is 1.77 and only one record has this perfect aspect ratio. 
# Also, for ratings > 6, the aspect ratio range is 1.18 to 2.76, 4 and 16 
# for aspect ratio 16 all the imdb scores are > 6 and for aspect ratio 4 imdb scores are > 5.8
# from 327 blanks, 184 records have imdb score > 6 

#rm(IMDB.subset)
IMDB.subset <- as.data.frame(IMDB.data,stringsAsFactors=FALSE)
IMDB.subset <- subset(IMDB.subset, select = -c(aspect_ratio))
str(IMDB.subset)

# Dealing with 0's

#We notice that there are some 0 values which should also be regarded as missing 
# value except for predictor facenumber_in_poster.
# First we need to replace NA with column average for facenumber_in_poster, 
# then replace 0s in other predictors with NA, and lastly replace all NAs with
# their respective column mean.
# hist(IMDB.subset$facenumber_in_poster, breaks = 80)
# replace NA with column average or mean for facenumber_in_poster
IMDB.subset$facenumber_in_poster[is.na(IMDB.subset$facenumber_in_poster)] <- 
  round(mean(IMDB.subset$facenumber_in_poster, na.rm = TRUE))

colSums(sapply(IMDB.data, is.na))

# convert 0s into NAs for other predictors
IMDB.subset[,c(5,6,8,14,16,25,27)][IMDB.subset[,c(5,6,8,14,16,25,27)] == 0] <- NA

dim(IMDB.subset)

IMDB.subset$facenumber_in_poster[is.na(IMDB.subset$facenumber_in_poster)] <- 
  round(mean(IMDB.subset$facenumber_in_poster, na.rm = TRUE))
# impute missing value with column mean
IMDB.subset$num_critic_for_reviews[is.na(IMDB.subset$num_critic_for_reviews)] <- 
  round(mean(IMDB.subset$num_critic_for_reviews, na.rm = TRUE))
IMDB.subset$num_user_for_reviews[is.na(IMDB.subset$num_user_for_reviews)]<-
  round(mean(IMDB.subset$num_user_for_reviews, na.rm = TRUE))
IMDB.subset$duration[is.na(IMDB.subset$duration)] <- round(mean(IMDB.subset$duration, na.rm = TRUE))
IMDB.subset$director_facebook_likes[is.na(IMDB.subset$director_facebook_likes)] <- 
  round(mean(IMDB.subset$director_facebook_likes, na.rm = TRUE))
IMDB.subset$actor_3_facebook_likes[is.na(IMDB.subset$actor_3_facebook_likes)] <- 
  round(mean(IMDB.subset$actor_3_facebook_likes, na.rm = TRUE))
IMDB.subset$actor_1_facebook_likes[is.na(IMDB.subset$actor_1_facebook_likes)] <- 
  round(mean(IMDB.subset$actor_1_facebook_likes, na.rm = TRUE))
IMDB.subset$cast_total_facebook_likes[is.na(IMDB.subset$cast_total_facebook_likes)] <- 
  round(mean(IMDB.subset$cast_total_facebook_likes, na.rm = TRUE))
IMDB.subset$actor_2_facebook_likes[is.na(IMDB.subset$actor_2_facebook_likes)] <- 
  round(mean(IMDB.subset$actor_2_facebook_likes, na.rm = TRUE))
IMDB.subset$movie_facebook_likes[is.na(IMDB.subset$movie_facebook_likes)] <- 
  round(mean(IMDB.subset$movie_facebook_likes, na.rm = TRUE))

# Sort out content ratings
table(IMDB.subset$content_rating)
# Blanks should be taken as missing value. Since these missing values cannot be 
# replaced with 
# reasonable data, we delete these rows.
IMDB.subset <- IMDB.subset[!(IMDB.subset$content_rating %in% ""),]
dim(IMDB.subset)

# According to the history of naming these different content ratings, we find M = GP = PG, X = NC-17.
# We want to replace M and GP with PG, replace X with NC-17, because these two are what we use nowadays.
# Check for TV-X values
IMDB.subset$content_rating[IMDB.subset$content_rating=='M'] <- 'PG'
IMDB.subset$content_rating[IMDB.subset$content_rating=='GP'] <- 'PG'
IMDB.subset$content_rating[IMDB.subset$content_rating=='X'] <- 'NC-17'

# We want to replace “Approved”, “Not Rated”, “Passed”, “Unrated” with the most common rating “R”.
IMDB.subset$content_rating[IMDB.subset$content_rating == 'Approved']  <- 'R' 
IMDB.subset$content_rating[IMDB.subset$content_rating == 'Not Rated'] <- 'R' 
IMDB.subset$content_rating[IMDB.subset$content_rating == 'Passed']    <- 'R' 
IMDB.subset$content_rating[IMDB.subset$content_rating == 'Unrated']   <- 'R' 


# We want to replace “TV-14”, “TV-G”, “TV-MA”, “TV-PG”, "TV-Y", "TV-Y7" with “TV”.
# cr <- data.frame(IMDB.subset)
#levels(cr$content_rating) <- c(levels(cr$content_rating),"TV")
#cr$content_rating[(cr$content_rating != 'G')&(cr$content_rating != 'NC-17')&
#                           (cr$content_rating != 'PG')&(cr$content_rating != 'PG-13')&(cr$content_rating != 'R')] <- 'TV'
#cr$content_rating <- factor(cr$content_rating)
#table(cr$content_rating)


# creating a new value for grouping all TV variables to TV
levels(IMDB.subset$content_rating) <- c(levels(IMDB.subset$content_rating),"TV")
IMDB.subset$content_rating[(IMDB.subset$content_rating!='G')&(IMDB.subset$content_rating != 'NC-17')&
                             (IMDB.subset$content_rating != 'PG')&(IMDB.subset$content_rating != 'PG-13')
                           &(IMDB.subset$content_rating != 'R')] <- 'TV'
IMDB.subset$content_rating <- factor(IMDB.subset$content_rating)
table(IMDB.subset$content_rating)
dim(IMDB.subset)

# have gross and budget information. So let’s add two colums: profit and percentage 
# return on investment for further analysis.

IMDB.subset <- IMDB.subset %>% 
  mutate(profit = gross - budget,
         return_on_investment_perc = (profit/budget)*100)


# Determining which predictors can be removed
table(IMDB.subset$color)

# 95.63% movies are colored, which indicates that this predictor is nearly constant.
#Let’s remove this predictor.
colSums(sapply(IMDB.data, is.na))
movie.data <- data.frame(IMDB.subset)
movie.data <- subset(movie.data,select = -c(color))

#movie.data1 <- read.xlsx("D:/Sem 3/ankit/Final Project/movie_data.xlsx",sheetName = "Sheet1")
#read.csv("D:/Sem 3/ankit/Final Project/movie_data.csv")
# is language and country important factor to predict imdb score?
table(movie.data$language)
movie.data

# 94.74% movies are in English, which means this variable is nearly constant. Let’s remove it.

movie.data <- subset(movie.data,select = -c(language))
table(movie.data$country)

#confirm the following stats
# Around 79% movies are from USA, 8% from UK, 13% from other countries. So we group other countries 
# together to make this categorical variable with less levels: USA, UK, Others.

levels(movie.data$country) <- c(levels(movie.data$country),"Others")
movie.data$country[(movie.data$country != 'USA')&(movie.data$country != 'UK')] <- 'Others'
movie.data$country <- factor(movie.data$country)
table(movie.data$country)

# Data Visualization

# Movie production just exploded after year 1990. It could be due to advancement in 
# technology and commercialisation of internet.

ggplot(movie.data, aes(title_year)) +
  geom_bar()+
  labs(x="Year movie was released", y = "Movie Count", title = " Histogram of movie released") + 
  theme(plot.title = element_text(hjust=0.5))

#From the graph, we see there aren’t many records of movies released before 1980. It’s better to remove those records because they might not be representative.

movie.data <- movie.data[movie.data$title_year >= 1980,]
table(movie.data$title_year)
dim(movie.data)

# Top 20 movies based on its profit
movie.data %>%
  filter(title_year %in% c(2000:2016)) %>%
  arrange(desc(profit)) %>%
  top_n(20, profit) %>%
  ggplot(aes(x=budget/1000000, y=profit/1000000)) +
  geom_point() +
  geom_smooth() + 
  geom_text_repel(aes(label=movie_title)) +
  labs(x = "Budget $million", y = "Profit $million", title = "Top 20 Profitable Movies") +
  theme(plot.title = element_text(hjust = 0.5))

# These are the top 20 movies based on the Profit earned (Gross - Budget). 
#It can be inferred from this plot that high budget movies tend to earn more profit.
# The trend is almost linear, with profit increasing with the increase in budget.

# Top 20 movies based on return on investment
movie.data %>%
  filter(budget > 100000) %>%
  mutate(profit = gross - budget,
         return_on_investment_perc = (profit/budget)*100) %>%
  arrange(desc(profit)) %>%
  top_n(20, profit) %>%
  ggplot(aes(x=budget/1000000, y = return_on_investment_perc)) + 
  geom_point(size = 2) + 
  geom_smooth(size = 1) + 
  geom_text_repel(aes(label = movie_title), size = 3) + 
  xlab("Budget $million") + 
  ylab("Percent Return on Investment") + 
  ggtitle("20 Most Profitable Movies based on its Return on Investment")
# These are the top 20 movies based on its Percentage Return on Investment 
#((profit/budget)*100).

#Since profit earned by a movie does not give a clear picture about its monetary success over the years, 
#this analysis, over the absolute value of the Return on Investment(ROI) across its Budget, would provide better results.

#As hypothesized, the ROI is high for Low Budget Films and decreases as the 
#budget of the movie increases.

#Top 20 directors with highest average IMDB score

movie.data %>%
  group_by(director_name) %>%
  summarise(avg_imdb = mean(imdb_score)) %>%
  arrange(desc(avg_imdb)) %>%
  top_n(20, avg_imdb) %>%
  formattable(list(avg_imdb = color_bar("orange")), align = 'l')

# Commercial Success v.s. Critical Acclaim
movie.data %>%
  top_n(20, profit) %>%
  ggplot(aes(x = imdb_score, y = gross/10^6, size = profit/10^6, 
             color = content_rating)) + 
  geom_point() + 
  geom_hline(aes(yintercept = 600)) + 
  geom_vline(aes(xintercept = 7.75)) + 
  geom_text_repel(aes(label = movie_title), size = 4) +
  xlab("Imdb score") + 
  ylab("Gross money earned in million dollars") + 
  ggtitle("Commercial success Vs Critical acclaim") +
  annotate("text", x = 8.5, y = 700, label = "High ratings \n & High gross") +
  theme(plot.title = element_text(hjust = 0.5))

# This is an analysis on the Commercial Success acclaimed by the movie (Gross earnings and profit earned) v.s. its IMDB Score.

#As expected, there is not much correlation since most critically 
#acclaimed movies do not do much well commercially.

# Relation between number of facebook likes and imdb_score

movie.data %>%
  plot_ly(x = ~movie_facebook_likes, y = ~imdb_score, color = ~content_rating ,
          mode = "markers", text = ~content_rating, alpha = 0.7, type = "scatter")

# We divide this scatter plot by content-rating. Movie with extremely high Facebook 
# likes tend to have higher imdb score. 
# But the score for movie with low Facebook likes vary in a very wide range.

# Data Pre-Processing
# Determining if we want to keep names as predictors or not

# number of directors
sum(uniqueN(movie.data$director_name))

# number of unique actors
sum(uniqueN(movie.data$actor_1_name))

sum(uniqueN(movie.data$actor_2_name))
sum(uniqueN(movie.data$actor_3_name))
sum(uniqueN(movie.data$plot_keywords))
# Almost more than 80% of data is unique
# Since all the names are so different for the whole dataset, there is no point to use names to predict score.

# Same with plot keywords, they are too diverse to be used in the prediction.

# And movie link is also a redundant variable.
typeof(movie.data)
colSums(sapply(movie.data, is.na))
#write.xlsx(data.subset, "D:/Sem 3/ankit/Final Project/data_subset1.xlsx")
data.subset <- data.frame(movie.data)
data.subset <- subset(data.subset, select = -c(director_name, actor_2_name, actor_1_name,
                                               movie_title, actor_3_name, plot_keywords, movie_imdb_link))
# Remove linear dependent variable
data.subset <- subset(data.subset, select = -c(profit,return_on_investment_perc))

# Remove Highly Correlated Variables
ggcorr(data.subset, label = TRUE, label_round = 2, label_size = 3.5, size = 2, hjust = .85) +
  ggtitle("Correlation Heatmap") +
  theme(plot.title = element_text(hjust = 0.5))
# Based on the heatmap, we can see some high correlations (greater than 0.7) between predictors.

#According to the highest correlation value 0.95, we find actor_1_facebook_likes is highly correlated with the cast_total_facebook_likes, 
#and both actor2 and actor3 are also somehow correlated to the total. So we want to modify them into two variables: actor_1_facebook_likes and other_actors_facebook_likes.

#There are high correlations among num_voted_users, num_user_for_reviews and num_critic_for_reviews. We want to keep num_voted_users and take the ratio of num_user_for_reviews and num_critic_for_reviews.
#add up actor 2 and 3 facebook likes into other actors facebook likes
data.subset$other_actors_facebook_likes <- data.subset$actor_2_facebook_likes + data.subset$actor_3_facebook_likes
# use the ratio of critical reviews amount to total reviews amount
data.subset$critic_review_ratio <- data.subset$num_critic_for_reviews / data.subset$num_user_for_reviews
# delete columns
data.subset <- subset(data.subset, select = -c(cast_total_facebook_likes, actor_2_facebook_likes, 
                                               actor_3_facebook_likes, num_critic_for_reviews, 
                                               num_user_for_reviews))
ggcorr(data.subset, label = TRUE, label_round = 2, label_size = 4, size = 3, hjust = .85) +
  ggtitle("Correlation Heatmap") +
  theme(plot.title = element_text(hjust = 0.5))
# We don’t see any strong correlation (absolute value greater than 0.7) any more.

# Bin Response Variable

#Our goal is to build a model, which can help us predict if a movie is good or bad. 
#So we don’t really want an exact score to be predicted, we only want to know how 
#good or how bad is the movie. Therefore, we bin the score into 3 buckets: 
#less than 5, 5~7 and 7~10, 
#which represents bad, good and excellent respectively.

data.subset$binned_score <- cut(data.subset$imdb_score, breaks = c(0,5,7,10))

# Organize the dataset
# We want to reorder the columns to make the dataset easier to be understood. 
# And we also renamed the columns to make the names shorter.
data.subset <- subset(data.subset,select = -c(genres))
data.subset <- data.subset[,c(9,4,5,14,12,2,3,13,1,6,10,7,8,11,15)]
colnames(data.subset) <- c("budget", "gross", "user_vote", "critic_review_ratio",
                           "movie_fb", "director_fb", "actor1_fb", "other_actors_fb",
                           "duration", "face_number", "year", "country", "content",
                           "imdb_score", "binned_score")




# Implementing Algorithms:

# 1) Classification Tree --> Fully grown Tree

library(rpart)
library(rpart.plot)
# Fully grown tree
tree <-data.subset
class.tree <- rpart(binned_score ~ . -imdb_score, data=tree, method = "class")
# plot tree
prp(class.tree, type = 1, extra = 1, under = TRUE, split.font=2, varlen = 0)
printcp(class.tree)

# Best-pruned tree

# cross validation procedure
# argument cp sets the smallest value for the complexity parameter.

set.seed(51)
cv.pruned.tree <- rpart(binned_score ~ . -imdb_score, data = tree, 
                        method = "class", cp=0.00001, minsplit = 5, xval = 5)
printcp(cv.pruned.tree)
# The 13th tree has the lowest cross-validation error (xerror): 0.76534

# prune by lowest cp
pruned.ct <- prune(cv.pruned.tree, 
                   cp = cv.pruned.tree$cptable[which.min(cv.pruned.tree$cptable
                                                         [,"xerror"]),"CP"])
length(pruned.ct$frame$var[pruned.ct$frame$var == "<leaf>"])
prp(pruned.ct, type = 1, extra = 1, split.font = 100, varlen = -10)

# Split the data into train and test set
siz <- round(.8 * dim(data.subset)[1])  # training set size
training_set <- data.subset[1:siz,]
testing_set <- data.subset[-(1:siz),]

# apply model on training set
tree.pred.train <- predict(pruned.ct, training_set, type = "class")
# generate confusion matrix for training data
confusionMatrix(tree.pred.train, training_set$binned_score)

# apply model on test set
tree.pred.test <- predict(pruned.ct, testing_set, type = "class")
# generate confusion matrix for test data
confusionMatrix(tree.pred.test, testing_set$binned_score)

# 2) Classification Algorithm ---> KNN Algo

# First, we need to prepare our data for applying knn purpose. Dummy variables are required for 
#categorical variables. 
# We use a copy of our data, so we can still use our original data in the future.
#rm(accuracy.df)
library(FNN)
# Use model.matrix() to create dummy variables for country and content.
knn.data <- data.subset
str(knn.data)
knn.data$country <- as.factor(knn.data$country)
knn.data$content <- as.factor(knn.data$content)
knn.data[,c("country_UK", "country_USA", "country_Others")] <- 
  model.matrix( ~ country - 1, data = knn.data)
knn.data[,c("content_G", "content_NC17", "content_PG", "content_PG13", 
            "content_R","content_TV")] <- model.matrix( ~ content - 1, data = knn.data)

# For Normalization
knn.data.norm <- knn.data
#extract numerical variables
num.vars <- sapply(knn.data.norm, is.numeric)
knn.data.norm[num.vars] <- lapply(knn.data.norm[num.vars], scale)
set.seed(123)
train_control<-trainControl(method = "cv", number = 10)
# fix parameters of algo
grid <- expand.grid(k=1:10)
m1_knn <- train(binned_score ~., data = knn.data.norm, trControl = train_control, 
                method="knn", 
                tuneGrid=grid)
# summarize result
print(m1_knn)


#3) Classification Algorithm --> Random Forest


library(randomForest)
rm(rf)
rf.data <- data.subset
set.seed(53)
rf <- randomForest(binned_score ~ . -imdb_score, data =rf.data , mtry = 5)
typeof(rf)
# Show model error
plot(rf)
legend('topright', colnames(rf$err.rate), col=1:5, fill=1:5)

# The black line shows the overall error rate which falls below 30%. 
# The red, green, blue lines show the error rate for bad,  good and excellent 
# movies respectively. We can see that right now we’re much more successful 
# predicting good movies. We cannot predict bad movies very well.

# Let’s look at relative variable importance by plotting the mean decrease in 
# Gini calculated across all trees.

# Get importance
importance <- importance(rf)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

# Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_few()
# From the plot, we see User_vote is a very important variable, while face_number, content and country are not so important.


dim(data.subset)
rf.data <- data.subset
controlParameters <- trainControl(method = "cv",number = 10)
paramGrid <- expand.grid(mtry=c(2,3,4,5))
m1.rf <- train(binned_score ~., data = rf.data, method="rf", 
               trControl = controlParameters, tuneGrid = paramGrid)

print(m1.rf)


rf.predict <- predict(m1.rf, rf.data)

confusionMatrix(rf.predict, rf.data$binned_score)

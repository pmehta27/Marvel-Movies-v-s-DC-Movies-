# Marvel vs DC (Data Analysis | Classification | R Programming)

## Introduction

Success of a movie is dependent on the high profits gained and there are various factors that contibute to the huge profits. Also, Movie Makers invest lot of money in making a movie but some movies fail to yield the title of a "BLOCK-BUSTER" film.

Hence, in this project, we will use a number of different supervised algorithms to determine the features that contribute to success of a movie using IMDB Movies Dataset.

IMDb aka Internet Movie Database is an online database of information for Movies, Games, TV Series etc. worldwide rated on a scale of 1-10. Based on these ratings(IMDB Score) a movie is considered as a success or a flop. (Ex: Avengers: Infinity War 8.5/10 And Green Lantern: 5.5/10).Marvel movies have high rating as compared to DC movies and these rating are given by people who share their experience in form of online movie-based review hosting websites and IMDB is the best source for that. The success/failure of a movie depends on several factors that include actors, directors, budget, genre, plot etc. The dataset consist of all this information and IMDB score for 5000+ movies from 60+ countries. Hence, I am building a model that will predict the movies IMDB score and determine if it will be a Success, Average or a Flop based on it’s IMDB Scores. 

## Motivation
To make a high-grossing success film depends on  several factors. But how can we assure the movie reception to the audience will be good?
Movie makers need to learn from their past mistakes to avoid same mistakes in the future.
Hence, we are proposing a way to determine significance of features that affect the rating IMDB score.
And we will classify the movies as per IMDB scores as:
* Good(7-10)
* Average(5-6)
* Bad(1-5)

## Details about project

In this project there are 5043 observations and 28 variables. The predictor variables are director_name, actor_2_name, color, genres, actor_1_name, movie_title, actor_3_name, plot_keywords, language, country, content_rating, movie_imdb_link, num_critic_for_reviews, duration, director_facebook_likes, actor_3_facebook_likes, actor_1_facebook_likes, gross, num_voted_users, cast_total_facebook_likes, facenumber_in_poster, num_user_for_reviews, budget, title_year, actor_2_facebook_likes, aspect_ratio, movie_facebook_likes and the response variable is "imdb_score".
This dataset is kind of messy, handled missing values by imputing with mean or median. Performed Exploratory Data Analysis to understand the nature of data. 
Used following three classification models to determine features that affect the IMDB Score:
* Decision Trees
* KNN
* Random Forest

## Conclusion

* In comparison with all the models-built Random Forest has the highest accuracy and is optimum for predicting imdb score of a movie.
* The high imdb score of movie depends on several factors but most importantly users’ votes, budget and duration matter the most as well  as the budget of the movie.
* Outcomes of this project can help the movie makers understand factors that lead to a successful and more profit-based revenue movies. 

## Contact
Created by [@pmehta27](https://www.linkedin.com/in/pooja-ym-mehta) - Please feel free to contact me, thank you for your time!







Nobel Price and Democracy
================
Jenn Lau
2020-12-11

## Overview

### Real-world question

In this project, the relationship between the birth and death place of
Nobel prize laureates and their Nobel prize will be studied. I will look
specifically into the level of democracy of each country with the help
of The Economist Democracy Index, a single value index that summarize
the level of democracy in every country.

<Modelling question here>

<An analysis of the appropriateness of your dataset for addressing these questions>

### Data Source

The [Nobel prize
dataset](https://www.kaggle.com/nobelfoundation/nobel-laureates)
contains details of every Nobel prize laureates and some details about
them while the [Economist Democracy
Index](https://en.wikipedia.org/wiki/Democracy_Index) is a single value
measurement of the level of Democracy of each country,

## Approach

### Problem Description

The two dataset used in this project is a Nobel prize dataset which
include every Nobel prize laureates from to 1901 to 2019 which also
include some details of the laureates such as award date, category,
where the they are born and where they died. The second dataset is a
index of every country with created by The Economist where each country
is scored out of 6 categories and the average of which is the “democracy
index”.

### Data

The Economist Democracy Index is scraped from Wikipedia with some
cleaning done to the table.

``` r
# Read the html file
scraped_democracy <- read_html("https://en.wikipedia.org/wiki/Democracy_Index")

# tables are scraped from the html file
scraped_tables <- scraped_democracy %>%
  html_nodes(".wikitable")

# the table needed is selected and wrangled with the correct data type and to give clean labels that is suitable for R.
democracy <- scraped_tables[[1]] %>% #selecting the right table from the different tables
  html_table() %>%
  select(2:9) %>% # selecting the 3rd to 10th column
  set_names(c("country", "score", "electorial_process_and_pluralism", 
              "functioning_of_government", "political_participation", 
              "political_culture", "civil_liberties", "regime_type")) %>% # changing the name to names that i suitable for R
  slice_head(n = nrow(.)-1) %>% # remove footer
  mutate(score = as.numeric(score), 
         electorial_process_and_pluralism = as.numeric(electorial_process_and_pluralism),
         functioning_of_government = as.numeric(functioning_of_government),
         political_participation = as.numeric(political_participation),
         political_culture = as.numeric(political_culture),
         civil_liberties = as.numeric(civil_liberties)
         ) %>% # changing to the correct data type
  mutate(country = fct_recode(country, 'South Korea' = "South Korea[n 2]")) # remove superscript from content scraped
```

Democracy index score and all 6 sub-index score is renamed into its
apropriate names.

``` r
democracy_birth <- democracy %>%
  rename(birth_country = country, 
         birth_country_score = score,
         birth_country_electorial_pluralism = electorial_process_and_pluralism,
         birth_country_government_function = functioning_of_government, 
         birth_country_political_participation = political_participation, 
         birth_country_political_culture = political_culture, 
         birth_country_liberties = civil_liberties, 
         birth_country_regime_type = regime_type)

# commented out for future use
# democracy_death <- democracy %>%
#   rename(death_country = country, 
#          death_country_score = score,
#          death_country_electorial_pluralism = electorial_process_and_pluralism,
#          death_country_government_function = functioning_of_government, 
#          death_country_political_participation = political_participation, 
#          death_country_political_culture = political_culture, 
#          death_country_liberties = civil_liberties, 
#          death_country_regime_type = regime_type)
```

``` r
nobel_original <- read_csv("data/complete.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character(),
    ##   awardYear = col_double(),
    ##   sortOrder = col_double(),
    ##   prizeAmount = col_double(),
    ##   prizeAmountAdjusted = col_double(),
    ##   dateAwarded = col_date(format = ""),
    ##   id = col_double(),
    ##   birth_date = col_date(format = ""),
    ##   death_date = col_date(format = ""),
    ##   org_founded_date = col_date(format = "")
    ## )

    ## See spec(...) for full column specifications.

    ## Warning: 32 parsing failures.
    ## row              col   expected     actual                file
    ##   1 birth_date       valid date 1943-00-00 'data/complete.csv'
    ##  27 birth_date       valid date 1898-00-00 'data/complete.csv'
    ##  48 org_founded_date valid date 1917-00-00 'data/complete.csv'
    ##  49 org_founded_date valid date 1961-00-00 'data/complete.csv'
    ## 237 org_founded_date valid date 1952-00-00 'data/complete.csv'
    ## ... ................ .......... .......... ...................
    ## See problems(...) for more details.

The Nobel prize dataset is read and organization Nobel prize receivers
is filtered out as it is difficult to pinpoint exactly what country the
group is recievers is from. Necessary rows is then filtered out for use,
with some factor recode done to match the country names provided in
democracy index dataset. 1row of data from Saint Lucia is filtered out
due to The Economist index not having it indexed.There is also 0row of
data that will be filtered out due to it being owned by France but is
located in the continent of North America.

``` r
nobel <- nobel_original %>%
  filter(ind_or_org == "Individual") %>%
  select(awardYear, category, gender, birth_cityNow, birth_countryNow,
         birth_continent, death_cityNow, death_countryNow, death_continent) %>% #selecting relevant variables for this study
  rename(birth_city = birth_cityNow, birth_country = birth_countryNow,
         death_city = death_cityNow, death_country = death_countryNow) %>% 
  mutate(birth_country = fct_recode(birth_country, 'Netherlands' = 'the Netherlands',
                                    'Ireland' = 'Northern Ireland', 'United Kingdom' = 'Scotland',
                                    'United States' = 'USA', 'Timor-Leste' = 'East Timor',
                                    'Denmark' = 'Faroe Islands (Denmark)',
                                    'France' = 'Guadeloupe Island')) %>%
  mutate(death_country = fct_recode(death_country, 'Netherlands' = 'the Netherlands',
                                    'Ireland' = 'Northern Ireland', 'United Kingdom' = 'Scotland',
                                    'United States' = 'USA', 'Timor-Leste' = 'East Timor',
                                    'Denmark' = 'Faroe Islands (Denmark)',
                                    'France' = 'Guadeloupe Island')) %>%
  filter(birth_country != "Saint Lucia") %>%
  filter(birth_city != "Pointe-à-Pitre")
```

    ## Warning: Problem with `mutate()` input `death_country`.
    ## i Unknown levels in `f`: East Timor, Faroe Islands (Denmark), Guadeloupe Island
    ## i Input `death_country` is `fct_recode(...)`.

    ## Warning: Unknown levels in `f`: East Timor, Faroe Islands (Denmark), Guadeloupe
    ## Island

``` r
nobel <- nobel %>% select(-death_city, -death_country, -death_continent) # filter out death country details
```

Nobel prize data is then split into 6 dataset by their category, then a
calculation of total\_nobel\_prize in that category earned by each
country is done

``` r
split_category <- function(input_data, nobel_category) {
  input_data %>%
  filter(category == nobel_category) %>%
  group_by(birth_country) %>%
  summarize(total_nobel_prize = n()) %>%
  left_join(nobel %>% select(birth_country, birth_continent) %>% unique())
}

nobel_physics <- split_category(nobel, "Physics")
```

    ## Joining, by = "birth_country"

``` r
nobel_chemistry <- split_category(nobel, "Chemistry")
```

    ## Joining, by = "birth_country"

``` r
nobel_peace <- split_category(nobel, "Peace")
```

    ## Joining, by = "birth_country"

``` r
nobel_literature <- split_category(nobel, "Literature")
```

    ## Joining, by = "birth_country"

``` r
nobel_economics <- split_category(nobel, "Economic Sciences")
```

    ## Joining, by = "birth_country"

``` r
nobel_medicine <- split_category(nobel, "Physiology or Medicine")   
```

    ## Joining, by = "birth_country"

all the data is then merged, a new column of total nobel prize each
country recieved is also added, then observations without death recorded
is filtered out.

``` r
merged_physics <- left_join(nobel_physics, democracy_birth)
```

    ## Joining, by = "birth_country"

``` r
merged_chemistry <- left_join(nobel_chemistry, democracy_birth)
```

    ## Joining, by = "birth_country"

``` r
merged_peace <- left_join(nobel_peace, democracy_birth)
```

    ## Joining, by = "birth_country"

``` r
merged_literature <- left_join(nobel_literature, democracy_birth)
```

    ## Joining, by = "birth_country"

``` r
merged_economics <- left_join(nobel_economics, democracy_birth)
```

    ## Joining, by = "birth_country"

``` r
merged_medicine <- left_join(nobel_medicine, democracy_birth)
```

    ## Joining, by = "birth_country"

A overall data is also produced without splitting into the six Nobel
prize categories is done, this is used for analysis section for a
overarching overview of the data. Total Nobel prize earned for each
country is calculated here regardless of what category the Nobel prize
is in.

``` r
merged <- left_join(nobel, democracy_birth) 
```

    ## Joining, by = "birth_country"

``` r
# commented out for death analysis
# merged <- merged %>% 
#   left_join(democracy_death) %>% 

merged_sum <- merged %>%
  group_by(birth_country) %>%
  summarize(total_nobel_prize = n())

merged_continent <- left_join(merged_sum, merged %>% select(birth_country, birth_continent, birth_country_score) %>% unique())
```

    ## Joining, by = "birth_country"

The decision made to use the birth country of the Laureats onlt is
because there is 333 rows of data out of 950 rows of data that do not
contain death country. That is approximately 35.1% the data which is a
significant portion.

Note: of the data wrangled here is for both birth and death country,
wrangling for death country will be commented out for this project but
could be used further down.

#### Provenance

The Nobel prize dataset is downloaded from Kaggle which is sourced from
the Nobel Prize API. Unfortunately The Economist democracy index is not
easily accessible to the general public, hence it is scraped from
Wikipedia. The index is double checked with a
[PDF](https://www.economist.com/media/pdf/democracy_index_2007_v3.pdf)
released by The Economist.

#### Structure

In the resulting six dataset `merged` there is 919 rows of data for all
of them combined with each row represent one Nobel prize laureates. Then
there are 10 variables where the variables details is as followed:

``` r
variables <- names(merged)

type <- c("ordinal", "nominal", "nominal", "nominal", "nominal", "nominal", "nominal", "nominal", "nominal", "ordinal", "ordinal", "ordinal", "ordinal")

merged_variables <- data.frame(variables, type, stringsAsFactors=TRUE)

merged_variables %>% knitr::kable()
```

| variables                                | type    |
| :--------------------------------------- | :------ |
| awardYear                                | ordinal |
| category                                 | nominal |
| gender                                   | nominal |
| birth\_city                              | nominal |
| birth\_country                           | nominal |
| birth\_continent                         | nominal |
| birth\_country\_score                    | nominal |
| birth\_country\_electorial\_pluralism    | nominal |
| birth\_country\_government\_function     | nominal |
| birth\_country\_political\_participation | ordinal |
| birth\_country\_political\_culture       | ordinal |
| birth\_country\_liberties                | ordinal |
| birth\_country\_regime\_type             | ordinal |

Other than that, for the data split into 6 categories I still have total
number of 919 rows and I have variables names of:

``` r
variables <- names(merged_physics)

type <- c("nominal", "numerical", "nominal", "ordinal", "ordinal", "ordinal", "ordinal", "ordinal", "ordinal", "ordinal")

merged_variables <- data.frame(variables, type, stringsAsFactors=TRUE)

merged_variables %>% knitr::kable()
```

| variables                                | type      |
| :--------------------------------------- | :-------- |
| birth\_country                           | nominal   |
| total\_nobel\_prize                      | numerical |
| birth\_continent                         | nominal   |
| birth\_country\_score                    | ordinal   |
| birth\_country\_electorial\_pluralism    | ordinal   |
| birth\_country\_government\_function     | ordinal   |
| birth\_country\_political\_participation | ordinal   |
| birth\_country\_political\_culture       | ordinal   |
| birth\_country\_liberties                | ordinal   |
| birth\_country\_regime\_type             | ordinal   |

#### Appropriateness for task

The resulting dataset is appropriate for addressing the central
questioned presented above. It contains all the Nobel prize awards given
from 1901 to 2019, as well as democracy index and it’s all six
sub-indexes.

### Modeling Question and Approach

The main modeling attempt will be to use democracy index to predict if a
country’s total number of Nobel prize. A second consideration would be
to use continent as a predicting factor.

## Exploratory Data Analysis

### Individual Variables

Visualizing the mean and spread of all six category of Nobel prize. I
can see how there is a different Standard deviation and mean for the
different Nobel prize category. Interesting thing to note is how
Economics and Medicine has higher mean than the rest and Peace and
Physics have higher Spread. One guess of why peace have a larger spread
may be attributed to how people from less democratic countries can have
many activist that is looked upon. However, Nobel prize of peace can
also be argued to disprove that. It is difficult to see how Physics is
any different than Medicine since both are highly technical fields that
is also very experimental.

``` r
histplot <- function(input_data, title) {
  input_data %>%
  ggplot(aes(x = birth_country_score)) +
  geom_histogram() +
  labs(x = "Birth country score", y = "Total number of countries", title = title) +
  geom_vline(aes(xintercept = mean(birth_country_score))) + 
  annotate("text", x = 4, y = 6, label = paste("Standard Deviation:", sd(input_data$birth_country_score)))
}

histplot(merged_physics, "Physics Nobel Prize")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

``` r
histplot(merged_chemistry, "Chemistry Nobel Prize")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

``` r
histplot(merged_peace, "Peace Nobel Prize")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

``` r
histplot(merged_literature, "Literature Nobel Prize")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

``` r
histplot(merged_economics, "Economics Nobel Prize")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

``` r
histplot(merged_medicine, "Medicine Nobel Prize")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

<img src="proj_files/figure-gfm/exploratory-by-category-1.png" width="50%" /><img src="proj_files/figure-gfm/exploratory-by-category-2.png" width="50%" /><img src="proj_files/figure-gfm/exploratory-by-category-3.png" width="50%" /><img src="proj_files/figure-gfm/exploratory-by-category-4.png" width="50%" /><img src="proj_files/figure-gfm/exploratory-by-category-5.png" width="50%" /><img src="proj_files/figure-gfm/exploratory-by-category-6.png" width="50%" />

We can also see how continent can be a factor, seeing how different
continents have different number of countries with different democracy
index. It is Interesting to note how Europe contains the most highly
democratic countries. However, do note that this do not consider the
population of each countries, just the number of countries.

``` r
continentplot <- function(input_data, title, continent_filter) {
  input_data %>% 
    filter(birth_continent == continent_filter) %>%
      ggplot(aes(x = birth_country_score)) +
      geom_histogram() +
      labs(x = "Birth country score", y = "Total number of countries", title = title) +
      xlim(0, 10.0) +
      geom_vline(aes(xintercept = mean(birth_country_score))) + 
      annotate("text", x = 2, y = 6, label = paste("Standard Deviation:", sd((input_data %>% filter(birth_continent == continent_filter))$birth_country_score)))
}

continentplot(merged_continent, "Physics Nobel Prize", "Africa")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

``` r
continentplot(merged_continent, "Physics Nobel Prize", "Asia")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

``` r
continentplot(merged_continent, "Physics Nobel Prize", "Europe")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

``` r
continentplot(merged_continent, "Physics Nobel Prize", "North America")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

``` r
continentplot(merged_continent, "Physics Nobel Prize", "Oceania")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

``` r
continentplot(merged_continent, "Physics Nobel Prize", "South America")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

<img src="proj_files/figure-gfm/exploratory-by-continent-1.png" width="50%" /><img src="proj_files/figure-gfm/exploratory-by-continent-2.png" width="50%" /><img src="proj_files/figure-gfm/exploratory-by-continent-3.png" width="50%" /><img src="proj_files/figure-gfm/exploratory-by-continent-4.png" width="50%" /><img src="proj_files/figure-gfm/exploratory-by-continent-5.png" width="50%" /><img src="proj_files/figure-gfm/exploratory-by-continent-6.png" width="50%" />

### Combinations of Variables

Looking back at Lab03, one main thing that should stand out is how
United States is have a large amount of Laureates which can affect the
plots and modeling done later on. Looking at the top 10, I can see
United States have two times the amount of Nobel prizes compared to the
United Kingdom who has less than half of what the United States
have.This is also shown in this plot.

``` r
merged_sum %>%
  top_n(n = 10, wt = total_nobel_prize) %>%
  arrange(desc(total_nobel_prize)) %>%
  rename("Birth country" = birth_country, "Total nobel prizes" = total_nobel_prize) %>%
  knitr::kable()
```

| Birth country  | Total nobel prizes |
| :------------- | -----------------: |
| United States  |                274 |
| United Kingdom |                 97 |
| Germany        |                 82 |
| France         |                 56 |
| Sweden         |                 29 |
| Poland         |                 28 |
| Japan          |                 27 |
| Russia         |                 26 |
| Canada         |                 20 |
| Italy          |                 19 |
| Switzerland    |                 19 |

``` r
corplot <- function(input_data, title) {
  input_data %>%
  ggplot(aes(x = birth_country_score, y = total_nobel_prize)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE) +
  labs(x = "Birth country score", y = "Total Nobel prize earned", title = title) +
  geom_text_repel(mapping = aes(label = birth_country), data = input_data %>% top_n(n = 3, wt = total_nobel_prize)) +
  stat_cor(aes(label = paste(..rr.label.., sep = "~`,`~")))
}

corplot(merged_physics, "Physics Nobel prize (with United States)")
```

    ## `geom_smooth()` using formula 'y ~ x'

![](proj_files/figure-gfm/top-nobel-prize-1.png)<!-- -->

Removing United States from the equation, I will try to fit all six
category of Nobel prize with a regression line.

``` r
corplot(merged_physics %>%  filter(birth_country != "United States"), "Physics Nobel prize (without United States)")
```

    ## `geom_smooth()` using formula 'y ~ x'

``` r
corplot(merged_chemistry %>%  filter(birth_country != "United States"), "Physics Nobel prize (without United States)")
```

    ## `geom_smooth()` using formula 'y ~ x'

``` r
corplot(merged_peace %>%  filter(birth_country != "United States"), "Physics Nobel prize (without United States)")
```

    ## `geom_smooth()` using formula 'y ~ x'

``` r
corplot(merged_literature %>%  filter(birth_country != "United States"), "Physics Nobel prize (without United States)")
```

    ## `geom_smooth()` using formula 'y ~ x'

``` r
corplot(merged_economics %>%  filter(birth_country != "United States"), "Physics Nobel prize (without United States)")
```

    ## `geom_smooth()` using formula 'y ~ x'

``` r
corplot(merged_medicine %>%  filter(birth_country != "United States"), "Physics Nobel prize (without United States)")
```

    ## `geom_smooth()` using formula 'y ~ x'

<img src="proj_files/figure-gfm/explore-bivariate-1.png" width="50%" /><img src="proj_files/figure-gfm/explore-bivariate-2.png" width="50%" /><img src="proj_files/figure-gfm/explore-bivariate-3.png" width="50%" /><img src="proj_files/figure-gfm/explore-bivariate-4.png" width="50%" /><img src="proj_files/figure-gfm/explore-bivariate-5.png" width="50%" /><img src="proj_files/figure-gfm/explore-bivariate-6.png" width="50%" />

We now know that the birth country score is not predictive of total
Nobel prize earned in the country. For every category the r-squared
values is very small.

## Modeling

A higher proportion of training data is selected due to the dataset
being very small

``` r
set.seed(1337)
merged_split <- initial_split(merged_peace %>%  filter(birth_country != "United States"), prop = 3/4)
merged_train <- training(merged_split)
merged_test <- testing(merged_split)
```

``` r
spec <-
  parsnip::linear_reg() %>%
  set_mode("regression") %>%
  set_engine("lm")
```

``` r
model1 <- spec %>%
  fit(formula = total_nobel_prize ~ birth_continent, data = merged_train) %>%
  step_dummy(birth_continent)
```

``` r
model1 %>%
  predict(merged_train) %>%
  bind_cols(merged_train %>% select(total_nobel_prize)) %>%
  yardstick::metrics(truth = total_nobel_prize, estimate = .pred)
```

    ## # A tibble: 3 x 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 rmse    standard       1.58 
    ## 2 rsq     standard       0.287
    ## 3 mae     standard       0.875

``` r
merged_resamples <- merged_train %>%
  vfold_cv(v = 10)

spec <- workflow() %>%
  add_recipe(
    recipe(total_nobel_prize ~ birth_continent, data = merged_train)
  ) %>%
  step_dummy(birth_continent) %>%
  add_model(
    linear_reg() %>%
      set_engine(engine = "lm")
  )

model2 <- spec %>%
  fit_resamples(resamples = merged_resamples, metrics = metric_set(mae))

#ncompute cross-validation results
model2 %>% collect_metrics(summarize = TRUE)
```

    ## # A tibble: 1 x 5
    ##   .metric .estimator  mean     n std_err
    ##   <chr>   <chr>      <dbl> <int>   <dbl>
    ## 1 mae     standard    1.01    10   0.191

A better model is attempted but given the data it only reduce it’s
accuracy

``` r
merged_resamples <- merged_train %>%
  vfold_cv(v = 10)

spec <- workflow() %>%
  add_recipe(
    recipe(total_nobel_prize ~ birth_continent + birth_country_score , data = merged_train)
  ) %>%
  step_dummy(birth_continente) %>%
  add_model(
    linear_reg() %>%
      set_engine(engine = "lm")
  )

model3 <- spec %>%
  fit_resamples(resamples = merged_resamples, metrics = metric_set(mae))

#ncompute cross-validation results
model3 %>% collect_metrics(summarize = TRUE)
```

    ## # A tibble: 1 x 5
    ##   .metric .estimator  mean     n std_err
    ##   <chr>   <chr>      <dbl> <int>   <dbl>
    ## 1 mae     standard    1.11    10   0.230

## Summary

From the factors observed, namely Laureates birth continent and category
of Nobel prize, I can conclude that the Nobel prize Laureates is a
fairly diverse in terms of birth continent and category. However there
is a rather significant anomaly. The United States is seen to have
magnitude of difference compared to other countries, even if only the
birth country is taken into account. The cause of this cannot be
determined in this study, but the fact is taken into consideration. This
is also seen in lab03 where death country is the factor considered,
where the United States have a even bigger magnitude of difference
compared to the rest of the country.

### Discussion of Findings

From the exploratory analysis I can visually see that that different
category of Nobel prize have a slightly different spread and average
distribution of Nobel prize. Placing the same dataset into a regression
plot shows that there is no correlation between category and democracy
index. This point is further proved in the modeling section. Then I
looked into the relationship between a country’s total Nobel prize and
continent, in the exploratory analysis I observed a more significant
difference. But in the modeling section, putting it to test, there is
actually a only a small relationship between the two variables.

### Limitations and Ethical Considerations

There are many assumptions that is made in this project, one of the most
significant one is that level of democracy of a country can change over
time. Another thing is how countries in the world do not stay constant
over time. Countries can collapse and rise which gives the
classification of older Nobel prize a debatable one. One prime example
of this is USSR. How its level of democracy have changed over time and
that it has split into multiple countries. This leads to a ethical
consideration as well, how disputed land can be problematic when
considering which country to rename to, which is necessary thing to do
when The Economist Index is used to study. This also show how the
Economist Index can be limited as well, but to be fair to The Economist,
it is not supposed to be a end all ranking but a useful approximation.

Other than that, The fact that there is 1/3 of the data that do not
contain death country also raises problems which may be due to the
Laureates being alive, or data is not available. We also would not know
where exactly Laureates spend most of their time researching Nobel prize
worthy research, as birth and death palace may not necessarily imply
where most of the work is produced in.

### Future Directions

Solving the some of the above problem by using or including other
democracy indices. Which could be fairer considering one index may
consider one factor to be more important than another. But thankfully
there is a few more indices available freely. To name a few there is
Democracy Ranking by Global Democracy Ranking which is a Austria based
index, or the Freedom in the World index produced by the U.S.
government, or the Global State of Democracy Indices. One can also
consider to merge the different score to get a average of them instead
of just using one.

A problem faced in this project is that the dataset that was wrangled
into the six Nobel prize category where each row represents one country,
city cannot be added to the data. So another possible look into the
project is to look at it from a city level rather than a country level.

Lastly, further project could also see if economic freedom such as one
from Index of Economic Freedom from The Heritage Foundation could be a
predicting factor of total nobel prize earned in a contry, Where
different aspects of economic freedom is compared rather than democracy.

## Appendix

``` r
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(readr)
library(rvest) # scrape HTML
library(tidymodels)
library(ggrepel) # ggplot improvement library for text handeling
library(ggpubr) # for including statistics in plots
options(dplyr.summarise.inform = FALSE) # silence a warning message
# Read the html file
scraped_democracy <- read_html("https://en.wikipedia.org/wiki/Democracy_Index")

# tables are scraped from the html file
scraped_tables <- scraped_democracy %>%
  html_nodes(".wikitable")

# the table needed is selected and wrangled with the correct data type and to give clean labels that is suitable for R.
democracy <- scraped_tables[[1]] %>% #selecting the right table from the different tables
  html_table() %>%
  select(2:9) %>% # selecting the 3rd to 10th column
  set_names(c("country", "score", "electorial_process_and_pluralism", 
              "functioning_of_government", "political_participation", 
              "political_culture", "civil_liberties", "regime_type")) %>% # changing the name to names that i suitable for R
  slice_head(n = nrow(.)-1) %>% # remove footer
  mutate(score = as.numeric(score), 
         electorial_process_and_pluralism = as.numeric(electorial_process_and_pluralism),
         functioning_of_government = as.numeric(functioning_of_government),
         political_participation = as.numeric(political_participation),
         political_culture = as.numeric(political_culture),
         civil_liberties = as.numeric(civil_liberties)
         ) %>% # changing to the correct data type
  mutate(country = fct_recode(country, 'South Korea' = "South Korea[n 2]")) # remove superscript from content scraped
democracy_birth <- democracy %>%
  rename(birth_country = country, 
         birth_country_score = score,
         birth_country_electorial_pluralism = electorial_process_and_pluralism,
         birth_country_government_function = functioning_of_government, 
         birth_country_political_participation = political_participation, 
         birth_country_political_culture = political_culture, 
         birth_country_liberties = civil_liberties, 
         birth_country_regime_type = regime_type)

# commented out for future use
# democracy_death <- democracy %>%
#   rename(death_country = country, 
#          death_country_score = score,
#          death_country_electorial_pluralism = electorial_process_and_pluralism,
#          death_country_government_function = functioning_of_government, 
#          death_country_political_participation = political_participation, 
#          death_country_political_culture = political_culture, 
#          death_country_liberties = civil_liberties, 
#          death_country_regime_type = regime_type)
nobel_original <- read_csv("data/complete.csv")
nobel <- nobel_original %>%
  filter(ind_or_org == "Individual") %>%
  select(awardYear, category, gender, birth_cityNow, birth_countryNow,
         birth_continent, death_cityNow, death_countryNow, death_continent) %>% #selecting relevant variables for this study
  rename(birth_city = birth_cityNow, birth_country = birth_countryNow,
         death_city = death_cityNow, death_country = death_countryNow) %>% 
  mutate(birth_country = fct_recode(birth_country, 'Netherlands' = 'the Netherlands',
                                    'Ireland' = 'Northern Ireland', 'United Kingdom' = 'Scotland',
                                    'United States' = 'USA', 'Timor-Leste' = 'East Timor',
                                    'Denmark' = 'Faroe Islands (Denmark)',
                                    'France' = 'Guadeloupe Island')) %>%
  mutate(death_country = fct_recode(death_country, 'Netherlands' = 'the Netherlands',
                                    'Ireland' = 'Northern Ireland', 'United Kingdom' = 'Scotland',
                                    'United States' = 'USA', 'Timor-Leste' = 'East Timor',
                                    'Denmark' = 'Faroe Islands (Denmark)',
                                    'France' = 'Guadeloupe Island')) %>%
  filter(birth_country != "Saint Lucia") %>%
  filter(birth_city != "Pointe-à-Pitre")


nobel <- nobel %>% select(-death_city, -death_country, -death_continent) # filter out death country details

split_category <- function(input_data, nobel_category) {
  input_data %>%
  filter(category == nobel_category) %>%
  group_by(birth_country) %>%
  summarize(total_nobel_prize = n()) %>%
  left_join(nobel %>% select(birth_country, birth_continent) %>% unique())
}

nobel_physics <- split_category(nobel, "Physics")
nobel_chemistry <- split_category(nobel, "Chemistry")
nobel_peace <- split_category(nobel, "Peace")
nobel_literature <- split_category(nobel, "Literature")
nobel_economics <- split_category(nobel, "Economic Sciences")
nobel_medicine <- split_category(nobel, "Physiology or Medicine")   
merged_physics <- left_join(nobel_physics, democracy_birth)
merged_chemistry <- left_join(nobel_chemistry, democracy_birth)
merged_peace <- left_join(nobel_peace, democracy_birth)
merged_literature <- left_join(nobel_literature, democracy_birth)
merged_economics <- left_join(nobel_economics, democracy_birth)
merged_medicine <- left_join(nobel_medicine, democracy_birth)
merged <- left_join(nobel, democracy_birth) 

# commented out for death analysis
# merged <- merged %>% 
#   left_join(democracy_death) %>% 

merged_sum <- merged %>%
  group_by(birth_country) %>%
  summarize(total_nobel_prize = n())

merged_continent <- left_join(merged_sum, merged %>% select(birth_country, birth_continent, birth_country_score) %>% unique())
variables <- names(merged)

type <- c("ordinal", "nominal", "nominal", "nominal", "nominal", "nominal", "nominal", "nominal", "nominal", "ordinal", "ordinal", "ordinal", "ordinal")

merged_variables <- data.frame(variables, type, stringsAsFactors=TRUE)

merged_variables %>% knitr::kable()
variables <- names(merged_physics)

type <- c("nominal", "numerical", "nominal", "ordinal", "ordinal", "ordinal", "ordinal", "ordinal", "ordinal", "ordinal")

merged_variables <- data.frame(variables, type, stringsAsFactors=TRUE)

merged_variables %>% knitr::kable()
histplot <- function(input_data, title) {
  input_data %>%
  ggplot(aes(x = birth_country_score)) +
  geom_histogram() +
  labs(x = "Birth country score", y = "Total number of countries", title = title) +
  geom_vline(aes(xintercept = mean(birth_country_score))) + 
  annotate("text", x = 4, y = 6, label = paste("Standard Deviation:", sd(input_data$birth_country_score)))
}

histplot(merged_physics, "Physics Nobel Prize")
histplot(merged_chemistry, "Chemistry Nobel Prize")
histplot(merged_peace, "Peace Nobel Prize")
histplot(merged_literature, "Literature Nobel Prize")
histplot(merged_economics, "Economics Nobel Prize")
histplot(merged_medicine, "Medicine Nobel Prize")
continentplot <- function(input_data, title, continent_filter) {
  input_data %>% 
    filter(birth_continent == continent_filter) %>%
      ggplot(aes(x = birth_country_score)) +
      geom_histogram() +
      labs(x = "Birth country score", y = "Total number of countries", title = title) +
      xlim(0, 10.0) +
      geom_vline(aes(xintercept = mean(birth_country_score))) + 
      annotate("text", x = 2, y = 6, label = paste("Standard Deviation:", sd((input_data %>% filter(birth_continent == continent_filter))$birth_country_score)))
}

continentplot(merged_continent, "Physics Nobel Prize", "Africa")
continentplot(merged_continent, "Physics Nobel Prize", "Asia")
continentplot(merged_continent, "Physics Nobel Prize", "Europe")
continentplot(merged_continent, "Physics Nobel Prize", "North America")
continentplot(merged_continent, "Physics Nobel Prize", "Oceania")
continentplot(merged_continent, "Physics Nobel Prize", "South America")
merged_sum %>%
  top_n(n = 10, wt = total_nobel_prize) %>%
  arrange(desc(total_nobel_prize)) %>%
  rename("Birth country" = birth_country, "Total nobel prizes" = total_nobel_prize) %>%
  knitr::kable()

corplot <- function(input_data, title) {
  input_data %>%
  ggplot(aes(x = birth_country_score, y = total_nobel_prize)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE) +
  labs(x = "Birth country score", y = "Total Nobel prize earned", title = title) +
  geom_text_repel(mapping = aes(label = birth_country), data = input_data %>% top_n(n = 3, wt = total_nobel_prize)) +
  stat_cor(aes(label = paste(..rr.label.., sep = "~`,`~")))
}

corplot(merged_physics, "Physics Nobel prize (with United States)")
corplot(merged_physics %>%  filter(birth_country != "United States"), "Physics Nobel prize (without United States)")
corplot(merged_chemistry %>%  filter(birth_country != "United States"), "Physics Nobel prize (without United States)")
corplot(merged_peace %>%  filter(birth_country != "United States"), "Physics Nobel prize (without United States)")
corplot(merged_literature %>%  filter(birth_country != "United States"), "Physics Nobel prize (without United States)")
corplot(merged_economics %>%  filter(birth_country != "United States"), "Physics Nobel prize (without United States)")
corplot(merged_medicine %>%  filter(birth_country != "United States"), "Physics Nobel prize (without United States)")
set.seed(1337)
merged_split <- initial_split(merged_peace %>%  filter(birth_country != "United States"), prop = 3/4)
merged_train <- training(merged_split)
merged_test <- testing(merged_split)
spec <-
  parsnip::linear_reg() %>%
  set_mode("regression") %>%
  set_engine("lm")
model1 <- spec %>%
  fit(formula = total_nobel_prize ~ birth_continent, data = merged_train) %>%
  step_dummy(birth_continent)
model1 %>%
  predict(merged_train) %>%
  bind_cols(merged_train %>% select(total_nobel_prize)) %>%
  yardstick::metrics(truth = total_nobel_prize, estimate = .pred)
merged_resamples <- merged_train %>%
  vfold_cv(v = 10)

spec <- workflow() %>%
  add_recipe(
    recipe(total_nobel_prize ~ birth_continent, data = merged_train)
  ) %>%
  step_dummy(birth_continent) %>%
  add_model(
    linear_reg() %>%
      set_engine(engine = "lm")
  )

model2 <- spec %>%
  fit_resamples(resamples = merged_resamples, metrics = metric_set(mae))

#ncompute cross-validation results
model2 %>% collect_metrics(summarize = TRUE)
merged_resamples <- merged_train %>%
  vfold_cv(v = 10)

spec <- workflow() %>%
  add_recipe(
    recipe(total_nobel_prize ~ birth_continent + birth_country_score , data = merged_train)
  ) %>%
  step_dummy(birth_continente) %>%
  add_model(
    linear_reg() %>%
      set_engine(engine = "lm")
  )

model3 <- spec %>%
  fit_resamples(resamples = merged_resamples, metrics = metric_set(mae))

#ncompute cross-validation results
model3 %>% collect_metrics(summarize = TRUE)
```

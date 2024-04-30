DS 202 Final Project Data Dynamos
================
Members: Logan Becker, Gavin Herum, Jackson Weaver

## View Data Set

``` r
#install.packages("readr")
library(readr)
# Use your file name (copy path)
injury_data <- read_csv("C:/Users/Logmo/Downloads/archive (1).zip")
injury_data
```

    ## # A tibble: 37,667 × 6
    ##     ...1 Date       Team    Acquired     Relinquished  Notes                    
    ##    <dbl> <date>     <chr>   <chr>        <chr>         <chr>                    
    ##  1     0 1951-12-25 Bullets <NA>         Don Barksdale placed on IL             
    ##  2     1 1952-12-26 Knicks  <NA>         Max Zaslofsky placed on IL with torn s…
    ##  3     2 1956-12-29 Knicks  <NA>         Jim Baechtold placed on inactive list  
    ##  4     3 1959-01-16 Lakers  <NA>         Elgin Baylor  player refused to play a…
    ##  5     4 1961-11-26 Lakers  <NA>         Elgin Baylor  player reported for mili…
    ##  6     5 1962-03-24 Lakers  Elgin Baylor <NA>          player given 2-day pass …
    ##  7     6 1962-03-31 Lakers  Elgin Baylor <NA>          player given weekend pas…
    ##  8     7 1962-10-25 Zephyrs <NA>         Al Ferrari    placed on disabled list  
    ##  9     8 1962-11-06 Zephyrs Al Ferrari   <NA>          activated from disabled …
    ## 10     9 1962-11-14 Zephyrs <NA>         Al Ferrari    placed on disabled list …
    ## # ℹ 37,657 more rows

## Filter Data to be all years beyond (inclusive) 2010

``` r
filtered_injurydata <- injury_data[as.numeric(format(injury_data$Date, "%Y")) >= 2010,]
summary(filtered_injurydata$Date)
```

    ##         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
    ## "2010-01-01" "2014-03-03" "2017-11-19" "2017-05-24" "2021-02-21" "2023-04-16"

## Change old team names to their new team name

``` r
all_teams <- (unique(filtered_injurydata$Team))
all_teams
```

    ##  [1] "Knicks"       "Celtics"      "Nets"         "Wizards"      "Blazers"     
    ##  [6] "Clippers"     "Heat"         "76ers"        "Mavericks"    "Pacers"      
    ## [11] "Raptors"      "Cavaliers"    "Jazz"         "Nuggets"      "Timberwolves"
    ## [16] "Grizzlies"    "Magic"        "Spurs"        "Bucks"        "Pistons"     
    ## [21] "Kings"        "Lakers"       "Hawks"        "Rockets"      "Thunder"     
    ## [26] "Warriors"     "Hornets"      "Suns"         "Bulls"        "Bobcats"     
    ## [31] "Pelicans"     "Bullets"

``` r
# We see there is 32 teams here, there should only be 30 nba teams. Bullets and Bobcats are old names, let's change those to be correct
# Hornets <- Bobcats
# Wizards <- Bullets

filtered_injurydata$Team <- 
  ifelse(filtered_injurydata$Team == "Bobcats", "Hornets",
  ifelse(filtered_injurydata$Team == "Bullets", "Wizards",
         filtered_injurydata$Team))
# Check if there is now 30 teams
all_teams <- (unique(filtered_injurydata$Team))
all_teams
```

    ##  [1] "Knicks"       "Celtics"      "Nets"         "Wizards"      "Blazers"     
    ##  [6] "Clippers"     "Heat"         "76ers"        "Mavericks"    "Pacers"      
    ## [11] "Raptors"      "Cavaliers"    "Jazz"         "Nuggets"      "Timberwolves"
    ## [16] "Grizzlies"    "Magic"        "Spurs"        "Bucks"        "Pistons"     
    ## [21] "Kings"        "Lakers"       "Hawks"        "Rockets"      "Thunder"     
    ## [26] "Warriors"     "Hornets"      "Suns"         "Bulls"        "Pelicans"

``` r
# Data is ready now
```

## Let’s look at the highest injured teams

``` r
team_counts <- table(filtered_injurydata$Team)
sorted_team_counts <- sort(team_counts, decreasing = TRUE)
highest5_injuredTeams <- head(sorted_team_counts, 5)
highest5_injuredTeams
```

    ## 
    ##     Spurs   Celtics   Raptors Mavericks      Heat 
    ##      1110      1028      1009       958       901

The most frequently injured team are Spurs (1110), Celtics (1028),
Raptors (1009), Mavericks (958), Heat (901).

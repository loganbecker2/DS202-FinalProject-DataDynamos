DS 202 Final Project Data Dynamos
================
Members: Logan Becker, Gavin Herum, Jackson Weaver

## View Data Set

``` r
#install.packages("readr")
library(readr)
# Use your file name for read_csv (copy file path)
injury_data <- read_csv("C:/Users/Logmo/Downloads/archive (1).zip")
player_data <- read_csv("C:/Users/Logmo/Downloads/archive.zip")
head(injury_data)
```

    ## # A tibble: 6 × 6
    ##    ...1 Date       Team    Acquired     Relinquished  Notes                     
    ##   <dbl> <date>     <chr>   <chr>        <chr>         <chr>                     
    ## 1     0 1951-12-25 Bullets <NA>         Don Barksdale placed on IL              
    ## 2     1 1952-12-26 Knicks  <NA>         Max Zaslofsky placed on IL with torn si…
    ## 3     2 1956-12-29 Knicks  <NA>         Jim Baechtold placed on inactive list   
    ## 4     3 1959-01-16 Lakers  <NA>         Elgin Baylor  player refused to play af…
    ## 5     4 1961-11-26 Lakers  <NA>         Elgin Baylor  player reported for milit…
    ## 6     5 1962-03-24 Lakers  Elgin Baylor <NA>          player given 2-day pass f…

``` r
head(player_data)
```

    ## # A tibble: 6 × 8
    ##    ...1 Player          height weight collage        born birth_city birth_state
    ##   <dbl> <chr>            <dbl>  <dbl> <chr>         <dbl> <chr>      <chr>      
    ## 1     0 Curly Armstrong    180     77 Indiana Univ…  1918 <NA>       <NA>       
    ## 2     1 Cliff Barker       188     83 University o…  1921 Yorktown   Indiana    
    ## 3     2 Leo Barnhorst      193     86 University o…  1924 <NA>       <NA>       
    ## 4     3 Ed Bartels         196     88 North Caroli…  1925 <NA>       <NA>       
    ## 5     4 Ralph Beard        178     79 University o…  1927 Hardinsbu… Kentucky   
    ## 6     5 Gene Berce         180     79 Marquette Un…  1926 <NA>       <NA>

``` r
# These datsets are already cleaned we just need to filter it to the data we want
```

## Filter Injury Data to be all years beyond 2010 (Inclusive)

``` r
# Filter injury data to be all years beyond 2010 (Inclusive)
filtered_injurydata <- injury_data[as.numeric(format(injury_data$Date, "%Y")) >= 2010,]
summary(filtered_injurydata$Date)
```

    ##         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
    ## "2010-01-01" "2014-03-03" "2017-11-19" "2017-05-24" "2021-02-21" "2023-04-16"

``` r
# Filter injury data to only have rows where Relinquished is not NA (someone is injured), delete Acquired row (Unnecessary)
library(dplyr)
filtered_injurydata <- filtered_injurydata %>%
  filter(!is.na(Relinquished))
filtered_injurydata <- subset(filtered_injurydata, select = -Acquired)
summary(filtered_injurydata)
```

    ##       ...1            Date                Team           Relinquished      
    ##  Min.   :14669   Min.   :2010-01-01   Length:12119       Length:12119      
    ##  1st Qu.:20334   1st Qu.:2014-02-12   Class :character   Class :character  
    ##  Median :26033   Median :2017-11-10   Mode  :character   Mode  :character  
    ##  Mean   :26103   Mean   :2017-05-13                                        
    ##  3rd Qu.:31854   3rd Qu.:2021-02-13                                        
    ##  Max.   :37633   Max.   :2023-04-09                                        
    ##     Notes          
    ##  Length:12119      
    ##  Class :character  
    ##  Mode  :character  
    ##                    
    ##                    
    ## 

``` r
#-------------TESTING-------------
# Starting 12,119 rows
# Filter injury data to include only notes that contain "Placed on IR"
filtered_injurydata <- subset(filtered_injurydata, grepl("placed on IL", Notes))
filtered_injurydata
```

    ## # A tibble: 12,106 × 5
    ##     ...1 Date       Team      Relinquished        Notes                         
    ##    <dbl> <date>     <chr>     <chr>               <chr>                         
    ##  1 14669 2010-01-01 Knicks    Jonathan Bender     placed on IL with sore left l…
    ##  2 14671 2010-01-02 Celtics   Kevin Garnett       placed on IL with hyperextend…
    ##  3 14673 2010-01-02 Nets      Sean Williams       placed on IL                  
    ##  4 14675 2010-01-02 Wizards   Mike James (Lamont) placed on IL                  
    ##  5 14677 2010-01-04 Blazers   Joel Przybilla      placed on IL recovering from …
    ##  6 14678 2010-01-04 Clippers  Brian Skinner       placed on IL                  
    ##  7 14680 2010-01-04 Heat      Jermaine O'Neal     placed on IL with strained gr…
    ##  8 14683 2010-01-05 76ers     Royal Ivey          placed on IL                  
    ##  9 14685 2010-01-05 Knicks    Eddy Curry          placed on IL                  
    ## 10 14687 2010-01-05 Mavericks Erick Dampier       placed on IL with left knee e…
    ## # ℹ 12,096 more rows

``` r
# Ending 12,106 rows


# See what notes include that arent placed on IL
# Do we include DTD injuries??
filtered_injurydata <- subset(filtered_injurydata, !grepl(("placed on IL|IR|IL"), Notes))
filtered_injurydata
```

    ## # A tibble: 0 × 5
    ## # ℹ 5 variables: ...1 <dbl>, Date <date>, Team <chr>, Relinquished <chr>,
    ## #   Notes <chr>

``` r
#--------------------------------
```

## Filter data further to only have rows of injuries (don’t include the rows that show acquired player)

## Change old team names to their new team name

``` r
all_teams <- (unique(filtered_injurydata$Team))
all_teams
```

    ## character(0)

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

    ## logical(0)

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

    ## integer(0)

The most frequently injured team are Spurs (582), Celtics (530), Raptors
(529), Mavericks (497), Heat (474).

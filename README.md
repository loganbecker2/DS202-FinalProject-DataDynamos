DS 202 Final Project Data Dynamos
================
Members: Logan Becker, Gavin Herum, Jackson Weaver

## View Data Set

``` r
#install.packages("readr")
library(readr)
# Use your file name for read_csv (copy file path)
injury_data <- read_csv("./injury_data.zip")

player_data <- read_csv("./player_data.csv")
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
    ##   name             year_start year_end position height weight birth_date college
    ##   <chr>                 <dbl>    <dbl> <chr>    <chr>   <dbl> <chr>      <chr>  
    ## 1 Alaa Abdelnaby         1991     1995 F-C      6-10      240 June 24, … Duke U…
    ## 2 Zaid Abdul-Aziz        1969     1978 C-F      6-9       235 April 7, … Iowa S…
    ## 3 Kareem Abdul-Ja…       1970     1989 C        7-2       225 April 16,… Univer…
    ## 4 Mahmoud Abdul-R…       1991     2001 G        6-1       162 March 9, … Louisi…
    ## 5 Tariq Abdul-Wah…       1998     2003 F        6-6       223 November … San Jo…
    ## 6 Shareef Abdur-R…       1997     2008 F        6-9       225 December … Univer…

``` r
# These datsets are already cleaned we just need to filter it to the data we want
```

# \## Filter Injury Data to be all years beyond 2010 (Inclusive)

# Use your file name (copy path)

injury_data \<- read_csv(“./archive.zip”) injury_data


    ## Filter Data to be all years beyond (inclusive) 2010

    >>>>>>> 560a834c5ef2231fff48ed95a8539967ea698ad9

    ```r
    # Filter injury data to be all years beyond 2010 (Inclusive)
    filtered_injurydata <- injury_data[as.numeric(format(injury_data$Date, "%Y")) >= 2010,]
    summary(filtered_injurydata$Date)

    ##         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
    ## "2010-01-01" "2014-03-03" "2017-11-19" "2017-05-24" "2021-02-21" "2023-04-16"

``` r
# Filter injury data to only have rows where Relinquished is not NA (someone is injured)
library(dplyr)
filtered_injurydata <- filtered_injurydata %>%
  filter(!is.na(Relinquished))
# Deletes acquired row
#filtered_injurydata <- subset(filtered_injurydata, select = -Acquired)
summary(filtered_injurydata)
```

    ##       ...1            Date                Team             Acquired        
    ##  Min.   :14669   Min.   :2010-01-01   Length:12119       Length:12119      
    ##  1st Qu.:20334   1st Qu.:2014-02-12   Class :character   Class :character  
    ##  Median :26033   Median :2017-11-10   Mode  :character   Mode  :character  
    ##  Mean   :26103   Mean   :2017-05-13                                        
    ##  3rd Qu.:31854   3rd Qu.:2021-02-13                                        
    ##  Max.   :37633   Max.   :2023-04-09                                        
    ##  Relinquished          Notes          
    ##  Length:12119       Length:12119      
    ##  Class :character   Class :character  
    ##  Mode  :character   Mode  :character  
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

    ## # A tibble: 12,106 × 6
    ##     ...1 Date       Team      Acquired Relinquished        Notes                
    ##    <dbl> <date>     <chr>     <chr>    <chr>               <chr>                
    ##  1 14669 2010-01-01 Knicks    <NA>     Jonathan Bender     placed on IL with so…
    ##  2 14671 2010-01-02 Celtics   <NA>     Kevin Garnett       placed on IL with hy…
    ##  3 14673 2010-01-02 Nets      <NA>     Sean Williams       placed on IL         
    ##  4 14675 2010-01-02 Wizards   <NA>     Mike James (Lamont) placed on IL         
    ##  5 14677 2010-01-04 Blazers   <NA>     Joel Przybilla      placed on IL recover…
    ##  6 14678 2010-01-04 Clippers  <NA>     Brian Skinner       placed on IL         
    ##  7 14680 2010-01-04 Heat      <NA>     Jermaine O'Neal     placed on IL with st…
    ##  8 14683 2010-01-05 76ers     <NA>     Royal Ivey          placed on IL         
    ##  9 14685 2010-01-05 Knicks    <NA>     Eddy Curry          placed on IL         
    ## 10 14687 2010-01-05 Mavericks <NA>     Erick Dampier       placed on IL with le…
    ## # ℹ 12,096 more rows

``` r
# Ending 12,106 rows


# See what notes include that arent placed on IL
# Do we include DTD injuries??
filtered_injurydata <- subset(filtered_injurydata, !grepl(("placed on IL|IR|IL"), Notes))
filtered_injurydata
```

    ## # A tibble: 0 × 6
    ## # ℹ 6 variables: ...1 <dbl>, Date <date>, Team <chr>, Acquired <chr>,
    ## #   Relinquished <chr>, Notes <chr>

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

\<\<\<\<\<\<\< HEAD The most frequently injured team are Spurs (582),
Celtics (530), Raptors (529), Mavericks (497), Heat (474). =======

The most frequently injured team are Spurs (1110), Celtics (1028),
Raptors (1009), Mavericks (958), Heat (901). \>\>\>\>\>\>\>
560a834c5ef2231fff48ed95a8539967ea698ad9

## Lets find out the average duration of an injury

``` r
library(magrittr)
library(dplyr)
library(ggplot2)

# Create subset of the end of an injury
Acquired_subset <- subset(filtered_injurydata, is.na(filtered_injurydata$Relinquished))
Acquired_subset <- subset( Acquired_subset, select = -c(1, Relinquished) )

# Create subset of the begining of an injury
Relinquished_subset <- subset(filtered_injurydata, is.na(filtered_injurydata$Acquired))
Relinquished_subset <- subset( Relinquished_subset, select = -c(1, Acquired) )


# Merge the dataset with itself based on the player's name
merged_data <- merge(x = Acquired_subset, y = Relinquished_subset, by.x = "Acquired", by.y = "Relinquished", suffixes = c("_end", "_begin"))

merged_data$Date_gap <- as.numeric(difftime(merged_data$Date_end, merged_data$Date_begin, units = "days"))

# Removing all Date_gaps less than 0
merged_data <- subset(merged_data, Date_gap > 0 )

# Filtering to ensure no duplicate Date_end for a given Date_begin
injury_duration <- merged_data %>% 
  group_by(Acquired, Date_end)  %>% 
  filter(Date_gap == min(Date_gap))
```

    ## Warning: There was 1 warning in `filter()`.
    ## ℹ In argument: `Date_gap == min(Date_gap)`.
    ## Caused by warning in `min()`:
    ## ! no non-missing arguments to min; returning Inf

``` r
# Reordering collumns of data
injury_duration <- injury_duration[, c("Acquired", "Date_begin", "Date_end", "Date_gap", "Team_begin", "Team_end", "Notes_begin", "Notes_end")]

# Rename column to Player
colnames(injury_duration)[colnames(injury_duration) == "Acquired"] <- 'Player'

Injury_duration_average <- mean(injury_duration$Date_gap)
Injury_duration_average
```

    ## [1] NaN

``` r
# This graph is messed up because Patty Mills had an injury that lasted for 2976 days
ggplot(injury_duration, aes(x = Date_gap)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Frequency of Different Injury Durations",
       x = "Duration of injury",
       y = "Frequency") +
  theme_minimal()
```

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
# This graph is a zoomed in version of only the injuries that lasted less than a year
ggplot(subset(injury_duration, Date_gap < 365 ), aes(x = Date_gap)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = 
  "Frequency of Different Injury Durations
  Excluding Injuries that Lasted more than a year (365 days)",
       x = "Duration of injury",
       y = "Frequency") +
  theme_minimal()
```

![](README_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

Based off of this data the average duration of an injury will be 13.85
days.

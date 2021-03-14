---
title: "What makes a song popular?"
subtitle: "A musical analysis of popular songs and their characteristics"
author: "Alex Ismail, Malek Kaloti, Brian Lee"
date: "13 March 2021"
output: 
  html_document:
    keep_md: TRUE
    toc: TRUE
    toc_float: TRUE
    df_print: paged
    code_download: true
    code_folding: hide

      
---




```r
#loading packages
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(tidyverse)
```

```
## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──
```

```
## ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
## ✓ tibble  3.0.5     ✓ stringr 1.4.0
## ✓ tidyr   1.1.2     ✓ forcats 0.5.0
## ✓ readr   1.4.0
```

```
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## x lubridate::as.difftime() masks base::as.difftime()
## x lubridate::date()        masks base::date()
## x dplyr::filter()          masks stats::filter()
## x lubridate::intersect()   masks base::intersect()
## x dplyr::lag()             masks stats::lag()
## x lubridate::setdiff()     masks base::setdiff()
## x lubridate::union()       masks base::union()
```

```r
library(ggridges) # for joy plots
library(plotly) 
```

```
## 
## Attaching package: 'plotly'
```

```
## The following object is masked from 'package:ggplot2':
## 
##     last_plot
```

```
## The following object is masked from 'package:stats':
## 
##     filter
```

```
## The following object is masked from 'package:graphics':
## 
##     layout
```

```r
library(gganimate)     # for adding animation layers to ggplots
library(gifski)        # for creating the gif (don't need to load this library every time,but need it installed)
```


```r
#loading data
spotify <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')
```

```
## 
## ── Column specification ────────────────────────────────────────────────────────
## cols(
##   .default = col_double(),
##   track_id = col_character(),
##   track_name = col_character(),
##   track_artist = col_character(),
##   track_album_id = col_character(),
##   track_album_name = col_character(),
##   track_album_release_date = col_character(),
##   playlist_name = col_character(),
##   playlist_id = col_character(),
##   playlist_genre = col_character(),
##   playlist_subgenre = col_character()
## )
## ℹ Use `spec()` for the full column specifications.
```

```r
spotify_rap <- spotify %>% 
  filter(playlist_genre == "rap")

randb <- spotify %>%
  filter(playlist_genre == "r&b") %>%
  select(-track_id, - track_album_id, -playlist_id, -playlist_name) %>%
  filter(track_popularity >= 75)
```

# Introduction & Background

A noticeable trend in the entertainment industry has been the transformation from physical to digitalized content. Newspapers are replaced with online articles, Netflix replaced Blockbuster, and online music streaming sites like Spotify have taken over traditonal music media as the primary source of music for millions. In addition to expanding the reach of music to new people, the transition to using Spotify has come with new ways to analyze music tastes, both individually and as a population. Spotify users have access to new daily playlists tailored to their individual preferences, in addition to having suggested music based of the data provided to Spotify of listening habits. In our analysis of the characteristics of songs, we hope to make obserations to help pinpoint what exactly makes a song popular.

## Codebook of variables
These variables and definitions were retrieved directly from the dataset's repository.

name                      definition
---------                 ------------------------
track_id                  Song unique ID
track_name                Song Name
track_artist              Song Artist
track_popularity          Song Popularity (0-100) where higher is better
track_album_id	          Album unique ID
track_album_name	        Song album name
track_album_release_date	Date when album released
playlist_name	            Name of playlist
playlist_id               Playlist ID
playlist_genre	          Playlist genre
playlist_subgenre	        Playlist subgenre
danceability	            Danceability describes how suitable a track is for dancing                            based on a combination of musical elements including tempo,                           rhythm stability, beat strength, and overall regularity. A                            value of 0.0 is least danceable and 1.0 is most danceable.
energy	                  Energy is a measure from 0.0 to 1.0 and represents a
                          perceptual measure of intensity and activity. Typically,                              energetic tracks feel fast, loud, and noisy. For example,                             death metal has high energy, while a Bach prelude scores low                           on the scale. Perceptual features contributing to this
                          attribute include dynamic range, perceived loudness, timbre,
                          onset rate, and general entropy.

## Data Collection
Data was retrieved from github:
https://github.com/rfordatascience/tidytuesday/blob/faca0b6bd282998693007c329e3f4b917a5fd7a8/data/2020/2020-01-21/readme.md

Retrieved from a past Tidy Tuesday dataset, we will be using data from Spotify for our analysis. The link to the repository can be can be accessed [here](https://github.com/rfordatascience/tidytuesday/blob/faca0b6bd282998693007c329e3f4b917a5fd7a8/data/2020/2020-01-21/readme.md)

Using data provided by Spotify, we will investigate how certain aspects of a song, such as genre, subgenre, release date, and other more complex measurements correlate to how popular a song is according to Spotify. Popularity is rated on a scale to 100 with the most popular songs on Spotify as of February 2020 will have a score near 100. We identified popularity scores greater than 75 to be a convenient threshold to deem a song "popular".

implications of using the data, any biases in the data?

## How has the popularity of genres changed over time?


```r
genre_pop <- spotify %>%
  filter(track_popularity >= 75) %>%
    mutate(ymd_release = ymd(track_album_release_date),
         year = year(ymd_release)) %>%
  group_by(year, playlist_genre) %>%
  summarize(avg_popularity = mean(track_popularity)) %>%
  ggplot(aes(x = year, y = avg_popularity, color = playlist_genre)) +
  geom_point() +
  labs(title="Average song popularity by genre per year",
       subtitle = "Overall, as music becomes more accessible, average peopulatity across all genres is on the rise.",
       x = "",
       y = "",
       color = "Genre") +
  theme_classic()
```

```
## Warning: Problem with `mutate()` input `ymd_release`.
## ℹ  68 failed to parse.
## ℹ Input `ymd_release` is `ymd(track_album_release_date)`.
```

```
## Warning: 68 failed to parse.
```

```
## `summarise()` regrouping output by 'year' (override with `.groups` argument)
```

```r
ggplotly(genre_pop)
```

<!--html_preserve--><div id="htmlwidget-5bb0d5e349a04d68609e" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-5bb0d5e349a04d68609e">{"x":{"data":[{"x":[2002,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,null],"y":[83,77,79,76,78,77.5,77,77.6428571428571,78.1428571428571,79.5384615384615,78.3636363636364,79.695652173913,78.8666666666667,81.7826086956522,84.2222222222222,86.1111111111111,77.5],"text":["year: 2002<br />avg_popularity: 83.00000<br />playlist_genre: edm","year: 2006<br />avg_popularity: 77.00000<br />playlist_genre: edm","year: 2007<br />avg_popularity: 79.00000<br />playlist_genre: edm","year: 2008<br />avg_popularity: 76.00000<br />playlist_genre: edm","year: 2009<br />avg_popularity: 78.00000<br />playlist_genre: edm","year: 2010<br />avg_popularity: 77.50000<br />playlist_genre: edm","year: 2011<br />avg_popularity: 77.00000<br />playlist_genre: edm","year: 2012<br />avg_popularity: 77.64286<br />playlist_genre: edm","year: 2013<br />avg_popularity: 78.14286<br />playlist_genre: edm","year: 2014<br />avg_popularity: 79.53846<br />playlist_genre: edm","year: 2015<br />avg_popularity: 78.36364<br />playlist_genre: edm","year: 2016<br />avg_popularity: 79.69565<br />playlist_genre: edm","year: 2017<br />avg_popularity: 78.86667<br />playlist_genre: edm","year: 2018<br />avg_popularity: 81.78261<br />playlist_genre: edm","year: 2019<br />avg_popularity: 84.22222<br />playlist_genre: edm","year: 2020<br />avg_popularity: 86.11111<br />playlist_genre: edm","year:   NA<br />avg_popularity: 77.50000<br />playlist_genre: edm"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(248,118,109,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(248,118,109,1)"}},"hoveron":"points","name":"edm","legendgroup":"edm","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1987,1988,1995,1997,1999,2003,2004,2005,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,null],"y":[77,75,80,75,76,75,77.5,77.5,77.6666666666667,77,77.0769230769231,77.3333333333333,77.75,79.75,78.4285714285714,77.5,80,78.2692307692308,80.6826923076923,83.488120950324,84.2916666666667,78],"text":["year: 1987<br />avg_popularity: 77.00000<br />playlist_genre: latin","year: 1988<br />avg_popularity: 75.00000<br />playlist_genre: latin","year: 1995<br />avg_popularity: 80.00000<br />playlist_genre: latin","year: 1997<br />avg_popularity: 75.00000<br />playlist_genre: latin","year: 1999<br />avg_popularity: 76.00000<br />playlist_genre: latin","year: 2003<br />avg_popularity: 75.00000<br />playlist_genre: latin","year: 2004<br />avg_popularity: 77.50000<br />playlist_genre: latin","year: 2005<br />avg_popularity: 77.50000<br />playlist_genre: latin","year: 2008<br />avg_popularity: 77.66667<br />playlist_genre: latin","year: 2009<br />avg_popularity: 77.00000<br />playlist_genre: latin","year: 2010<br />avg_popularity: 77.07692<br />playlist_genre: latin","year: 2011<br />avg_popularity: 77.33333<br />playlist_genre: latin","year: 2012<br />avg_popularity: 77.75000<br />playlist_genre: latin","year: 2013<br />avg_popularity: 79.75000<br />playlist_genre: latin","year: 2014<br />avg_popularity: 78.42857<br />playlist_genre: latin","year: 2015<br />avg_popularity: 77.50000<br />playlist_genre: latin","year: 2016<br />avg_popularity: 80.00000<br />playlist_genre: latin","year: 2017<br />avg_popularity: 78.26923<br />playlist_genre: latin","year: 2018<br />avg_popularity: 80.68269<br />playlist_genre: latin","year: 2019<br />avg_popularity: 83.48812<br />playlist_genre: latin","year: 2020<br />avg_popularity: 84.29167<br />playlist_genre: latin","year:   NA<br />avg_popularity: 78.00000<br />playlist_genre: latin"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(183,159,0,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(183,159,0,1)"}},"hoveron":"points","name":"latin","legendgroup":"latin","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1969,1978,1980,1982,1983,1984,1985,1987,1988,1989,1990,1991,1994,1995,1996,1999,2000,2001,2002,2003,2004,2005,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,null],"y":[80,75,80.5,77,79.3333333333333,77,83,78.5,76,75,81,81,79,80,79,76.3333333333333,78.5,75.5,77.75,78,77.6666666666667,79.25,76.6,78,76.8333333333333,77.9,78.6190476190476,77.0322580645161,78.4333333333333,78.5277777777778,78.0816326530612,79.2571428571429,79.7875,80.7238805970149,84.2132352941177,86.4545454545455,77],"text":["year: 1969<br />avg_popularity: 80.00000<br />playlist_genre: pop","year: 1978<br />avg_popularity: 75.00000<br />playlist_genre: pop","year: 1980<br />avg_popularity: 80.50000<br />playlist_genre: pop","year: 1982<br />avg_popularity: 77.00000<br />playlist_genre: pop","year: 1983<br />avg_popularity: 79.33333<br />playlist_genre: pop","year: 1984<br />avg_popularity: 77.00000<br />playlist_genre: pop","year: 1985<br />avg_popularity: 83.00000<br />playlist_genre: pop","year: 1987<br />avg_popularity: 78.50000<br />playlist_genre: pop","year: 1988<br />avg_popularity: 76.00000<br />playlist_genre: pop","year: 1989<br />avg_popularity: 75.00000<br />playlist_genre: pop","year: 1990<br />avg_popularity: 81.00000<br />playlist_genre: pop","year: 1991<br />avg_popularity: 81.00000<br />playlist_genre: pop","year: 1994<br />avg_popularity: 79.00000<br />playlist_genre: pop","year: 1995<br />avg_popularity: 80.00000<br />playlist_genre: pop","year: 1996<br />avg_popularity: 79.00000<br />playlist_genre: pop","year: 1999<br />avg_popularity: 76.33333<br />playlist_genre: pop","year: 2000<br />avg_popularity: 78.50000<br />playlist_genre: pop","year: 2001<br />avg_popularity: 75.50000<br />playlist_genre: pop","year: 2002<br />avg_popularity: 77.75000<br />playlist_genre: pop","year: 2003<br />avg_popularity: 78.00000<br />playlist_genre: pop","year: 2004<br />avg_popularity: 77.66667<br />playlist_genre: pop","year: 2005<br />avg_popularity: 79.25000<br />playlist_genre: pop","year: 2007<br />avg_popularity: 76.60000<br />playlist_genre: pop","year: 2008<br />avg_popularity: 78.00000<br />playlist_genre: pop","year: 2009<br />avg_popularity: 76.83333<br />playlist_genre: pop","year: 2010<br />avg_popularity: 77.90000<br />playlist_genre: pop","year: 2011<br />avg_popularity: 78.61905<br />playlist_genre: pop","year: 2012<br />avg_popularity: 77.03226<br />playlist_genre: pop","year: 2013<br />avg_popularity: 78.43333<br />playlist_genre: pop","year: 2014<br />avg_popularity: 78.52778<br />playlist_genre: pop","year: 2015<br />avg_popularity: 78.08163<br />playlist_genre: pop","year: 2016<br />avg_popularity: 79.25714<br />playlist_genre: pop","year: 2017<br />avg_popularity: 79.78750<br />playlist_genre: pop","year: 2018<br />avg_popularity: 80.72388<br />playlist_genre: pop","year: 2019<br />avg_popularity: 84.21324<br />playlist_genre: pop","year: 2020<br />avg_popularity: 86.45455<br />playlist_genre: pop","year:   NA<br />avg_popularity: 77.00000<br />playlist_genre: pop"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(0,186,56,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(0,186,56,1)"}},"hoveron":"points","name":"pop","legendgroup":"pop","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1971,1986,1987,1990,1994,1996,1999,2000,2002,2003,2004,2006,2008,2010,2012,2013,2014,2015,2016,2017,2018,2019,2020,null],"y":[76,79,80,75,90,76,77.5,77,77,78,77,76,77,77.5,75,79.6666666666667,77.6,77.9166666666667,79.1666666666667,79.1935483870968,81.8369565217391,84.1321428571429,84,77.3333333333333],"text":["year: 1971<br />avg_popularity: 76.00000<br />playlist_genre: r&b","year: 1986<br />avg_popularity: 79.00000<br />playlist_genre: r&b","year: 1987<br />avg_popularity: 80.00000<br />playlist_genre: r&b","year: 1990<br />avg_popularity: 75.00000<br />playlist_genre: r&b","year: 1994<br />avg_popularity: 90.00000<br />playlist_genre: r&b","year: 1996<br />avg_popularity: 76.00000<br />playlist_genre: r&b","year: 1999<br />avg_popularity: 77.50000<br />playlist_genre: r&b","year: 2000<br />avg_popularity: 77.00000<br />playlist_genre: r&b","year: 2002<br />avg_popularity: 77.00000<br />playlist_genre: r&b","year: 2003<br />avg_popularity: 78.00000<br />playlist_genre: r&b","year: 2004<br />avg_popularity: 77.00000<br />playlist_genre: r&b","year: 2006<br />avg_popularity: 76.00000<br />playlist_genre: r&b","year: 2008<br />avg_popularity: 77.00000<br />playlist_genre: r&b","year: 2010<br />avg_popularity: 77.50000<br />playlist_genre: r&b","year: 2012<br />avg_popularity: 75.00000<br />playlist_genre: r&b","year: 2013<br />avg_popularity: 79.66667<br />playlist_genre: r&b","year: 2014<br />avg_popularity: 77.60000<br />playlist_genre: r&b","year: 2015<br />avg_popularity: 77.91667<br />playlist_genre: r&b","year: 2016<br />avg_popularity: 79.16667<br />playlist_genre: r&b","year: 2017<br />avg_popularity: 79.19355<br />playlist_genre: r&b","year: 2018<br />avg_popularity: 81.83696<br />playlist_genre: r&b","year: 2019<br />avg_popularity: 84.13214<br />playlist_genre: r&b","year: 2020<br />avg_popularity: 84.00000<br />playlist_genre: r&b","year:   NA<br />avg_popularity: 77.33333<br />playlist_genre: r&b"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(0,191,196,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(0,191,196,1)"}},"hoveron":"points","name":"r&b","legendgroup":"r&b","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1974,1982,1994,1995,1996,1997,1999,2000,2002,2003,2004,2005,2007,2008,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,null],"y":[81,77,76,80,76.2,75,75,78.5,80,77,79,76,76,79,77,76,75,82,76.6363636363636,79.6666666666667,79.4761904761905,80.2333333333333,82.0657276995305,81.3333333333333,79],"text":["year: 1974<br />avg_popularity: 81.00000<br />playlist_genre: rap","year: 1982<br />avg_popularity: 77.00000<br />playlist_genre: rap","year: 1994<br />avg_popularity: 76.00000<br />playlist_genre: rap","year: 1995<br />avg_popularity: 80.00000<br />playlist_genre: rap","year: 1996<br />avg_popularity: 76.20000<br />playlist_genre: rap","year: 1997<br />avg_popularity: 75.00000<br />playlist_genre: rap","year: 1999<br />avg_popularity: 75.00000<br />playlist_genre: rap","year: 2000<br />avg_popularity: 78.50000<br />playlist_genre: rap","year: 2002<br />avg_popularity: 80.00000<br />playlist_genre: rap","year: 2003<br />avg_popularity: 77.00000<br />playlist_genre: rap","year: 2004<br />avg_popularity: 79.00000<br />playlist_genre: rap","year: 2005<br />avg_popularity: 76.00000<br />playlist_genre: rap","year: 2007<br />avg_popularity: 76.00000<br />playlist_genre: rap","year: 2008<br />avg_popularity: 79.00000<br />playlist_genre: rap","year: 2011<br />avg_popularity: 77.00000<br />playlist_genre: rap","year: 2012<br />avg_popularity: 76.00000<br />playlist_genre: rap","year: 2013<br />avg_popularity: 75.00000<br />playlist_genre: rap","year: 2014<br />avg_popularity: 82.00000<br />playlist_genre: rap","year: 2015<br />avg_popularity: 76.63636<br />playlist_genre: rap","year: 2016<br />avg_popularity: 79.66667<br />playlist_genre: rap","year: 2017<br />avg_popularity: 79.47619<br />playlist_genre: rap","year: 2018<br />avg_popularity: 80.23333<br />playlist_genre: rap","year: 2019<br />avg_popularity: 82.06573<br />playlist_genre: rap","year: 2020<br />avg_popularity: 81.33333<br />playlist_genre: rap","year:   NA<br />avg_popularity: 79.00000<br />playlist_genre: rap"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(97,156,255,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(97,156,255,1)"}},"hoveron":"points","name":"rap","legendgroup":"rap","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1965,1968,1969,1970,1971,1972,1973,1974,1975,1976,1977,1978,1979,1980,1981,1982,1983,1984,1985,1986,1987,1989,1990,1991,1992,1993,1994,1995,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2011,2012,2013,2014,2015,2016,2017,2018,2019,null],"y":[76,76,77.2857142857143,77.3333333333333,77.1666666666667,76.5,76.8,81,76.875,80.3333333333333,77.6666666666667,77.5,79.2857142857143,80.3333333333333,76,81,79.6,76.5,80.2,79,79.2307692307692,75.5,78.5,77.8571428571429,75,77.3333333333333,78.6,79,77.25,75.6,78.8333333333333,82.3333333333333,78,76.25,79.5,77.1666666666667,77.4,76.875,76,76.8333333333333,75,79.3333333333333,75,77.6666666666667,81,84.3333333333333,79,78.6,81.3333333333333,82.3333333333333,76.9268292682927],"text":["year: 1965<br />avg_popularity: 76.00000<br />playlist_genre: rock","year: 1968<br />avg_popularity: 76.00000<br />playlist_genre: rock","year: 1969<br />avg_popularity: 77.28571<br />playlist_genre: rock","year: 1970<br />avg_popularity: 77.33333<br />playlist_genre: rock","year: 1971<br />avg_popularity: 77.16667<br />playlist_genre: rock","year: 1972<br />avg_popularity: 76.50000<br />playlist_genre: rock","year: 1973<br />avg_popularity: 76.80000<br />playlist_genre: rock","year: 1974<br />avg_popularity: 81.00000<br />playlist_genre: rock","year: 1975<br />avg_popularity: 76.87500<br />playlist_genre: rock","year: 1976<br />avg_popularity: 80.33333<br />playlist_genre: rock","year: 1977<br />avg_popularity: 77.66667<br />playlist_genre: rock","year: 1978<br />avg_popularity: 77.50000<br />playlist_genre: rock","year: 1979<br />avg_popularity: 79.28571<br />playlist_genre: rock","year: 1980<br />avg_popularity: 80.33333<br />playlist_genre: rock","year: 1981<br />avg_popularity: 76.00000<br />playlist_genre: rock","year: 1982<br />avg_popularity: 81.00000<br />playlist_genre: rock","year: 1983<br />avg_popularity: 79.60000<br />playlist_genre: rock","year: 1984<br />avg_popularity: 76.50000<br />playlist_genre: rock","year: 1985<br />avg_popularity: 80.20000<br />playlist_genre: rock","year: 1986<br />avg_popularity: 79.00000<br />playlist_genre: rock","year: 1987<br />avg_popularity: 79.23077<br />playlist_genre: rock","year: 1989<br />avg_popularity: 75.50000<br />playlist_genre: rock","year: 1990<br />avg_popularity: 78.50000<br />playlist_genre: rock","year: 1991<br />avg_popularity: 77.85714<br />playlist_genre: rock","year: 1992<br />avg_popularity: 75.00000<br />playlist_genre: rock","year: 1993<br />avg_popularity: 77.33333<br />playlist_genre: rock","year: 1994<br />avg_popularity: 78.60000<br />playlist_genre: rock","year: 1995<br />avg_popularity: 79.00000<br />playlist_genre: rock","year: 1997<br />avg_popularity: 77.25000<br />playlist_genre: rock","year: 1998<br />avg_popularity: 75.60000<br />playlist_genre: rock","year: 1999<br />avg_popularity: 78.83333<br />playlist_genre: rock","year: 2000<br />avg_popularity: 82.33333<br />playlist_genre: rock","year: 2001<br />avg_popularity: 78.00000<br />playlist_genre: rock","year: 2002<br />avg_popularity: 76.25000<br />playlist_genre: rock","year: 2003<br />avg_popularity: 79.50000<br />playlist_genre: rock","year: 2004<br />avg_popularity: 77.16667<br />playlist_genre: rock","year: 2005<br />avg_popularity: 77.40000<br />playlist_genre: rock","year: 2006<br />avg_popularity: 76.87500<br />playlist_genre: rock","year: 2007<br />avg_popularity: 76.00000<br />playlist_genre: rock","year: 2008<br />avg_popularity: 76.83333<br />playlist_genre: rock","year: 2009<br />avg_popularity: 75.00000<br />playlist_genre: rock","year: 2011<br />avg_popularity: 79.33333<br />playlist_genre: rock","year: 2012<br />avg_popularity: 75.00000<br />playlist_genre: rock","year: 2013<br />avg_popularity: 77.66667<br />playlist_genre: rock","year: 2014<br />avg_popularity: 81.00000<br />playlist_genre: rock","year: 2015<br />avg_popularity: 84.33333<br />playlist_genre: rock","year: 2016<br />avg_popularity: 79.00000<br />playlist_genre: rock","year: 2017<br />avg_popularity: 78.60000<br />playlist_genre: rock","year: 2018<br />avg_popularity: 81.33333<br />playlist_genre: rock","year: 2019<br />avg_popularity: 82.33333<br />playlist_genre: rock","year:   NA<br />avg_popularity: 76.92683<br />playlist_genre: rock"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(245,100,227,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(245,100,227,1)"}},"hoveron":"points","name":"rock","legendgroup":"rock","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":43.7625570776256,"r":7.30593607305936,"b":25.5707762557078,"l":22.648401826484},"plot_bgcolor":"rgba(255,255,255,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"title":{"text":"Average song popularity by genre per year","font":{"color":"rgba(0,0,0,1)","family":"","size":17.5342465753425},"x":0,"xref":"paper"},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[1962.25,2022.75],"tickmode":"array","ticktext":["1980","2000","2020"],"tickvals":[1980,2000,2020],"categoryorder":"array","categoryarray":["1980","2000","2020"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":true,"linecolor":"rgba(0,0,0,1)","linewidth":0.66417600664176,"showgrid":false,"gridcolor":null,"gridwidth":0,"zeroline":false,"anchor":"y","title":{"text":"","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[74.25,90.75],"tickmode":"array","ticktext":["75","80","85","90"],"tickvals":[75,80,85,90],"categoryorder":"array","categoryarray":["75","80","85","90"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":true,"linecolor":"rgba(0,0,0,1)","linewidth":0.66417600664176,"showgrid":false,"gridcolor":null,"gridwidth":0,"zeroline":false,"anchor":"x","title":{"text":"","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":true,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895},"y":0.913385826771654},"annotations":[{"text":"Genre","x":1.02,"y":1,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"left","yanchor":"bottom","legendTitle":true}],"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","showSendToCloud":false},"source":"A","attrs":{"8f4a784dea11":{"x":{},"y":{},"colour":{},"type":"scatter"}},"cur_data":"8f4a784dea11","visdat":{"8f4a784dea11":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

The graph above shows how track popularity has trended by genre since. Though Spotify has a wide variety of songs, based on the plot rock is the only genre that has any significant foundations of popular songs from before the 1980s and 90s. Since then however, song popularity across every genre has experienced exponential growth in popularity, most notably pop and edm.


## How does does song popularity vary based on genre?


```r
prelim_graph <- spotify %>%
  ggplot(aes(y = playlist_genre, x = track_popularity)) +
  labs(title = "Song Popularity by Genre",
       x = "", y = "",
       subtitle = "Song popularity is measured from 0-100, with higher numbers being indiciative of more popularity.\nHighest median popularities belong to pop and latin with an overall median popularity of 40",
       caption = "Alex Ismail, Malek Kaloti, Brian Lee") +
  theme_classic() + 
  theme(plot.title.position = "plot",
        plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(size = 10, face = "italic")) +
  geom_boxplot() +
  geom_vline(aes(xintercept = median(track_popularity, na.rm = TRUE)), color = "blue") 

prelim_graph
```

![](STAT112_finalProject_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

The box plot above compares distribution of song popularity by genre again, however this time including the songs not deemed popular. EDM music is noticeably behind in median popularity compared to the universal median of ~40, while pop, latin, and to lesser degree rap show a slight advantage over the other genres in median.


# Analysis: Diving deeper into our favorite genres

## Rap

Rap is a particularly fascinating genre to investigate using the Spotify data to look at what traits of music have correlated with popularity as the genre has undergone several changes in audience and style. Though a relatively new genre arriving on the greater music scene in the 80s, rap has undergone a myriad of trends and style variations. Fans of old school rap from the 80s and 90s may have distaste for today's artists like Drake and Eminem for having modernized the genre too much. Fans of modern rap may get bored of the authentic sound of artists like Run-DMC or Tupac. Are there trends that tie all of rap together as to what makes a song popular? 

### Song Quality

The first and most natural observations to make are on overarching metrics that Spotify provides. Using the descriptions provided, I was most interested on the following values in correlation to track popularity: Danceability due to rap's heavy emphasis on rhythm and beats, Energy due to some artists' signature style of shouting to "hype" up a crowd (ie. Lil Jon, DMX), the inverse variables of Speechiness/Instrumentalness due to other artist's signature of rapping as fast as possible (ie. Eminem, Busta Rhymes), and Valence for the perceived association between rap and violence, drugs, and focus on other less-than-righteous topics.


```
## `summarise()` regrouping output by 'Stat1' (override with `.groups` argument)
```

```
## Warning: Problem with `mutate()` input `Stat`.
## ℹ Unknown levels in `f`: Rounded_Energy, Rounded_Speechiness, Rounded_Instrumental, Rounded_Valence
## ℹ Input `Stat` is `fct_recode(...)`.
## ℹ The error occurred in group 1: Stat1 = "Rounded_Danceability".
```

```
## Warning: Unknown levels in `f`: Rounded_Energy, Rounded_Speechiness,
## Rounded_Instrumental, Rounded_Valence
```

```
## Warning: Problem with `mutate()` input `Stat`.
## ℹ Unknown levels in `f`: Rounded_Danceability, Rounded_Speechiness, Rounded_Instrumental, Rounded_Valence
## ℹ Input `Stat` is `fct_recode(...)`.
## ℹ The error occurred in group 2: Stat1 = "Rounded_Energy".
```

```
## Warning: Unknown levels in `f`: Rounded_Danceability, Rounded_Speechiness,
## Rounded_Instrumental, Rounded_Valence
```

```
## Warning: Problem with `mutate()` input `Stat`.
## ℹ Unknown levels in `f`: Rounded_Danceability, Rounded_Energy, Rounded_Speechiness, Rounded_Valence
## ℹ Input `Stat` is `fct_recode(...)`.
## ℹ The error occurred in group 3: Stat1 = "Rounded_Instrumental".
```

```
## Warning: Unknown levels in `f`: Rounded_Danceability, Rounded_Energy,
## Rounded_Speechiness, Rounded_Valence
```

```
## Warning: Problem with `mutate()` input `Stat`.
## ℹ Unknown levels in `f`: Rounded_Danceability, Rounded_Energy, Rounded_Instrumental, Rounded_Valence
## ℹ Input `Stat` is `fct_recode(...)`.
## ℹ The error occurred in group 4: Stat1 = "Rounded_Speechiness".
```

```
## Warning: Unknown levels in `f`: Rounded_Danceability, Rounded_Energy,
## Rounded_Instrumental, Rounded_Valence
```

```
## Warning: Problem with `mutate()` input `Stat`.
## ℹ Unknown levels in `f`: Rounded_Danceability, Rounded_Energy, Rounded_Speechiness, Rounded_Instrumental
## ℹ Input `Stat` is `fct_recode(...)`.
## ℹ The error occurred in group 5: Stat1 = "Rounded_Valence".
```

```
## Warning: Unknown levels in `f`: Rounded_Danceability, Rounded_Energy,
## Rounded_Speechiness, Rounded_Instrumental
```

![](STAT112_finalProject_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

Oddly, the biggest conclusion I drew from this graph was not any positive or negative correlation, but a lack of connection between valence and popularity. For a genre that has a reputation for being connected with gangs, guns, drugs, etc., there is a complete lack of correlation between valence and popularity. Beyond that, there is a moderately strong correlation between popularity and danceability, as I had expected based on the prevalence of beats and rhythms in rap. The energy line shows that the highest percentage of songs to become popular are ~.5 energy, which likely suggests too much energy can take away from the popularity of a song. Finally, the speechiness/instrumentalness variable shows that songs on the extreme end of speechiness (.8+) are most likely to be popular.

### Do songs with multiple artists become popular at a higher rate?

Beyond the stats, there was one more observation I wanted to make on rap music. Based on my experience listening to rap, some of my favorite songs are remixes, features, or any other way multiple artists can put verses on the same song. Songs like "Life is Good" by Drake and Future, or remixes to songs like "HIGHEST IN THE ROOM" which incorporates Lil Baby in a song by Travis Scott add a certain level of freshness and break up three consecutive minutes of one artist rapping into fun back and forths with styles. Below is a graphic comparing the popularity rates of those songs vs solo songs between rap and other genres.


```r
Multiple_Artist_Graph <- spotify %>% 
  mutate(track_name_lower = str_to_lower(track_name),
         remix = str_detect(track_name_lower, "Remix"),
         feature = str_detect(track_name_lower, "feat"),
         ma_prep = remix|feature,
         ma_prep2 = replace_na(ma_prep, FALSE),
         multiple_artists = if_else(ma_prep2, true = "Multiple Artists", false = "One Artist"),
         popular = track_popularity > 75) %>% 
  group_by(multiple_artists, playlist_genre) %>% 
  summarize(prop_pop = mean(popular)*100) %>% 
  mutate(genre = fct_relevel(playlist_genre, "rap")) %>% 
  ggplot() +
  geom_col(aes(x = multiple_artists, y = prop_pop), fill = "black") +
  facet_wrap(~genre) +
  labs(title = "Popularity of Songs Containing Mulitple Artists Across Genre",
       x = "", y = "Percent of Songs Popular") +
  theme_classic() + 
  theme(plot.title.position = "plot",
        plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(size = 10, face = "italic"))
```

```
## `summarise()` regrouping output by 'multiple_artists' (override with `.groups` argument)
```

```r
ggplotly(Multiple_Artist_Graph)
```

```
## Warning: `group_by_()` is deprecated as of dplyr 0.7.0.
## Please use `group_by()` instead.
## See vignette('programming') for more help
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_warnings()` to see where this warning was generated.
```

<!--html_preserve--><div id="htmlwidget-c51ca2ce376696fc75d5" style="width:864px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-c51ca2ce376696fc75d5">{"x":{"data":[{"orientation":"v","width":[0.9,0.9],"base":[0,0],"x":[1,2],"y":[14.84375,4.5436741088915],"text":["multiple_artists: Multiple Artists<br />prop_pop: 14.843750","multiple_artists: One Artist<br />prop_pop:  4.543674"],"type":"bar","marker":{"autocolorscale":false,"color":"rgba(0,0,0,1)","line":{"width":1.88976377952756,"color":"transparent"}},"showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":[0.9,0.9],"base":[0,0],"x":[1,2],"y":[6.27943485086342,3.95856455789863],"text":["multiple_artists: Multiple Artists<br />prop_pop:  6.279435","multiple_artists: One Artist<br />prop_pop:  3.958565"],"type":"bar","marker":{"autocolorscale":false,"color":"rgba(0,0,0,1)","line":{"width":1.88976377952756,"color":"transparent"}},"showlegend":false,"xaxis":"x2","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":[0.9,0.9],"base":[0,0],"x":[1,2],"y":[17.2995780590717,11.5787224951933],"text":["multiple_artists: Multiple Artists<br />prop_pop: 17.299578","multiple_artists: One Artist<br />prop_pop: 11.578722"],"type":"bar","marker":{"autocolorscale":false,"color":"rgba(0,0,0,1)","line":{"width":1.88976377952756,"color":"transparent"}},"showlegend":false,"xaxis":"x3","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":[0.9,0.9],"base":[0,0],"x":[1,2],"y":[22.1095334685598,12.764260071799],"text":["multiple_artists: Multiple Artists<br />prop_pop: 22.109533","multiple_artists: One Artist<br />prop_pop: 12.764260"],"type":"bar","marker":{"autocolorscale":false,"color":"rgba(0,0,0,1)","line":{"width":1.88976377952756,"color":"transparent"}},"showlegend":false,"xaxis":"x","yaxis":"y2","hoverinfo":"text","frame":null},{"orientation":"v","width":[0.9,0.9],"base":[0,0],"x":[1,2],"y":[16.2454873646209,8.11974574533525],"text":["multiple_artists: Multiple Artists<br />prop_pop: 16.245487","multiple_artists: One Artist<br />prop_pop:  8.119746"],"type":"bar","marker":{"autocolorscale":false,"color":"rgba(0,0,0,1)","line":{"width":1.88976377952756,"color":"transparent"}},"showlegend":false,"xaxis":"x2","yaxis":"y2","hoverinfo":"text","frame":null},{"orientation":"v","width":[0.9,0.9],"base":[0,0],"x":[1,2],"y":[2.63157894736842,4.47791573376756],"text":["multiple_artists: Multiple Artists<br />prop_pop:  2.631579","multiple_artists: One Artist<br />prop_pop:  4.477916"],"type":"bar","marker":{"autocolorscale":false,"color":"rgba(0,0,0,1)","line":{"width":1.88976377952756,"color":"transparent"}},"showlegend":false,"xaxis":"x3","yaxis":"y2","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":67.4072229140722,"r":7.30593607305936,"b":28.4931506849315,"l":37.2602739726027},"plot_bgcolor":"rgba(255,255,255,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"title":{"text":"<b> Popularity of Songs Containing Mulitple Artists Across Genre <\/b>","font":{"color":"rgba(0,0,0,1)","family":"","size":26.5670402656704},"x":0,"xref":"paper"},"xaxis":{"domain":[0,0.324877388804329],"automargin":true,"type":"linear","autorange":false,"range":[0.4,2.6],"tickmode":"array","ticktext":["Multiple Artists","One Artist"],"tickvals":[1,2],"categoryorder":"array","categoryarray":["Multiple Artists","One Artist"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":true,"linecolor":"rgba(0,0,0,1)","linewidth":0.66417600664176,"showgrid":false,"gridcolor":null,"gridwidth":0,"zeroline":false,"anchor":"y2","title":"","hoverformat":".2f"},"yaxis":{"domain":[0.539573820395738,1],"automargin":true,"type":"linear","autorange":false,"range":[-1.10547667342799,23.2150101419878],"tickmode":"array","ticktext":["0","5","10","15","20"],"tickvals":[0,5,10,15,20],"categoryorder":"array","categoryarray":["0","5","10","15","20"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":true,"linecolor":"rgba(0,0,0,1)","linewidth":0.66417600664176,"showgrid":false,"gridcolor":null,"gridwidth":0,"zeroline":false,"anchor":"x","title":"","hoverformat":".2f"},"annotations":[{"text":"Percent of Songs Popular","x":-0.0262134280399121,"y":0.5,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xref":"paper","yref":"paper","textangle":-90,"xanchor":"right","yanchor":"center","annotationType":"axis"},{"text":"rap","x":0.162438694402165,"y":1,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(26,26,26,1)","family":"","size":11.689497716895},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"center","yanchor":"bottom"},{"text":"edm","x":0.5,"y":1,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(26,26,26,1)","family":"","size":11.689497716895},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"center","yanchor":"bottom"},{"text":"latin","x":0.837561305597835,"y":1,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(26,26,26,1)","family":"","size":11.689497716895},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"center","yanchor":"bottom"},{"text":"pop","x":0.162438694402165,"y":0.460426179604262,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(26,26,26,1)","family":"","size":11.689497716895},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"center","yanchor":"bottom"},{"text":"r&b","x":0.5,"y":0.460426179604262,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(26,26,26,1)","family":"","size":11.689497716895},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"center","yanchor":"bottom"},{"text":"rock","x":0.837561305597835,"y":0.460426179604262,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(26,26,26,1)","family":"","size":11.689497716895},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"center","yanchor":"bottom"}],"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":0.324877388804329,"y0":0.539573820395738,"y1":1},{"type":"rect","fillcolor":"rgba(255,255,255,1)","line":{"color":"rgba(0,0,0,1)","width":1.32835201328352,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0,"x1":0.324877388804329,"y0":0,"y1":23.37899543379,"yanchor":1,"ysizemode":"pixel"},{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0.341789277862337,"x1":0.658210722137663,"y0":0.539573820395738,"y1":1},{"type":"rect","fillcolor":"rgba(255,255,255,1)","line":{"color":"rgba(0,0,0,1)","width":1.32835201328352,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0.341789277862337,"x1":0.658210722137663,"y0":0,"y1":23.37899543379,"yanchor":1,"ysizemode":"pixel"},{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0.675122611195671,"x1":1,"y0":0.539573820395738,"y1":1},{"type":"rect","fillcolor":"rgba(255,255,255,1)","line":{"color":"rgba(0,0,0,1)","width":1.32835201328352,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0.675122611195671,"x1":1,"y0":0,"y1":23.37899543379,"yanchor":1,"ysizemode":"pixel"},{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":0.324877388804329,"y0":0,"y1":0.460426179604262},{"type":"rect","fillcolor":"rgba(255,255,255,1)","line":{"color":"rgba(0,0,0,1)","width":1.32835201328352,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0,"x1":0.324877388804329,"y0":0,"y1":23.37899543379,"yanchor":0.460426179604262,"ysizemode":"pixel"},{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0.341789277862337,"x1":0.658210722137663,"y0":0,"y1":0.460426179604262},{"type":"rect","fillcolor":"rgba(255,255,255,1)","line":{"color":"rgba(0,0,0,1)","width":1.32835201328352,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0.341789277862337,"x1":0.658210722137663,"y0":0,"y1":23.37899543379,"yanchor":0.460426179604262,"ysizemode":"pixel"},{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0.675122611195671,"x1":1,"y0":0,"y1":0.460426179604262},{"type":"rect","fillcolor":"rgba(255,255,255,1)","line":{"color":"rgba(0,0,0,1)","width":1.32835201328352,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0.675122611195671,"x1":1,"y0":0,"y1":23.37899543379,"yanchor":0.460426179604262,"ysizemode":"pixel"}],"xaxis2":{"type":"linear","autorange":false,"range":[0.4,2.6],"tickmode":"array","ticktext":["Multiple Artists","One Artist"],"tickvals":[1,2],"categoryorder":"array","categoryarray":["Multiple Artists","One Artist"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":true,"linecolor":"rgba(0,0,0,1)","linewidth":0.66417600664176,"showgrid":false,"domain":[0.341789277862337,0.658210722137663],"gridcolor":null,"gridwidth":0,"zeroline":false,"anchor":"y2","title":"","hoverformat":".2f"},"xaxis3":{"type":"linear","autorange":false,"range":[0.4,2.6],"tickmode":"array","ticktext":["Multiple Artists","One Artist"],"tickvals":[1,2],"categoryorder":"array","categoryarray":["Multiple Artists","One Artist"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":true,"linecolor":"rgba(0,0,0,1)","linewidth":0.66417600664176,"showgrid":false,"domain":[0.675122611195671,1],"gridcolor":null,"gridwidth":0,"zeroline":false,"anchor":"y2","title":"","hoverformat":".2f"},"yaxis2":{"type":"linear","autorange":false,"range":[-1.10547667342799,23.2150101419878],"tickmode":"array","ticktext":["0","5","10","15","20"],"tickvals":[0,5,10,15,20],"categoryorder":"array","categoryarray":["0","5","10","15","20"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":true,"linecolor":"rgba(0,0,0,1)","linewidth":0.66417600664176,"showgrid":false,"domain":[0,0.460426179604262],"gridcolor":null,"gridwidth":0,"zeroline":false,"anchor":"x","title":"","hoverformat":".2f"},"showlegend":false,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","showSendToCloud":false},"source":"A","attrs":{"8f4a48eed3c3":{"x":{},"y":{},"type":"bar"}},"cur_data":"8f4a48eed3c3","visdat":{"8f4a48eed3c3":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

Rap had the largest change between songs with multiple artists and song with only one, with a gap of 10%. Based on other genres' disparity like R&B and pop, I hypothesize that these trends are connected with the modern day music scene. Collaboration between artists at the top of their field has become more commonplace, even with some mega-tracks like "Forever" with verses from Eminem, Kanye West, Drake, and Lil Wayne all in the same song. These collaborations can create songs with blended styles, which even furthers the development of rap as a unique genre.



## R&B

### Why R&B?

In this section, I want to take a closer look at one of my favorite genres of music, R&B. I think I love it so much because it's often good music to unwind to -- it's smooth, slow, and relaxing. I also love its versatility! R&B can fit the mood of anything from a gloomy, rainy day to a bright, sunny day. But why? What characteristics make R&B such a great genre to listen to? Using the Spotify dataset and some visualizations which look at the specific characteristics of the most popular R&B songs (songs with a popularity rating of above 75), I hope to come closer to answering these questions.


```r
randb %>%
  select(track_name, track_artist, playlist_genre, playlist_subgenre, track_popularity, danceability, energy, loudness, mode, speechiness, acousticness, instrumentalness, liveness, valence, duration_ms) %>%
  arrange(desc(track_popularity)) %>%
  head(12) %>%
  knitr::kable() 
```



|track_name          |track_artist   |playlist_genre |playlist_subgenre  | track_popularity| danceability| energy| loudness| mode| speechiness| acousticness| instrumentalness| liveness| valence| duration_ms|
|:-------------------|:--------------|:--------------|:------------------|----------------:|------------:|------:|--------:|----:|-----------:|------------:|----------------:|--------:|-------:|-----------:|
|ROXANNE             |Arizona Zervas |r&b            |urban contemporary |               99|        0.621|  0.601|   -5.616|    0|      0.1480|      0.05220|         0.000000|   0.4600|   0.457|      163636|
|ROXANNE             |Arizona Zervas |r&b            |hip pop            |               99|        0.621|  0.601|   -5.616|    0|      0.1480|      0.05220|         0.000000|   0.4600|   0.457|      163636|
|The Box             |Roddy Ricch    |r&b            |urban contemporary |               98|        0.896|  0.586|   -6.687|    0|      0.0559|      0.10400|         0.000000|   0.7900|   0.642|      196653|
|Memories            |Maroon 5       |r&b            |urban contemporary |               98|        0.764|  0.320|   -7.209|    1|      0.0546|      0.83700|         0.000000|   0.0822|   0.575|      189486|
|Blinding Lights     |The Weeknd     |r&b            |urban contemporary |               98|        0.513|  0.796|   -4.075|    1|      0.0629|      0.00147|         0.000209|   0.0938|   0.345|      201573|
|Blinding Lights     |The Weeknd     |r&b            |hip pop            |               98|        0.513|  0.796|   -4.075|    1|      0.0629|      0.00147|         0.000209|   0.0938|   0.345|      201573|
|The Box             |Roddy Ricch    |r&b            |hip pop            |               98|        0.896|  0.586|   -6.687|    0|      0.0559|      0.10400|         0.000000|   0.7900|   0.642|      196653|
|Tusa                |KAROL G        |r&b            |hip pop            |               98|        0.803|  0.715|   -3.280|    1|      0.2980|      0.29500|         0.000134|   0.0574|   0.574|      200960|
|Memories            |Maroon 5       |r&b            |hip pop            |               98|        0.764|  0.320|   -7.209|    1|      0.0546|      0.83700|         0.000000|   0.0822|   0.575|      189486|
|Circles             |Post Malone    |r&b            |hip pop            |               98|        0.695|  0.762|   -3.497|    1|      0.0395|      0.19200|         0.002440|   0.0863|   0.553|      215280|
|Don't Start Now     |Dua Lipa       |r&b            |urban contemporary |               97|        0.794|  0.793|   -4.521|    0|      0.0842|      0.01250|         0.000000|   0.0952|   0.677|      183290|
|everything i wanted |Billie Eilish  |r&b            |urban contemporary |               97|        0.704|  0.225|  -14.454|    0|      0.0994|      0.90200|         0.657000|   0.1060|   0.243|      245426|

Above are the top 10 most popular songs in the R&B genre (12 songs were pulled from the dataset to account for 2 songs that were each in 2 different subgenres -- Arizona Zeravas' *Roxanne* and The Weeknd's *Blinding Lights*. We can see that all of them were released in 2019 and all categorized under my two favorite two subgenres of R&B, Urban Contemporary and Hip Pop. All of them also boast a danceability score of above 0.5, with most of them (with the exception of Maroon 5's *Memories* and Billie Eilish's *everything i wanted*) having energy scores of above 0.5. We can also see that across the board, all 10 songs have low speechiness and instrumentalness scores (with the exception of Billie Eilish's *everything i wanted*. Interestingly, all of the songs fall within a valence of 0.2-0.6. The other characteristics are quite varied. So, for the purposes of my analysis of the R&B genre, I will only focus on the song characteristics that have clear trends across the genre -- danceabiility, energy, speechiness, instrumentalness, and valence.

### Which subgenres have the most popular songs?

In the exploratory phase of my analysis of the R&B genre, the most obvious characteristic of a song in the R&B genre was a song's subgenre. Are certain genres more likely to have more popular songs because some have more fans and listeners than others? In the density plot below, we see that this is the case -- Neo-Soul and New Jack Swing have the highest quantity of popular songs. 


```r
randb %>% 
  ggplot(aes(x = track_popularity, fill = playlist_subgenre)) +
  geom_density(alpha = 0.1) +
  theme_classic() +
  labs(title = "Do certain subgenres have more popular songs?",
       subtitle = "This density plot only includes songs with a popularity of >=75.\nIt seems that Neo-Soul and New Jack Swing have the most popular songs.\n\nR&B Subgenre: {closest_state}",
       x = "Track Popularity",
       y = "",
       fill = "R&B subgenre",
       caption = "Visualization created by Brian Lee") +
  transition_states(playlist_subgenre, transition_length = 3, state_length = 1)
#get rid of axes, make subtitle descriptive
anim_save("randb_density.gif")
```


```r
knitr::include_graphics("randb_density.gif")
```

![](randb_density.gif)<!-- -->

In the density plot above, Neo-Soul and New Jack Swing both seem to have a lot of popular songs on the lower end of the spectrum (75-85), with Urban Contemporary and Hip Pop following similar trends, but in comparison to the other two genres, their density curves are not as large, signaling that the former two genres have more songs classified as "popular" than teh latter two.

I believe that this trend could be occurring because of the huge increase in the production of hip pop and urban contemporary music. With streaming services such as Spotify making it easier than ever for small creators to attain platforms and with the advancement of technology making it easier to produce and release music from one's own bedroom, this may be because of the oversaturation of the music industry -- there are more songs being released than ever.


```r
randb %>%
  group_by(playlist_subgenre) %>%
  summarize(num_of_songs = n(), avg_pop = mean(track_popularity)) %>%
  knitr::kable() 
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```



|playlist_subgenre  | num_of_songs|  avg_pop|
|:------------------|------------:|--------:|
|hip pop            |          281| 82.95018|
|neo soul           |           41| 78.90244|
|new jack swing     |            4| 77.50000|
|urban contemporary |          204| 82.13725|

Despite the large density curves, on average, hip pop and urban contemporary are slightly more popular than the Neo-Soul and New Jack Swing. Another interesting observation we can make is the sheer lack of popular songs for Neo-Soul and New Jack Swing. 

A quick Google search will reveal that both Neo-Soul and New Jack Swing were subgenres of R&B that were popular during the 1980's/90's. Their large density curves could be due to this fact. Because the technology for household high quality handheld microphones and producing equipment was not in abundance like it is now, artists had to rely on label companies and managers for the funding to acquire the money for studios and expensive equipment, thus leading to less music being produced. Additionally, because labeling agencies and managerial agencies essentially "invested" in discovered artists whom they knew they would get a high profit margin from, the discovered artists who were given a platform by these agencies were more likely to be successful. With a smaller pool of music and more popular songs making up that small poool of music, large density curves such as the ones we see in the visualization above for Neo-Soul and New Jack Swing are possible, and could serve as an explanation for the difference in the quantity between the four genres.

As I move forward in my analysis to look at the specific characteristics of popular R&B songs, I will restrict myself to the two subgenres with more cases to look at and my person two favorite subgenres -- Hip-Pop and Urban Contemporary.

### Taking a closer look at the specific characteristics of popular songs in the Hip-Pop and Urban Contemporary Subgenres


```r
randb %>%
  group_by(playlist_subgenre) %>%
  filter(playlist_subgenre == c("hip pop", "urban contemporary")) %>%
  summarise_at(c("track_popularity", "danceability", "energy", "speechiness", "instrumentalness", "valence"), mean, na.rm = TRUE) %>%
  knitr::kable() 
```

```
## Warning in playlist_subgenre == c("hip pop", "urban contemporary"): longer
## object length is not a multiple of shorter object length

## Warning in playlist_subgenre == c("hip pop", "urban contemporary"): longer
## object length is not a multiple of shorter object length
```



|playlist_subgenre  | track_popularity| danceability|    energy| speechiness| instrumentalness|   valence|
|:------------------|----------------:|------------:|---------:|-----------:|----------------:|---------:|
|hip pop            |         82.62411|    0.6985887| 0.6000979|   0.1304929|        0.0120022| 0.4780922|
|urban contemporary |         81.98039|    0.6823333| 0.5401578|   0.1340971|        0.0135849| 0.4606735|

```r
# Add graph
```

# What now? Why is our analysis important?

As it becomes easier to produce and release music from one's own bedroom and streaming platforms such as Apple Music and Spotify increasingly making music accessible to everyone, we believe our analysis has important implications which can help listeners find new songs that they like and help platforms build algorithms that give better and more relevant song recommendations to its users. 

### An Acknowledgement: Correlation does no equal causation.

As we close our analysis of what makes a song popular, it would be remiss if we did not acknowledge that just because we have identified certain characteristics that make a song have a high popularity rating, it does not necessarily mean that *if* a certain song has all the preferred characteristics that we mentioned in our analyses, it *will* be popular, as we can never know whether or not something will with 100% certainly. 
There is also a factor of human behavior that we simply cannot predict using data science. We can try our best to follow trends and to look at graphs, but trying to predict whether or not a song willl be well liked by its audience is something that must be taken with a grain of salt. At its core, the purpose of our analysis was simply to make general observations about trends that caught the eye -- not to predict with complete certainty the popularity of a song.

## Possibilities for further analysis & A Conclusion
If we had more time, we believe it would be fascinating and useful to build a shiny app where people coul adjust their preferences for features they look for in songs their favorite songs (eg. high danceability, low valence, etc.) so that the app could return recommended songs with similar values. Or, we could have users input their favorite songs to find recommendations based on similar characteristic and song attributes. We could also create playlists, similar to what Spotify does. By determining recommended values for different moods and purposes such as relaxation, meditating, and studying, it would be intersting to see how accurate these playlists are. Again, as we mentioned in the previous section, we cannot predict human judgement, so we do expect some level of human error in determining the "correctness" of the playlists. The possibilities with this dataset are endless!

Thanks to streaming platforms such as Spotify and Apple Music, small creators are also given a platform for creative release. Our analyses of pop, rap, and R&B, can also help small artists grow their own platforms to cater to the interests of specific audiences. In a time such as now when the consumption of art (whether it be in the form of movies, music, or television), is essential to one's mental wellbeing, our analysis can help boost these efforts. By asking the question, "What makes a song in a given genre popular?" We have taken a close look at the specific characteristics of songs with a popularity rating of 75 or higher.






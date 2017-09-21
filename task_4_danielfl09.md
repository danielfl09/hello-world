# Task 4: Becoming a databender


## Background
This report was created in response to the following scenari;

"You just started your internship at a big firm in New York, and your manager gave you an extensive file of flights that departed JFK, LGA, or EWR in 2013. From this data (install.packages(nycflights13)) your manager wants you to answer the following questions;

  1. If I am leaving before noon, what are my top two airline options at each airport (JFK, LGA, EWR) that will have the least amount of delay time?
  
  2. Which origin airport is best to minimize my chances of a late arrival when I am using Delta Airlines?
  
  3. Which destination airport is the worst airport for arrival time?
"

### If I am leaving before noon, what are my top two airline options at each airport (JFK, LGA, EWR) that will have the least amount of delay time?<br><br>

In order to answer this question, I first filtered out of the data every flight that departed after 12:00 PM. Then for each of the origin airports, I created a new set of data that only included flights that left from that airport. I then used the "transmute" function to create new tables of data, one for each origin airport, that contained the columns "sched_dep_time", "carrier", and a new column called "total_delay" that contained the total delay time for each flight. All that was left to do was arrange the data so that the carriers that had the lowest average total delay time would appear at the top row. The lists found below are the result after arranging the data. 

The airlines with the lowest average delay time are Delta and American at JFK, US and American at LGA, and Alaskan and Virgin America at EWR. 


```r
early_jfk <- filter(flights, sched_dep_time <= 1200 & origin == "JFK")
q1 <- transmute(early_jfk,
                sched_dep_time,
                carrier,
                total_delay = arr_delay + dep_delay
                )
q1g <- group_by(q1, carrier)
summ1.1 <- summarise(q1g, avg_delay = mean(total_delay, na.rm = TRUE))
v1 <- arrange(summ1.1, avg_delay)

early_lga <- filter(flights, sched_dep_time <= 1200 & origin == "LGA")
q1 <- transmute(early_lga,
                sched_dep_time,
                carrier,
                total_delay = arr_delay + dep_delay
                )
q1g <- group_by(q1, carrier)
summ1.2 <- summarise(q1g, avg_delay = mean(total_delay, na.rm = TRUE))
v2 <- arrange(summ1.2, avg_delay)

early_ewr <- filter(flights, sched_dep_time <= 1200 & origin == "EWR")
q1 <- transmute(early_ewr,
                sched_dep_time,
                carrier,
                total_delay = arr_delay + dep_delay
                )
q1g <- group_by(q1, carrier)
summ1.3 <- summarise(q1g, avg_delay = mean(total_delay, na.rm = TRUE))
v3 <- arrange(summ1.3, avg_delay)

sidebyside <- function(..., width=60){
  l <- list(...)
  p <- lapply(l, function(x){
        xx <- capture.output(print(x, width=width))
        xx <- gsub("\"", "", xx)
        format(xx, justify="left", width=width)
      }
  )
  p <- do.call(cbind, p)
  sapply(seq_len(nrow(p)), function(x)paste(p[x, ], collapse=""))
}
t1 <- "JFK"
t2 <- "LGA"
t3 <- "EWR"

sidebyside(t1, t2, t3, width = 30)
```

```
[1] "[1] JFK                       [1] LGA                       [1] EWR                       "
```

```r
sidebyside(v1, v2, v3, width = 30)
```

```
 [1] "# A tibble: 10 x 2            # A tibble: 13 x 2            # A tibble: 11 x 2            "
 [2] "   carrier avg_delay             carrier   avg_delay           carrier   avg_delay        "
 [3] "     <chr>     <dbl>               <chr>       <dbl>             <chr>       <dbl>        "
 [4] " 1      DL -7.831231           1      US  -5.9612145         1      AS -19.8405797        "
 [5] " 2      AA -3.786742           2      AA  -5.0783561         2      VX  -5.9070968        "
 [6] " 3      UA -2.812810           3      B6  -4.3574553         3      AA  -5.2432229        "
 [7] " 4      VX -2.433058           4      DL   0.2078032         4      B6  -4.5170666        "
 [8] " 5      HA -2.014620           5      9E   1.5339147         5      US  -4.3768889        "
 [9] " 6      US  3.602218           6      UA   2.0337243         6      WN  -1.2692822        "
[10] " 7      B6  5.013091           7      WN   3.9556650         7      UA   0.6060606        "
[11] " 8      9E  9.585219           8      MQ   5.2069500         8      9E   1.5130785        "
[12] " 9      MQ 12.558920           9      EV  12.5629283         9      DL   2.9970588        "
[13] "10      EV 16.271624          10      FL  16.0352734        10      EV  12.4964867        "
[14] "# A tibble: 10 x 2            11      F9  16.8465909        11      MQ  21.3277662        "
[15] "   carrier avg_delay          12      YV  18.6702128        # A tibble: 11 x 2            "
[16] "     <chr>     <dbl>          13      OO 174.0000000           carrier   avg_delay        "
```
<br><br>

 

### Which origin airport is best to minimize my chances of a late arrival when I am using Delta Airlines?

For this question, I first created a new table of only Delta's flight data. From there, I grouped the data by the origin of flight. Then I summarized the data by average arrival delay time, and arranged it so that the airport with the lowest average was shown at the top of the list. The result can be seen below.

The JFK airport has the lowest average arrival delay time for all Delta flights, which means it's your best option for minimizing your chance of a late arrival when using Delta.


```r
on_time <- filter(flights, carrier == "DL")
q2 <- group_by(on_time, origin)
summ2 <- summarise(q2, avg_delay = mean(arr_delay, na.rm = TRUE))
arrange(summ2, avg_delay)
```

```
# A tibble: 3 x 2
  origin avg_delay
   <chr>     <dbl>
1    JFK -2.379250
2    LGA  3.927776
3    EWR  8.780442
```


### Which destination airport is the worst airport for arrival time?

This question was relatively easy to answer. All I had to do was group together all of the destinations of all flights and summarize the data by average arrival time. Then I arranged the data in descending order so that the destination with the highest average arrival delay time was placed at the top. 

The Columbia Metropolitan Airport has the highest average arrival delay time out of all the destinations in the data. 


```r
q3 <- group_by(flights, dest)
summ3 <- summarise(q3, avg_delay = mean(arr_delay, na.rm = TRUE))
arrange(summ3, desc(avg_delay))
```

```
# A tibble: 105 x 2
    dest avg_delay
   <chr>     <dbl>
 1   CAE  41.76415
 2   TUL  33.65986
 3   OKC  30.61905
 4   JAC  28.09524
 5   TYS  24.06920
 6   MSN  20.19604
 7   RIC  20.11125
 8   CAK  19.69834
 9   DSM  19.00574
10   GRR  18.18956
# ... with 95 more rows
```


<!-- 
### If I am leaving before noon, what are my top two airline options at each airport (JFK, LGA, EWR) that will have the least amount of delay time?<br><br>

##### John F. Kennedy International Airport (JFK)

```r
early_jfk <- filter(flights, sched_dep_time <= 1200 & origin == "JFK")
q1 <- transmute(early_jfk,
                sched_dep_time,
                carrier,
                total_delay = arr_delay + dep_delay
                )
q1g <- group_by(q1, carrier)
summ1.1 <- summarise(q1g, avg_delay = mean(total_delay, na.rm = TRUE))
arrange(summ1.1, avg_delay)
```

```
# A tibble: 10 x 2
   carrier avg_delay
     <chr>     <dbl>
 1      DL -7.831231
 2      AA -3.786742
 3      UA -2.812810
 4      VX -2.433058
 5      HA -2.014620
 6      US  3.602218
 7      B6  5.013091
 8      9E  9.585219
 9      MQ 12.558920
10      EV 16.271624
```
<br><br>

##### LaGuardia Airport (LGA)

```r
early_lga <- filter(flights, sched_dep_time <= 1200 & origin == "LGA")
q1 <- transmute(early_lga,
                sched_dep_time,
                carrier,
                total_delay = arr_delay + dep_delay
                )
q1g <- group_by(q1, carrier)
summ1.2 <- summarise(q1g, avg_delay = mean(total_delay, na.rm = TRUE))
arrange(summ1.2, avg_delay)
```

```
# A tibble: 13 x 2
   carrier   avg_delay
     <chr>       <dbl>
 1      US  -5.9612145
 2      AA  -5.0783561
 3      B6  -4.3574553
 4      DL   0.2078032
 5      9E   1.5339147
 6      UA   2.0337243
 7      WN   3.9556650
 8      MQ   5.2069500
 9      EV  12.5629283
10      FL  16.0352734
11      F9  16.8465909
12      YV  18.6702128
13      OO 174.0000000
```
<br><br>

##### Newark Liberty International Airport (EWR)

```r
early_ewr <- filter(flights, sched_dep_time <= 1200 & origin == "EWR")
q1 <- transmute(early_ewr,
                sched_dep_time,
                carrier,
                total_delay = arr_delay + dep_delay
                )
q1g <- group_by(q1, carrier)
summ1.3 <- summarise(q1g, avg_delay = mean(total_delay, na.rm = TRUE))
arrange(summ1.3, avg_delay)
```

```
# A tibble: 11 x 2
   carrier   avg_delay
     <chr>       <dbl>
 1      AS -19.8405797
 2      VX  -5.9070968
 3      AA  -5.2432229
 4      B6  -4.5170666
 5      US  -4.3768889
 6      WN  -1.2692822
 7      UA   0.6060606
 8      9E   1.5130785
 9      DL   2.9970588
10      EV  12.4964867
11      MQ  21.3277662
```
<br><br>




### Which origin airport is best to minimize my chances of a late arrival when I am using Delta Airlines?


```r
on_time <- filter(flights, carrier == "DL")
q2 <- group_by(on_time, origin)
summ2 <- summarise(q2, avg_delay = mean(arr_delay, na.rm = TRUE))
arrange(summ2, avg_delay)
```

```
# A tibble: 3 x 2
  origin avg_delay
   <chr>     <dbl>
1    JFK -2.379250
2    LGA  3.927776
3    EWR  8.780442
```


### Which destination airport is the worst airport for arrival time?


```r
q3 <- group_by(flights, dest)
summ3 <- summarise(q3, avg_delay = mean(arr_delay, na.rm = TRUE))
arrange(summ3, desc(avg_delay))
```

```
# A tibble: 105 x 2
    dest avg_delay
   <chr>     <dbl>
 1   CAE  41.76415
 2   TUL  33.65986
 3   OKC  30.61905
 4   JAC  28.09524
 5   TYS  24.06920
 6   MSN  20.19604
 7   RIC  20.11125
 8   CAK  19.69834
 9   DSM  19.00574
10   GRR  18.18956
# ... with 95 more rows
```

-->

# Case_Study_03
Daniel Lopez  



##Background
The [FiveThrirtyEight](http://fivethirtyeight.com/) article titled ["Gun Deaths In America"](https://fivethirtyeight.com/features/gun-deaths/) illustrates the different causes for each death by gun recorded by the CDC between the years of 2012 and 2014. Their use of the word "cause" refers to the intent of the shooter. A lot of today's mainstream media would have us think that the majority of gun deaths in America are caused by wither mass shooters, terrorists, gang members, and crooked cops. The truth is, according to data collected by the CDC for the years 2012 through 2014, the majority of deaths by gun are suicides. The graph below summarizes the data by intent, similar to the graph shown in FiveThirtyEight's website.



```r
#summarise the data to find the total number of gun deaths per intent
gun_data2 <- 
  five_thiry_eight %>%
  filter(!is.na(intent)) %>%
  group_by(intent, sex) %>%
  summarise(n = n())

#create a histogram to summarise the data
gun_data2 %>%
  ggplot(aes(x = intent, y = n, color = sex)) +
  geom_histogram(stat = "identity", binwidth = 10) +
  labs(title = "There are more suicides by gun than any other gun related death",
       x = "Intent behind gun death",
       y = "Total gun deaths between the years 2012 - 2014")
```

![](Case_Study_03_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

## Analysis
The graph shown above illustrates two things; the majority of deaths caused by guns are suicides, and the majority of suicide victims are male. This gives us an idea of what demographic would be most effective to target with commercials. The next few visuals will help us narrow down that demographic, first by race, then by time of year. 


```r
gun_data_byrace <-
  five_thiry_eight %>%
  filter(race == c("Black", "Hispanic", "White"), intent == c("Homicide", "Suicide")) %>%
  group_by(year, month, intent, race) %>%
  summarise(amount = n())

gun_data_byrace %>%
  ggplot(aes(x = month, y = amount, color = intent)) +
  geom_histogram(stat = "identity") +
  scale_x_discrete(limits = c(1:12),
                   labels = c("1" = "Jan",
                              "2" = "Feb",
                              "3" = "Mar",
                              "4" = "Apr",
                              "5" = "May",
                              "6" = "Jun",
                              "7" = "Jul",
                              "8" = "Aug",
                              "9" = "Sep",
                              "10" = "Oct",
                              "11" = "Nov",
                              "12" = "Dec")) +
  theme(panel.spacing = unit(1, "lines")) +
  facet_wrap(~race, nrow = 3)
```

![](Case_Study_03_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

It's important to note that some ethnicities are not shown here. Native American, Native Alaskan, Asian and Pacific Islander are not shown because of their consistently low gun death rate. To give you an idea, The numbers shown on the hispanic graph are almost double the combined numbers of the excluded ethnicities. Looking at the other plots, it is easy to tell that Blacks suffer Homicides more than whites. Whites suffer from suicides more than all of the other races combined. It appears that whites may be the best race to target with anti-suicide and gun death ads. 


```r
gun_data_white <-
  five_thiry_eight %>%
  filter(race == "White") %>%
  group_by(year, month, intent, race) %>%
  summarise(amount = n())

gun_data_white %>%
  ggplot(aes(x = month, y = amount, color = intent)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  scale_x_discrete(limits = c(1:12),
                   labels = c("1" = "Jan",
                              "2" = "Feb",
                              "3" = "Mar",
                              "4" = "Apr",
                              "5" = "May",
                              "6" = "Jun",
                              "7" = "Jul",
                              "8" = "Aug",
                              "9" = "Sep",
                              "10" = "Oct",
                              "11" = "Nov",
                              "12" = "Dec")) +
  
  theme(panel.spacing = unit(1, "lines")) +
  facet_wrap(~race, nrow = 1)
```

```
`geom_smooth()` using method = 'loess'
```

![](Case_Study_03_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

## Conclusion
This last plot shows the trends in the white data for gun deaths over the course of the year. It appears that the months May through August have the highest amount of suicides by gun compared to the rest of the year. In conclusion, I reccommend that your commercials target suicidal whites, and air most frequently during the months of May through August.  


<br><br><br><br>










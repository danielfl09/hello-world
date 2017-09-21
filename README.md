# hello-world
The is part of the tutorial from GitHub
My name is Daniel and I'm from California. I used to be an accounting major but I decided to change after learning about data science. 


<!-- 

```{r, include=FALSE}
top_two <- select(flights, sched_dep_time, dep_delay, arr_delay, carrier)

top_two$delay_total <- top_two$dep_delay + top_two$arr_delay

top_two <- filter(top_two, sched_dep_time <= 1200)

top_two <- group_by(top_two, carrier)

top_two <- arrange(top_two, (arr_delay + dep_delay))
```

```{r, comment=NA}
print(summarise(top_two, delay_avg = mean(delay_total, na.rm = TRUE)))
```

-->
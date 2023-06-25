Intro to APIs: Interacting with PokeAPI
================
Landon Batts
2023-06-21

- <a href="#rawtocharacter"
  id="toc-rawtocharacter"><code>rawtocharacter</code></a>
- <a href="#pokemonapi" id="toc-pokemonapi"><code>pokemonapi</code></a>

This is a vignette to show how to retrieve data from an API. In order to
demonstrate, I will be retrieving data from a Pokemon API displaying
data of various topics about Pokemon and it’s universe.

I will be writing functions in order to contact
[PokeAPI](https://pokeapi.co/) and return usable data frames that can be
used to display summaries.

\##Required Packages In order to create functions to contact and
interact with PokeAPI, the following packages are needed:

- [`tidyverse`](https://www.tidyverse.org/packages/) - Family of
  packages for manipulating and visualizing data
- [`jsonlite`](https://cran.r-project.org/web/packages/jsonlite/index.html) -
  package for dealing with and reading in JSON data in R
- [`httr`](https://cran.r-project.org/web/packages/httr/vignettes/quickstart.html)-
  package used for contacting the API in order to retrieve data
- [`knitr`](https://cran.r-project.org/web/packages/knitr/index.html):
  displaying tables in a markdown friendly way

``` r
library(httr)
library(jsonlite)
library(tidyverse)
```

\##API Interaction Functions In order to actually access and interact
with data from the API, I created some functions to make it easier and
user-friendly.

### `rawtocharacter`

I created this function to apply the rawToChar function in order to
convert the raw JSON data to character variables in order to be in a
usable form

``` r
rawtocharacter <- function(x) {
  rawToChar(x$content)
}
```

### `pokemonapi`

I decided to make a function to call the PokeAPI and be able to extract
data into a usable form based on the user’s options

``` r
pokemonapi <- function(endpoint){
  pokemon <- GET("https://pokeapi.co/api/v2/pokemon?limit=100000&offset=0") #GET function calls to PokeAPI
  parsed <- fromJSON(rawToChar(pokemon$content)) #used fromJSON function to get parsed data in character form
  pokeurl <- parsed$results #saves urls from parsed results
  pokedata <- lapply(lapply(lapply(pokeurl$url,GET),rawtocharacter),fromJSON) #applies GET function to each url in order to get data from all endpoints
  if(endpoint=="abilities"){
    pokedf <- pokeurl %>% select(url) %>% add_column(pokedata) %>%
  unnest_wider(pokedata,names_repair='minimal') %>% 
  unnest_longer(col=abilities)#returns dataset with url and data for ability for each pokemon
    
    pokedf <- pokedf %>% cbind(ability=as.vector(pokedf$abilities$ability$name)) %>% select(name,ability,base_experience,height,weight)
    #makes variable form dataframe variable in order to select usable columns
  }
  if(endpoint=="moves"){
    pokedf <- pokeurl %>% select(url) %>% add_column(pokedata) %>%
  unnest_wider(pokedata,names_repair='minimal') %>% 
  unnest_longer(col=moves) #returns dataset with url and data for each move for every pokemon from moves endpoint
    
    pokedf <- pokedf %>% cbind(move=as.vector(pokedf$moves$move$name)) %>% select(name,move,base_experience,height,weight)
  }
  if(endpoint=="forms"){
    pokedf <- pokeurl %>% select(url) %>% add_column(pokedata) %>%
  unnest_wider(pokedata,names_repair='minimal') %>% 
  unnest_longer(col=forms) #returns dataset with url and data for each form for every pokemon from form endpoint
    
    pokedf <- pokedf %>% cbind(form=as.vector(pokedf$forms$name)) %>% select(name,form,base_experience,height,weight)
  }
  if(endpoint=="stats"){
    pokedf <- pokeurl %>% select(url) %>% add_column(pokedata) %>%
  unnest_wider(pokedata,names_repair='minimal') %>% 
  unnest_longer(col=stats) #returns dataset with url and data for each stat and their level for every pokemon from stats endpoint
    
    pokedf <- pokedf %>% cbind(basestat=as.vector(pokedf$stats$base_stat),statname=as.vector(pokedf$stats$stat$name)) %>% select(name,statname,basestat,base_experience,height,weight)
  }
  if(endpoint=="types"){
    pokedf <- pokeurl %>% select(url) %>% add_column(pokedata) %>%
  unnest_wider(pokedata,names_repair='minimal')  %>%
  unnest_longer(col=types) #returns dataset with url and data for each type for every pokemon from types endpoint
    
    pokedf <- pokedf %>% cbind(type=as.vector(pokedf$types$type$name)) %>% select(name,type,base_experience,height,weight)
  }
  if(endpoint=="game"){
    pokedf <- pokeurl %>% select(url) %>% add_column(pokedata) %>%
  unnest_wider(pokedata,names_repair='minimal') %>% 
  unnest_longer(col=game_indices) #returns dataset with url and data for each game that the Pokemon are included for every pokemon from the game_indices endpoint
    pokedf <- pokedf %>% cbind(games=as.vector(pokedf$game_indices$version$name)) %>% select(name,games,base_experience,height,weight)
  }
  return(pokedf)
}
```

With this function, the user is able to specify one of 6 potential
endpoints available and returns a dataframe with data for each. Also
included in the data frame is common statistics for Pokemon including
experience, height, and weight.

\##Data Exploration Now that the function to read in a dataframe has
been created, we can use this to create summaries and graphs. First,
I’ll get a dataset for each endpoint in order to have data from each
stat in a usable form

``` r
abilities <- pokemonapi(endpoint="abilities")
moves <- pokemonapi(endpoint="moves")
forms <- pokemonapi(endpoint="forms")
stat <- pokemonapi(endpoint="stats")
stat <- na.omit(stat)
types <- pokemonapi(endpoint="types")
game <- pokemonapi(endpoint="game")
table(types$type)
```

    ## 
    ##      bug     dark   dragon electric    fairy fighting     fire 
    ##      104       94      102      109       82       99      101 
    ##   flying    ghost    grass   ground      ice   normal   poison 
    ##      149       89      144       92       66      154       98 
    ##  psychic     rock    steel    water 
    ##      133      100       89      185

``` r
table(game$games)
```

    ## 
    ##      black    black-2       blue    crystal    diamond    emerald 
    ##        667        673        151        251        498        386 
    ##    firered       gold  heartgold  leafgreen omega-ruby      pearl 
    ##        386        251        505        386         27        498 
    ##   platinum        red       ruby   sapphire     silver soulsilver 
    ##        505        151        386        386        251        505 
    ##      white    white-2     yellow 
    ##        667        673        151

``` r
#Creates contingency table for amount of pokemon for each type and games they are in
g <- ggplot(data=types,aes(x=type)) #Creates object for type data that can be used to create plots
g + geom_bar(aes(fill="red")) +
theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position = "none") + labs(x="Pokemon Type",y="Count",title="Pokemon Type Bar Graph")
```

![](README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
#Creates labeled bar graph with red bars of pokemon type
#There is a pretty even spread of pokemon types with water having the most pokemon and ice having the least
types <- types %>% mutate(heightweight=weight/height)
#Creates new variable in types data set for the weight height ratio in order to have a continuous variable for plots
g2 <- ggplot(data=types,aes(x=heightweight))
g2 + geom_histogram(fill="red") + xlim(0,400)+labs(title="Distribution of Weight/Height Ratio", x="Weight/Height Ratio",y="Count")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with
    ## `binwidth`.

![](README_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->

``` r
#From the graph, the distribution appears right skewed as a majority of pokemon are smaller with some larger ones such as snorlax
g2 + geom_boxplot()+ xlim(0,400) + labs(title="Boxplot of Pokemon Weight/Height Ratio",x="Weight/Height Ratio")
```

![](README_files/figure-gfm/unnamed-chunk-8-3.png)<!-- -->

``` r
#Since there is such a wide range of ratios and the data is skewed, there are many outliers present. It appears that the median ratio is around 30 while the maximum is all the way at nearly 1000
g3 <- ggplot(data=stat,aes(x=basestat,y=base_experience))
g3+geom_point(aes(col=statname)) + labs(title="Base Stat Level vs Base XP", x="Base Stat",y="Base Experience")
```

![](README_files/figure-gfm/unnamed-chunk-8-4.png)<!-- -->

``` r
#This creates a scatterplot comparing the relationship between the base level of a Pokemons stat with their base level experience
#It appears to be somewhat of a positive relationship as the stonger pokemon tend to have more experience.
#I also grouped by statname to see if there were a difference for each type of stat although all stats seem to increase along with experience as well
g4 <- ggplot(data=stat,aes(x=basestat))
g4 + geom_boxplot() + facet_wrap(~statname) + labs(title="Base Stat level for each Stat Type",x="Base Stat")
```

![](README_files/figure-gfm/unnamed-chunk-8-5.png)<!-- -->

``` r
#This creates box plots for each type of stat over the level of the base stat for each pokemon.
#Each stat has a similar median with attack being the highest slightly. Although, hp has more outliers and a higher max.

#In order to see this numerically, I will now provide summary stats for the base stat of each different stat name
statsum <- stat %>% select(basestat,statname) %>% 
  group_by(statname) %>%
  summarize("Min"=min(basestat),
            "Q1"=quantile(basestat,0.25,na.rm=TRUE),
            "Median"=quantile(basestat,0.25,na.rm=TRUE),
            "Mean"=mean(basestat),
            "Q3"=quantile(basestat,0.75,na.rm=TRUE),
            "Max"=max(basestat),
            "Standard Deviation"=sd(basestat))
#Statsum now has summary statistics for base stat for each level of stat name
#Now I will put this in a better looking table using the kable function
knitr::kable(statsum,caption="Summary Statistics for Base Stat by Stat Name",digits=2)
```

| statname        | Min |    Q1 | Median |  Mean |  Q3 | Max | Standard Deviation |
|:----------------|----:|------:|-------:|------:|----:|----:|-------------------:|
| attack          |   5 | 55.25 |  55.25 | 80.72 | 100 | 190 |              31.98 |
| defense         |   5 | 50.25 |  50.25 | 74.67 |  90 | 250 |              30.90 |
| hp              |   1 | 50.00 |  50.00 | 69.97 |  80 | 255 |              26.61 |
| special-attack  |  10 | 50.00 |  50.00 | 73.21 |  95 | 194 |              32.35 |
| special-defense |  20 | 50.00 |  50.00 | 72.61 |  90 | 250 |              27.73 |
| speed           |   5 | 45.00 |  45.00 | 69.67 |  90 | 200 |              30.04 |

Summary Statistics for Base Stat by Stat Name

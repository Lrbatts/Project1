Project1
================
Landon Batts
2023-06-21

This is a vignette to show how to retrieve data from an API. In order to
demonstrate, I will be retrieving data from a Pokemon API displaying
data of various topics about Pokemon throughout the years. I

``` r
library(httr)
library(jsonlite)
```

    ## Warning: package 'jsonlite' was built under R version 4.1.3

``` r
library(tidyverse)
```

    ## Warning: package 'ggplot2' was built under R version 4.1.3

    ## Warning: package 'tibble' was built under R version 4.1.3

    ## Warning: package 'dplyr' was built under R version 4.1.3

    ## Warning: package 'stringr' was built under R version 4.1.3

    ## Warning: package 'forcats' was built under R version 4.1.3

    ## Warning: package 'lubridate' was built under R version 4.1.3

    ## -- Attaching core tidyverse packages ------------------- tidyverse 2.0.0.9000 --
    ## v dplyr     1.1.2     v readr     2.1.1
    ## v forcats   1.0.0     v stringr   1.5.0
    ## v ggplot2   3.4.2     v tibble    3.2.1
    ## v lubridate 1.9.2     v tidyr     1.1.4
    ## v purrr     0.3.4     
    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter()  masks stats::filter()
    ## x purrr::flatten() masks jsonlite::flatten()
    ## x dplyr::lag()     masks stats::lag()
    ## i Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
pokemon <- GET("https://pokeapi.co/api/v2/move?limit=100000&offset=0")
str(pokemon,max.level=1)
```

    ## List of 10
    ##  $ url        : chr "https://pokeapi.co/api/v2/move?limit=100000&offset=0"
    ##  $ status_code: int 200
    ##  $ headers    :List of 25
    ##   ..- attr(*, "class")= chr [1:2] "insensitive" "list"
    ##  $ all_headers:List of 1
    ##  $ cookies    :'data.frame': 0 obs. of  7 variables:
    ##  $ content    : raw [1:61344] 7b 22 63 6f ...
    ##  $ date       : POSIXct[1:1], format: "2023-06-23 17:34:31"
    ##  $ times      : Named num [1:6] 0 0.0489 0.1049 0.2498 1.2046 ...
    ##   ..- attr(*, "names")= chr [1:6] "redirect" "namelookup" "connect" "pretransfer" ...
    ##  $ request    :List of 7
    ##   ..- attr(*, "class")= chr "request"
    ##  $ handle     :Class 'curl_handle' <externalptr> 
    ##  - attr(*, "class")= chr "response"

``` r
parsed <- fromJSON(rawToChar(pokemon$content))
str(parsed,max.level=1)
```

    ## List of 4
    ##  $ count   : int 918
    ##  $ next    : NULL
    ##  $ previous: NULL
    ##  $ results :'data.frame':    918 obs. of  2 variables:

``` r
results <- parsed$results
rawtocharacter <- function(x) {
  rawToChar(x$content)
}
movesdata <- lapply(lapply(lapply(results$url,GET),rawtocharacter),fromJSON)

movesdf <- results  %>% select(url) %>% add_column(movesdata) %>%
  unnest_wider(movesdata,names_repair='minimal')
 
pokemon2 <- GET("https://pokeapi.co/api/v2/pokemon?limit=100000&offset=0")
str(pokemon2)
```

    ## List of 10
    ##  $ url        : chr "https://pokeapi.co/api/v2/pokemon?limit=100000&offset=0"
    ##  $ status_code: int 200
    ##  $ headers    :List of 27
    ##   ..$ date                       : chr "Fri, 23 Jun 2023 17:45:57 GMT"
    ##   ..$ content-type               : chr "application/json; charset=utf-8"
    ##   ..$ content-length             : chr "11430"
    ##   ..$ connection                 : chr "keep-alive"
    ##   ..$ access-control-allow-origin: chr "*"
    ##   ..$ cache-control              : chr "public, max-age=86400, s-maxage=86400"
    ##   ..$ content-encoding           : chr "gzip"
    ##   ..$ etag                       : chr "W/\"157e5-4Uiu+gT8WF757lDu3GQyOjhCEUM\""
    ##   ..$ function-execution-id      : chr "ulbgubexwt59"
    ##   ..$ strict-transport-security  : chr "max-age=31556926"
    ##   ..$ x-cloud-trace-context      : chr "48b158a061ec8b59ed5baa93868b1e86;o=1"
    ##   ..$ x-country-code             : chr "CA"
    ##   ..$ x-orig-accept-language     : chr "pl,es;q=0.9,en-US;q=0.8,en;q=0.7,pl-PL;q=0.6"
    ##   ..$ x-powered-by               : chr "Express"
    ##   ..$ x-served-by                : chr "cache-yyz4579-YYZ"
    ##   ..$ x-cache                    : chr "MISS"
    ##   ..$ x-cache-hits               : chr "0"
    ##   ..$ x-timer                    : chr "S1681471688.628025,VS0,VE356"
    ##   ..$ vary                       : chr "Accept-Encoding,cookie,need-authorization, x-fh-requested-host, accept-encoding"
    ##   ..$ alt-svc                    : chr "h3=\":443\"; ma=86400"
    ##   ..$ cf-cache-status            : chr "HIT"
    ##   ..$ age                        : chr "15117"
    ##   ..$ accept-ranges              : chr "bytes"
    ##   ..$ report-to                  : chr "{\"endpoints\":[{\"url\":\"https:\\/\\/a.nel.cloudflare.com\\/report\\/v3?s=aZ23KFVJqWujMs5HYS8v%2BXDYz30Y%2BrS"| __truncated__
    ##   ..$ nel                        : chr "{\"success_fraction\":0,\"report_to\":\"cf-nel\",\"max_age\":604800}"
    ##   ..$ server                     : chr "cloudflare"
    ##   ..$ cf-ray                     : chr "7dbe8c33f92eb133-ATL"
    ##   ..- attr(*, "class")= chr [1:2] "insensitive" "list"
    ##  $ all_headers:List of 1
    ##   ..$ :List of 3
    ##   .. ..$ status : int 200
    ##   .. ..$ version: chr "HTTP/1.1"
    ##   .. ..$ headers:List of 27
    ##   .. .. ..$ date                       : chr "Fri, 23 Jun 2023 17:45:57 GMT"
    ##   .. .. ..$ content-type               : chr "application/json; charset=utf-8"
    ##   .. .. ..$ content-length             : chr "11430"
    ##   .. .. ..$ connection                 : chr "keep-alive"
    ##   .. .. ..$ access-control-allow-origin: chr "*"
    ##   .. .. ..$ cache-control              : chr "public, max-age=86400, s-maxage=86400"
    ##   .. .. ..$ content-encoding           : chr "gzip"
    ##   .. .. ..$ etag                       : chr "W/\"157e5-4Uiu+gT8WF757lDu3GQyOjhCEUM\""
    ##   .. .. ..$ function-execution-id      : chr "ulbgubexwt59"
    ##   .. .. ..$ strict-transport-security  : chr "max-age=31556926"
    ##   .. .. ..$ x-cloud-trace-context      : chr "48b158a061ec8b59ed5baa93868b1e86;o=1"
    ##   .. .. ..$ x-country-code             : chr "CA"
    ##   .. .. ..$ x-orig-accept-language     : chr "pl,es;q=0.9,en-US;q=0.8,en;q=0.7,pl-PL;q=0.6"
    ##   .. .. ..$ x-powered-by               : chr "Express"
    ##   .. .. ..$ x-served-by                : chr "cache-yyz4579-YYZ"
    ##   .. .. ..$ x-cache                    : chr "MISS"
    ##   .. .. ..$ x-cache-hits               : chr "0"
    ##   .. .. ..$ x-timer                    : chr "S1681471688.628025,VS0,VE356"
    ##   .. .. ..$ vary                       : chr "Accept-Encoding,cookie,need-authorization, x-fh-requested-host, accept-encoding"
    ##   .. .. ..$ alt-svc                    : chr "h3=\":443\"; ma=86400"
    ##   .. .. ..$ cf-cache-status            : chr "HIT"
    ##   .. .. ..$ age                        : chr "15117"
    ##   .. .. ..$ accept-ranges              : chr "bytes"
    ##   .. .. ..$ report-to                  : chr "{\"endpoints\":[{\"url\":\"https:\\/\\/a.nel.cloudflare.com\\/report\\/v3?s=aZ23KFVJqWujMs5HYS8v%2BXDYz30Y%2BrS"| __truncated__
    ##   .. .. ..$ nel                        : chr "{\"success_fraction\":0,\"report_to\":\"cf-nel\",\"max_age\":604800}"
    ##   .. .. ..$ server                     : chr "cloudflare"
    ##   .. .. ..$ cf-ray                     : chr "7dbe8c33f92eb133-ATL"
    ##   .. .. ..- attr(*, "class")= chr [1:2] "insensitive" "list"
    ##  $ cookies    :'data.frame': 0 obs. of  7 variables:
    ##   ..$ domain    : logi(0) 
    ##   ..$ flag      : logi(0) 
    ##   ..$ path      : logi(0) 
    ##   ..$ secure    : logi(0) 
    ##   ..$ expiration: 'POSIXct' num(0) 
    ##   ..$ name      : logi(0) 
    ##   ..$ value     : logi(0) 
    ##  $ content    : raw [1:88037] 7b 22 63 6f ...
    ##  $ date       : POSIXct[1:1], format: "2023-06-23 17:45:57"
    ##  $ times      : Named num [1:6] 0 0.000097 0.000097 0.000192 0.054091 ...
    ##   ..- attr(*, "names")= chr [1:6] "redirect" "namelookup" "connect" "pretransfer" ...
    ##  $ request    :List of 7
    ##   ..$ method    : chr "GET"
    ##   ..$ url       : chr "https://pokeapi.co/api/v2/pokemon?limit=100000&offset=0"
    ##   ..$ headers   : Named chr "application/json, text/xml, application/xml, */*"
    ##   .. ..- attr(*, "names")= chr "Accept"
    ##   ..$ fields    : NULL
    ##   ..$ options   :List of 2
    ##   .. ..$ useragent: chr "libcurl/7.84.0 r-curl/5.0.0 httr/1.4.6"
    ##   .. ..$ httpget  : logi TRUE
    ##   ..$ auth_token: NULL
    ##   ..$ output    : list()
    ##   .. ..- attr(*, "class")= chr [1:2] "write_memory" "write_function"
    ##   ..- attr(*, "class")= chr "request"
    ##  $ handle     :Class 'curl_handle' <externalptr> 
    ##  - attr(*, "class")= chr "response"

``` r
parsed2 <- fromJSON(rawToChar(pokemon2$content))
str(parsed2,max.level=1)
```

    ## List of 4
    ##  $ count   : int 1281
    ##  $ next    : NULL
    ##  $ previous: NULL
    ##  $ results :'data.frame':    1281 obs. of  2 variables:

``` r
pokeurl <- parsed2$results
pokedata <- lapply(lapply(lapply(pokeurl$url,GET),rawtocharacter),fromJSON)
pokedf <- pokeurl %>% select(url) %>% add_column(pokedata) %>%
  unnest_wider(pokedata,names_repair='minimal') %>% 
  unnest_longer(col=abilities) %>% select(name,abilities,base_experience,height,weight)

pokemonapi <- function(endpoint){
  pokemon2 <- GET("https://pokeapi.co/api/v2/pokemon?limit=100000&offset=0")
  parsed2 <- fromJSON(rawToChar(pokemon2$content))
  pokeurl <- parsed2$results
  pokedata <- lapply(lapply(lapply(pokeurl$url,GET),rawtocharacter),fromJSON)
  if(endpoint=="abilities"){
    pokedf <- pokeurl %>% select(url) %>% add_column(pokedata) %>%
  unnest_wider(pokedata,names_repair='minimal') %>% 
  unnest_longer(col=abilities) %>% select(name,abilities,base_experience,height,weight)
  }
  if(endpoint=="moves"){
    pokedf <- pokeurl %>% select(url) %>% add_column(pokedata) %>%
  unnest_wider(pokedata,names_repair='minimal') %>% 
  unnest_longer(col=moves) %>% select(name,moves,base_experience,height,weight)
  }
  if(endpoint=="forms"){
    pokedf <- pokeurl %>% select(url) %>% add_column(pokedata) %>%
  unnest_wider(pokedata,names_repair='minimal') %>% 
  unnest_longer(col=forms) %>% select(name,forms,base_experience,height,weight)
  }
  if(endpoint=="stats"){
    pokedf <- pokeurl %>% select(url) %>% add_column(pokedata) %>%
  unnest_wider(pokedata,names_repair='minimal') %>% 
  unnest_longer(col=stats) %>% select(name,stats,base_experience,height,weight)
  }
  if(endpoint=="types"){
    pokedf <- pokeurl %>% select(url) %>% add_column(pokedata) %>%
  unnest_wider(pokedata,names_repair='minimal')  %>%
  unnest_longer(col=types) %>%
  select(name,types,base_experience,height,weight)
  }
  pokedf$types[,2]
  if(endpoint=="game"){
    pokedf <- pokeurl %>% select(url) %>% add_column(pokedata) %>%
  unnest_wider(pokedata,names_repair='minimal') %>% 
  unnest_longer(col=game_indices) %>% select(name,game_indices,base_experience,height,weight)
  }
  return(pokedf)
}
abilities <- pokemonapi(endpoint="abilities")
```

    ## Warning: Unknown or uninitialised column: `types`.

``` r
moves <- pokemonapi(endpoint="moves")
```

    ## Warning: Unknown or uninitialised column: `types`.

``` r
forms <- pokemonapi(endpoint="forms")
```

    ## Warning: Unknown or uninitialised column: `types`.

``` r
stat <- pokemonapi(endpoint="stats")
```

    ## Warning: Unknown or uninitialised column: `types`.

``` r
types <- pokemonapi(endpoint="types")
game <- pokemonapi(endpoint="game")
```

    ## Warning: Unknown or uninitialised column: `types`.

``` r
table(pokedf$types[,2])
```

    ## Warning: Unknown or uninitialised column: `types`.

    ## < table of extent 0 >

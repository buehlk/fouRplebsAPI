
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fouRplebsAPI

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-stable.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![R-CMD-check](https://github.com/buehlk/fouRplebsAPI/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/buehlk/fouRplebsAPI/actions/workflows/R-CMD-check.yaml)

[![DOI](https://zenodo.org/badge/499033736.svg)](https://zenodo.org/badge/latestdoi/499033736)

<!-- badges: end -->

The R package fouRplebsAPI enables researchers to query the 4chan
database archived by [4plebs.org](https://www.4plebs.org/). This
database is the largest ongoing archive of the ever-disappearing posts
on the imageboard 4chan. With this package researchers can use the
detailed search functionalities offered by 4plebs and retrieve
structured data of the communication on 4chan.

The package is based on [4plebs API
documentation](https://4plebs.tech/foolfuuka/).

If fouRplebsAPI is helpful for your research, please cite as:

Buehling, K. (2022). fouRplebsAPI: R package for accessing 4chan posts
via the 4plebs.org API (Version 0.9.0).
<https://doi.org/10.5281/zenodo.6637440>

## Installation

You can install the fouRplebsAPI from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("buehlk/fouRplebsAPI")
```

The 4chan boards currently covered by 4plebs are:

| Board.name            | Abbreviation |
|:----------------------|:-------------|
| Politically Incorrect | pol          |
| High Resolution       | hr           |
| Traditional Games     | tg           |
| Television & Film     | tv           |
| Paranormal            | x            |
| Sh\*t 4chan Says      | s4s          |
| Auto                  | o            |
| Advice                | adv          |
| Travel                | trv          |
| Flash                 | f            |
| Sports                | sp           |
| My Little Politics    | mlpol        |
| Mecha & Auto          | mo           |

## Search the 4chan archive

While this package includes several funtions that allow researchers to
query and inspect specific 4chan posts (get_4chan_post) or threads
(get_4chan_thread), researchers wanting to collect data from the 4plebs
archive will probably be interested in collecting a larger amount of
data.

The *first* way of collecting data is by collecting the latest threads
in a given board. Let’s say you are interested in the 20 latest threads
from the “Advice” board (excluding the comments accompanying the opening
post), one way of querying the data is:

``` r
library(fouRplebsAPI)

recentAdv <- get_4chan_board_range(board = "adv", page_start = 1, page_stop = 2, latest_comments = FALSE)

str(recentAdv, vec.len = 1, nchar.max = 60)
#> 'data.frame':    20 obs. of  15 variables:
#>  $ thread_id          : chr  "26681932" ...
#>  $ doc_id             : chr  "12984602" ...
#>  $ num                : chr  "26681932" ...
#>  $ subnum             : chr  "0" ...
#>  $ op                 : num  1 1 ...
#>  $ timestamp          : int  1655110365 1655109856 ...
#>  $ fourchan_date      : chr  "6/13/22(Mon)4:52" ...
#>  $ name               : chr  "Anonymous" ...
#>  $ title              : logi  NA ...
#>  $ referencing_comment: logi  NA ...
#>  $ comments           : chr  ">get a bouquet of baptisia flowers from bf\"| __truncated__ ...
#>  $ poster_country     : logi  NA ...
#>  $ nreplies           : logi  NA ...
#>  $ formatted          : logi  FALSE ...
#>  $ media_link         : logi  NA ...
```

The output description can be found in the function documentations.
Theoretically it would be possible to scrape vast ranges of the archive
with this function, even though the API has an API rate limit, which
slows down the querying process.

A *second* way collecting 4chan data with this package is the search
function. 4plebs allows for a very detailed search with many search
filter. I will show only simple examples of the data that can be
collected with fouRplebsAPI.

The example I show here is rather cheerful, because I would like to
avoid the more controversial topics for which 4chan, especially the
/pol/ board is notorious. Researchers, for instance the ones interested
in the political communication of actors with contentious ideologies,
will find it easy to adapt this example. But this one is about vacation.

Let’s find the communication in the “Travel” board that discusses
Mallorca, Spain.

First, to get a first impression of the search results, one can inspect
a snippet of the 25 most recent posts containing the search term
“mallorca”.

``` r
mallorca_snippet <- search_4chan_snippet(boards = "trv", start_date = "2021-01-01", end_date = "2022-12-31", text = "mallorca")
#> The 1 - 25 oldest posts of the 78 total search results are shown.
#> Scraping all 78 results would take ~ 1.33 minutes.

str(mallorca_snippet, vec.len = 1, nchar.max = 60)
#> 'data.frame':    25 obs. of  15 variables:
#>  $ thread_id          : chr  "1938850" ...
#>  $ doc_id             : chr  "1113628" ...
#>  $ num                : chr  "1938924" ...
#>  $ subnum             : chr  "0" ...
#>  $ op                 : num  0 1 ...
#>  $ timestamp          : int  1610611412 1611403974 ...
#>  $ fourchan_date      : chr  "1/14/21(Thu)3:03" ...
#>  $ name               : chr  "Anonymous" ...
#>  $ title              : chr  NA ...
#>  $ referencing_comment: chr  "1938909\n" ...
#>  $ comments           : chr  ">got murdered and/or raped in shitholes ove"| __truncated__ ...
#>  $ poster_country     : logi  NA ...
#>  $ nreplies           : int  NA 13 ...
#>  $ formatted          : logi  FALSE ...
#>  $ media_link         : chr  NA ...
```

Note that the function search_4chan_snippet() also prints the total
number of search results and the estimated time to retrieve them with
search_4chan(). This estimation is based on 5 requests per minute API
limit.

Users only interested in the number of results can just retrieve them by
changing the parameter result_type to “results_num”. Now one can compare
the number of posts mentioning Mallorca between different time periods.
For example, pre-pandemic vs. post-pandemic:

``` r
mallorca_pre <- search_4chan_snippet(boards = "trv", start_date = "2018-01-01", end_date = "2019-12-31", text = "mallorca", result_type = "results_num")
mallorca_post <- search_4chan_snippet(boards = "trv", start_date = "2020-01-01", end_date = "2021-12-31", text = "mallorca", result_type = "results_num")

data.frame("Years" = c("2018 & 2019", "2020 & 2021"),
       "Total results" = c(mallorca_pre["total_found"], mallorca_post["total_found"])
       )
#>         Years Total.results
#> 1 2018 & 2019            86
#> 2 2020 & 2021            99
```

It seems that this island has been mentioned more as people tended to
stay home.

Researchers interested in gathering more data than just a snippet of the
posts can use the function search_4chan(). Staying with the example of
posts mentioning Mallorca over time, one could be inclined to ask,
whether the image of Mallorca has changed during the pandemic. Apart
from simply getting *all* the posts mentioning a search term, it is for
example possible to filter for posts containing image data:

``` r
mallorca_pre_pics <- search_4chan(boards = "trv", start_date = "2018-01-01", end_date = "2019-12-31", text = "mallorca", show_only = "image")
#> [1] "Approximate time: 0.33 minutes."
mallorca_post_pics <- search_4chan_snippet(boards = "trv", start_date = "2018-01-01", end_date = "2019-12-31", text = "mallorca", show_only = "image")
#> The 1 - 16 oldest posts of the 16 total search results are shown.
#> Scraping all 16 results would take ~ 0.33 minutes.

head(mallorca_post_pics$media_link)
#> [1] "http://i.4pcdn.org/trv/1521713616876.jpg"
#> [2] "http://i.4pcdn.org/trv/1525249686528.jpg"
#> [3] "http://i.4pcdn.org/trv/1527534752103.jpg"
#> [4] "http://i.4pcdn.org/trv/1527865867839.jpg"
#> [5] "http://i.4pcdn.org/trv/1533082869505.jpg"
#> [6] "http://i.4pcdn.org/trv/1547062117808.jpg"
```

The column media_link provides the downloadable image links of the posts
retrieved.

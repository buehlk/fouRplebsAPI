
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fouRplebsAPI

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The R package fouRplebsAPI enables researchers to query the 4chan
database archived by [4plebs.org](https://www.4plebs.org/). This
database is the largest ongoing archive of the ever-disappearing posts
on the imageboard 4chan. With this package researchers can use the
detailed search functionalities offered by 4plebs and retrieve
structured data of the communication on 4chan.

The package is based on [4plebs API
documentation](https://4plebs.tech/foolfuuka/).

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

## Installation

You can install the fouRplebsAPI from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("buehlk/fouRplebsAPI")
```

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
#>  $ thread_id          : chr  "26598584" ...
#>  $ doc_id             : chr  "12901275" ...
#>  $ num                : chr  "26598584" ...
#>  $ subnum             : chr  "0" ...
#>  $ op                 : num  1 1 ...
#>  $ timestamp          : int  1654182594 1654181906 ...
#>  $ fourchan_date      : chr  "6/2/22(Thu)11:09" ...
#>  $ name               : chr  "Anonymous" ...
#>  $ title              : logi  NA ...
#>  $ referencing_comment: logi  NA ...
#>  $ comments           : chr  "My moms a slob, and my sister isnt much wor"| __truncated__ ...
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
#> The 1 - 25 oldest threads of the 77 total search results are shown.
#> Scraping all 77 results would take ~ 1.33 minutes.

str(recentAdv, vec.len = 1, nchar.max = 60)
#> 'data.frame':    20 obs. of  15 variables:
#>  $ thread_id          : chr  "26598584" ...
#>  $ doc_id             : chr  "12901275" ...
#>  $ num                : chr  "26598584" ...
#>  $ subnum             : chr  "0" ...
#>  $ op                 : num  1 1 ...
#>  $ timestamp          : int  1654182594 1654181906 ...
#>  $ fourchan_date      : chr  "6/2/22(Thu)11:09" ...
#>  $ name               : chr  "Anonymous" ...
#>  $ title              : logi  NA ...
#>  $ referencing_comment: logi  NA ...
#>  $ comments           : chr  "My moms a slob, and my sister isnt much wor"| __truncated__ ...
#>  $ poster_country     : logi  NA ...
#>  $ nreplies           : logi  NA ...
#>  $ formatted          : logi  FALSE ...
#>  $ media_link         : logi  NA ...
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

str(recentAdv, vec.len = 1, nchar.max = 60)
#> 'data.frame':    20 obs. of  15 variables:
#>  $ thread_id          : chr  "26598584" ...
#>  $ doc_id             : chr  "12901275" ...
#>  $ num                : chr  "26598584" ...
#>  $ subnum             : chr  "0" ...
#>  $ op                 : num  1 1 ...
#>  $ timestamp          : int  1654182594 1654181906 ...
#>  $ fourchan_date      : chr  "6/2/22(Thu)11:09" ...
#>  $ name               : chr  "Anonymous" ...
#>  $ title              : logi  NA ...
#>  $ referencing_comment: logi  NA ...
#>  $ comments           : chr  "My moms a slob, and my sister isnt much wor"| __truncated__ ...
#>  $ poster_country     : logi  NA ...
#>  $ nreplies           : logi  NA ...
#>  $ formatted          : logi  FALSE ...
#>  $ media_link         : logi  NA ...
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
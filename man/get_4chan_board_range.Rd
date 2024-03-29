% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_4chan_board_range.R
\name{get_4chan_board_range}
\alias{get_4chan_board_range}
\title{Return threads for a range of 4chan board indices}
\usage{
get_4chan_board_range(
  board,
  page_start,
  page_stop,
  latest_comments = TRUE,
  cool = 5
)
}
\arguments{
\item{board}{Character variable of the 4chan board.\cr
Available boards are: "adv", "plebs", "hr", "tg", "tv", "x", "s4s", "pol",
"o", "trv", "f", "sp", "mlpol", "mo".}

\item{page_start}{Integer, Start of page range}

\item{page_stop}{Integer, End of page range}

\item{latest_comments}{Boolean, TRUE: Return opening posts and all replies,
FALSE: Return only opening posts, Default: TRUE}

\item{cool}{Integer (seconds), The 4plebs API includes an undocumented API
rate limit, a cool-down period is recommended. , Default: 5}
}
\value{
Dataframe with details on all posts on a given board page.
}
\description{
This function returns the threads from range of board page
indices, starting from the latest thread
}
\details{
Variables in API output:\cr\cr
thread_id: 4chan ID of the thread the post is situated in\cr
doc_id: 4chan document ID\cr
num: 4chan post ID\cr
subnum: Binary, 1: Ghost Post, 0: Non-Ghost Post\cr
op: Binary, 1: Opening Post, 0: Reply Post\cr
timestamp: Time sent in Posix time\cr
fourchan_date: Time sent\cr
name: Author name\cr
title: Post title\cr
referencing_comment: The post_id this post is quoting\cr
comments: Text of the post\cr
poster_country: Author country\cr
nreplies: Number of replies\cr
formatted: Boolean, Has this post been formatted?\cr
media_link: Download link to the media (e.g. images) that have been shared
in the post
}
\examples{
\dontrun{
get_4chan_board_range(board = "o", page_start = 1, page_stop = 2)

get_4chan_board_range(board = "o", page_start = 1, page_stop = 2,
latest_comments = TRUE)
}
}

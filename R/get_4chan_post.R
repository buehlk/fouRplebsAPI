#' @title Return specific 4chan post
#' @description Return 4chan post by looking up its ID
#' @param board Character variable of the 4chan board.\cr
#' Available boards are: "adv", "plebs", "hr", "tg", "tv", "x", "s4s", "pol",
#' "o", "trv", "f", "sp", "mlpol", "mo".
#' @param post_id Integer value of the post ID.
#' @return Dataframe with post details.
#' @details Variables in API output:\cr\cr
#' thread_id: 4chan ID of the thread the post is situated in\cr
#' doc_id: 4chan document ID\cr
#' num: 4chan post ID\cr
#' subnum: Binary, 1: Ghost Post, 0: Non-Ghost Post\cr
#' op: Binary, 1: Opening Post, 0: Reply Post\cr
#' timestamp: Time sent in Posix time\cr
#' fourchan_date: Time sent\cr
#' name: Author name\cr
#' title: Post title\cr
#' referencing_comment: The post_id this post is quoting\cr
#' comments: Text of the post\cr
#' poster_country: Author country\cr
#' nreplies: Number of replies
#' formatted: Boolean, Has this post been formatted?\cr
#' media_link: Download link to the media (e.g. images) that have been shared
#' in the post
#'
#' @examples
#' \dontrun{
#' get_4chan_post(board = "trv", post = 2226503)
#'
#'  get_4chan_post(board = "adv", post = 24299103)
#' }
#' @seealso
#'  \code{\link[httr]{modify_url}}, \code{\link[httr]{user_agent}},
#'  \code{\link[httr]{GET}}, \code{\link[httr]{http_type}},
#'  \code{\link[httr]{content}}, \code{\link[httr]{http_error}}
#'  \code{\link[jsonlite]{toJSON, fromJSON}}
#'  \code{\link[stringr]{str_extract}}
#' @rdname get_4chan_post
#' @export
#' @importFrom httr modify_url user_agent GET http_type content http_error
#' @importFrom jsonlite fromJSON
#' @importFrom stringr str_extract

get_4chan_post <- function(board, post_id) {
  match.arg(board, c("adv", "plebs", "hr", "tg", "tv", "x", "s4s", "pol", "o",
                     "trv", "f", "sp", "mlpol", "mo"))

  path <- sprintf("_/api/chan/post/?board=%s&num=%i", board, post_id)
  url <- httr::modify_url("http://archive.4plebs.org/", path = path)
  ua <- httr::user_agent("4Rplebs API")

  resp <- httr::GET(url, ua)
  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"),
                               simplifyVector = FALSE)

  if (httr::http_error(resp)|is.null(parsed[["error"]]) == FALSE) {
    statuscode <- httr:status_code(resp)
    stop(
      sprintf(
        "4plebs.org API request failed [%s]\n%s",
        statuscode,
        parsed$error
      ),
      call. = FALSE
    )
  }

  data.frame("thread_id" = parsed$thread_num,
             "doc_id" = parsed$doc_id,
             "num" = parsed$num,
             "subnum" = parsed$subnum,
             "op" = as.numeric(parsed$op),
             "timestamp" = parsed$timestamp,
             "fourchan_date" = parsed$fourchan_date,
             "name" = parsed$name,
             "title" = ifelse(is.null(parsed$title), NA, parsed$title),
             "referencing_comment" = gsub(">>| ",
                                          "",
                                          stringr::str_extract(
                                            parsed$comment,
                                            ">>[0-9]*( |\\n)"
                                            )
                                          ),
             "comments" = gsub("@>>[0-9]*( |\\n)", "", parsed$comment),
             "poster_country" = ifelse(
               is.null(parsed$poster_country),
               NA,
               parsed$poster_country
               ),
             "nreplies" = ifelse(
               is.null(parsed$nreplies),
               NA,
               parsed$nreplies),
             "formatted" = parsed$formatted,
             "media_link" = ifelse(
               is.null(parsed$media),
               NA,
               parsed$media$media_link)
  )
}

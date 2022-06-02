#' @title Return threads for a range of 4chan board indices
#' @description This function returns the threads from range of board page indices, starting from the latest thread
#' @param board Character variable of the 4chan board.\cr
#' Available boards are: "adv", "plebs", "hr", "tg", "tv", "x", "s4s", "pol", "o", "trv", "f", "sp", "mlpol", "mo".
#' @param page_start Integer, Start of page range
#' @param page_stop Integer, End of page range
#' @param latest_comments Boolean, TRUE: Return opening posts and all replies, FALSE: Return only opening posts, Default: TRUE
#' @param cool Integer (seconds), The 4plebs API includes an undocumented API rate limit, a cool-down period is recommended. , Default: 5
#' @return Dataframe with details on all posts on a given board page.
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
#' nreplies: Number of replies\cr
#' formatted: Boolean, Has this post been formatted?\cr
#' media_link: Download link to the media (e.g. images) that have been shared in the post
#' @examples
#' \dontrun{
#' get_4chan_board_range(board = "o", page_start = 1, page_stop = 2)
#'
#' get_4chan_board_range(board = "0", page_start = 1, page_stop = 2, last_comments = TRUE)
#' }
#' @rdname get_4chan_board_range
#' @export

get_4chan_board_range <- function(board, page_start, page_stop, latest_comments = TRUE, cool = 5){
  range <- page_start:page_stop
  do.call("rbind", lapply(range, function(i){get_4chan_board_index(board = board, page = i, latest_comments = latest_comments, cool = cool)}))
}

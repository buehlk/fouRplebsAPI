#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param board Character variable of the 4chan board.\cr
#' Available boards are: "adv", "plebs", "hr", "tg", "tv", "x", "s4s", "pol", "o", "trv", "f", "sp", "mlpol", "mo".
#' @param page_start PARAM_DESCRIPTION
#' @param page_stop PARAM_DESCRIPTION, Default: T
#' @param latest_comments PARAM_DESCRIPTION, Default: T
#' @param cool PARAM_DESCRIPTION, Default: 5
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname get_4chan_board_range
#' @export

get_4chan_board_range <- function(board, page_start, page_stop = T, latest_comments = T, cool = 5){
  range <- page_start:page_stop
  do.call("rbind", lapply(range, function(i){get_4chan_board_index(board = board, page = i, latest_comments = latest_comments, cool = cool)}))
}

#' Reorder a Data Frame
#'
#' @param df A data frame to be reordered.
#' @param x A character scalar specifying the column to be reordered.
#' @param y A character scalar specifying the column to order by if ordering by values.
#' @param group A character scalar specifying the grouping column (optional).
#' @param order A character scalar specifying the order type (one of "none", "y", "grouped"). See details.
#' @param dir_order A logical scalar specifying whether to flip the order.
#'
#' @details Ordering takes the following possible values:
#'
#' * "none": No reordering.
#' * "y": Order by values of y.
#' * "grouped_y": Order by values of y and group.
#' * "x": Order alphabetically by x.
#' * "grouped_x": Order alphabetically by x and group.
#'
#'
#' @return The reordered data frame.
#'
#' @examples
#' # Example usage
#' df <- data.frame(col1 = c("b", "a", "c"), col2 = c(10, 25, 3))
#' reorder_by(df, "col1", "col2")
#'
#' @export
reorder_by <- function(df, x, y, group = "", order = "y", dir_order = 1) {
  #------ Checks

  # df is a data frame
  checkmate::assert_data_frame(df)

  # x and y are character scalar and in df
  checkmate::assert_character(x, len = 1)
  checkmate::assert_character(y, len = 1)
  checkmate::assert_subset(x, colnames(df))
  checkmate::assert_subset(y, colnames(df))

  # group is character scalar and in df if not empty
  checkmate::assert_character(group, len = 1)
  if (group != "") {
    checkmate::assert_subset(group, colnames(df))
  }

  # order is a character scalar in c("none", "y", "grouped")
  checkmate::assert_choice(order, c("none", "y", "grouped_y", "x", "grouped_x"))

  # dir_order is 1 or -1 (numeric scalar)
  checkmate::assert_subset(dir_order, c(1, -1))

  # Convert dir_order to decreasing logical flag
  dir_order_lgl <- (dir_order == -1)

  #------ Reorder

  # droplevels first
  if (is.factor(df[[x]])) {
    df[[x]] <- droplevels(df[[x]])
  }

  # reording options
  if (order == "y") {
    # Order by values of y
    df <- df[order(df[[y]], decreasing = dir_order_lgl), ]
    df[[x]] <- forcats::fct_inorder(df[[x]])
  } else if (order == "grouped_y" && group != "") {
    # Order by group first, then by values of y
    df <- df[
      order(df[[group]], df[[y]], decreasing = c(FALSE, dir_order_lgl)),
    ]
    df[[x]] <- forcats::fct_inorder(df[[x]])
  } else if (order == "grouped_y" && group == "") {
    # Fallback to ordering by y if group is empty
    rlang::warn("Group is empty. Ordering by y only.")
    df <- df[order(df[[y]], decreasing = dir_order_lgl), ]
    df[[x]] <- forcats::fct_inorder(df[[x]])
  } else if (order == "x") {
    # Order alphabetically by x
    df <- df[order(df[[x]], decreasing = dir_order_lgl), ]
    df[[x]] <- forcats::fct_inorder(df[[x]])
  } else if (order == "grouped_x" && group != "") {
    # Order by group first, then alphabetically by x
    df <- df[
      order(df[[group]], df[[x]], decreasing = c(FALSE, dir_order_lgl)),
    ]
    df[[x]] <- forcats::fct_inorder(df[[x]])
  } else if (order == "grouped_x" && group == "") {
    # Fallback to ordering by x if group is empty
    rlang::warn("Group is empty. Ordering by x only.")
    df <- df[order(df[[x]], decreasing = dir_order_lgl), ]
    df[[x]] <- forcats::fct_inorder(df[[x]])
  }

  # Reset row names
  rownames(df) <- NULL

  return(df)
}

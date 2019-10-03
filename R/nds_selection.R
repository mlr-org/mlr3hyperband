#' @title Best points w.r.t. non dominated sorting with hypervolume contrib.
#'
#' @description Select best subset of points by non dominated sorting with
#' hypervolume contribution for tie breaking. Works on an arbitrary dimension
#' of size two or higher.
#' @param points Matrix with each column corresponding to a point
#' @param n_select Amount of points to select (integer(1L))
#' @param ref_point Reference point for hypervolume (integer())
#' @param minimize Should the ranking be based on minimization? (Single bool
#' for all dimensions, or vector of bools corresponding to the dimensions)
#' @return Vector of indeces of selected points
#' 
#' @examples
#' set.seed(123)
#'
#' points = rbind(
#'   y1 = runif(10),
#'   y2 = runif(10)
#' )
#'
#' nds_selection(points, 7, c(10, 10))

nds_selection = function(points, n_select, ref_point = NULL, minimize = TRUE) {

  # maximize/minimize preprocessing: switch sign in each dim to maximize
  points = points * (minimize * 2 - 1)

  # init output indeces
  survivors = 1:ncol(points)

  # if no reference point is defined, use maximum of each dimensions
  if (is.null(ref_point)) {

    minmax    = if (minimize) max else min
    ref_point = apply(points, 1, minmax)
  }

  # front indeces of every point
  front_ranks = emoa::nds_rank(points)
  # the index of the highest front in the end selection
  last_sel_front = min(which(cumsum(table(front_ranks)) >= n_select))

  # non-tied indeces by nds rank
  sel_surv = survivors[front_ranks < last_sel_front]
  # tied subselection of indeces/points
  tie_surv = survivors[front_ranks == last_sel_front]
  tie_points = points[, front_ranks == last_sel_front]

  # remove tied indeces/points as long as we are bigger than n_select
  while (length(c(tie_surv, sel_surv)) > n_select) {

    # tie points extended with the reference point to never end up with a two
    # point matrix (this would break the following sapply)
    tie_points_ext = cbind(tie_points, ref_point)

    # calculate the hypervolume with each point excluded separately
    hypervolumes = sapply(
      seq_len(ncol(tie_points_ext) - 1),
      function(i) {
        emoa::dominated_hypervolume(tie_points_ext[, -i], ref = ref_point)
      }
    )

    # index of the tied case with the lowest hypervolume contribution
    to_remove = which.max(hypervolumes)
    tie_points = tie_points[, -to_remove]
    tie_surv = tie_surv[-to_remove]
  }

  # since we only have the true ranks of the ties, we sort to make the output
  # not misleading
  survivors = sort(c(sel_surv, tie_surv))

  return(survivors)
}




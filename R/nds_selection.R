#' @title Best points w.r.t. non dominated sorting with hypervolume contrib.
#'
#' @description Select best subset of points by non dominated sorting with
#' hypervolume contribution for tie breaking. Works on an arbitrary dimension
#' of size two or higher.
#' @section Parameters:
#' * `points` :: `matrix()`\cr
#'   Numeric matrix with each column corresponding to a point
#' * `n_select` :: `integer(1L)`\cr
#'   Amount of points to select
#' * `ref_point` :: `integer()`\cr
#'   Reference point for hypervolume
#' * `minimize` :: `logical()`\cr
#'   Should the ranking be based on minimization? (Single bool
#'   for all dimensions, or vector of bools with each entry corresponding to
#' each dimension)
#' * `tie_breaker` :: `character()` \cr
#'    Tie-breaking when selecting survivors according to nondominated sorint. 
#'    Defaults to "s-metric" selection. Other possibilities are "crowding-distance", and `NULL` (front survives completely).  
#' @return Vector of indices of selected points
#' @usage NULL

nds_selection = function(points, n_select, ref_point = NULL, minimize = TRUE, tie_breaker = "s-metric") {

  mlr3misc::require_namespaces("emoa")

  # check input for correctness
  assert_matrix(points, mode = "numeric")
  assert_int(n_select, lower = 1, upper = ncol(points))
  assert_logical(
    minimize, min.len = 1, max.len = nrow(points), any.missing = FALSE
  )
  assert_numeric(ref_point, len = nrow(points), null.ok = TRUE)
  assert_logical(minimize)
  assert_character()

  # maximize/minimize preprocessing: switch sign in each dim to maximize
  points = points * (minimize * 2 - 1)

  # init output indices
  survivors = seq_col(points)

  # if no reference point is defined, use maximum of each dimensions
  if (is.null(ref_point)) {
    ref_point = apply(points, 1, max)
  }

  # front indices of every point
  front_ranks = emoa::nds_rank(points)
  # the index of the highest front in the end selection
  last_sel_front = min(which(cumsum(table(front_ranks)) >= n_select))

  # this option still needs to be documented! 
  if (is.null(tie_breaker)) {
    sel_surv = survivors[front_ranks <= last_sel_front]
  } else {
    # non-tied indices by nds rank
    sel_surv = survivors[front_ranks < last_sel_front]

    # tied subselection of indices/points
    tie_surv = survivors[front_ranks == last_sel_front]
    tie_points = points[, front_ranks == last_sel_front, drop = FALSE]

    # remove tied indices/points as long as we are bigger than n_select
    while (length(tie_surv) + length(sel_surv) > n_select) {

      # tie points extended with the reference point to never end up with a two
      # point matrix (this would break the following sapply)
      tie_points_ext = cbind(tie_points, ref_point)

      switch(tie_breaker, 
        "s-metric" = {
        # calculate the hypervolume with each point excluded separately
          hypervolumes = mlr3misc::map_dbl(
            seq_len(ncol(tie_points_ext) - 1L),
            function(i) {
              emoa::dominated_hypervolume(
                tie_points_ext[, -i, drop = FALSE],
                ref = ref_point
              )
            }
          )
          # index of the tied case with the lowest hypervolume contribution
          to_remove = which(hypervolumes == max(hypervolumes))
        }, 
        "crowding-distance" = {
          crowding = emoa::crowding_distance(tie_points)
          # remove the one with the lowest crowding distance
          to_remove = which(crowding == min(crowding))
        }
      )

      tie_points = tie_points[, -to_remove, drop = FALSE]
      tie_surv = tie_surv[-to_remove]
  }

  # since we only have the true ranks of the ties, we sort to make the output
  # not misleading
  sel_surv = sort(c(sel_surv, tie_surv))
  }

  return(sel_surv)
}

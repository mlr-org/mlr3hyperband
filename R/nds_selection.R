#' @title Best points w.r.t. non dominated sorting with hypervolume contrib.
#'
#' @description Select best subset of points by non dominated sorting with
#' hypervolume contribution for tie breaking. Works on an arbitrary dimension
#' of size two or higher. Returns a vector of indices of selected points.
#'
#' @section Parameters:
#' \describe{
#' \item{`points`}{`matrix()`\cr
#' Numeric matrix with each column corresponding to a point.}
#' \item{`n_select`}{`integer(1)`\cr
#' Amount of points to select.}
#' \item{`ref_point`}{`integer()`\cr
#' Reference point for hypervolume.}
#' \item{`minimize`}{`logical()`\cr
#' Should the ranking be based on minimization? (Single bool for all dimensions,
#' or vector of bools with each entry corresponding to each dimension).}
#' }
#'
#' @return `integer()`
#' @usage NULL
select_survivors = function(points, n_select, ref_point = NULL, minimize = TRUE,
  method = "dominance_based", tie_breaker = "HV", archive = NULL) {

  require_namespaces("emoa")

  # check input for correctness
  assert_data_frame(points)
  assert_int(n_select, lower = 1, upper = nrow(points))
  assert_logical(
    minimize, min.len = 1, max.len = ncol(points), any.missing = FALSE
  )
  assert_numeric(ref_point, len = ncol(points), null.ok = TRUE)
  assert_logical(minimize)

  assert_choice(method, choices = c("dominance_based", "indicator_based"))

  if (method == "dominance_based")
    assert_choice(tie_breaker, choices = c("CD", "HV"))

  if (method == "indicator_based")
    assert_data_frame(archive, ncols = ncol(points), null.ok = TRUE)

  # maximize/minimize preprocessing: switch sign in each dim to maximize (since emoa maximizes)
  points = mapply(`*`, points, c(minimize * 2 - 1))

  # also switch sign for the reference point if reference point is given
  # otherwise use the nadir point
  if (!is.null(ref_point)) {
    ref_point = ref_point * (minimize * 2 - 1)
  } else {
    ref_point = apply(points, 2, max)
  }

  # init output indices
  survivors = seq_row(points)

  if (method == "dominance_based") {

    front_ranks = eaf::pareto_rank(points, maximise = FALSE)

    breaked_front = min(which(cumsum(table(front_ranks)) > n_select))
    sel_surv = survivors[front_ranks < breaked_front]

    if (length(sel_surv) < n_select) {

      tie_idx = which(front_ranks == breaked_front)
      tie_points = points[tie_idx, ]

      if (tie_breaker == "CD") {
        # choose additional candidates based on crowding distance
        tie_surv = head(order(emoa::crowding_distance(t(as.matrix(tie_points))), decreasing = TRUE), n_select - length(sel_surv))
        sel_surv = c(sel_surv, tie_idx[tie_surv])
      }

      if (tie_breaker == "HV") {

        while(length(sel_surv) + length(tie_idx) > n_select) {

          # I am not trusting emoa here; empirically it brought strange results
          hv_contrib = eaf::hv_contributions(data = t(tie_points), ref = ref_point, maximise = FALSE)

          # index of the tied case with the lowest hypervolume contribution
          to_remove = which(hv_contrib == min(hv_contrib))

          # if two points have the exact same hypervolume contribution, the point is sampled
          if (length(to_remove) > 1)
            to_remove = sample(to_remove, 1)

          tie_points = tie_points[, - to_remove, drop = FALSE]
          tie_idx = tie_idx[- to_remove]
        }

        sel_surv = c(sel_surv, tie_idx)
      }
    }
  }

  if (method == "indicator_based") {

    # for every proposed point, get the hypervolume contribution
    hvc = unlist(lapply(seq_row(points), function(i) {

      to_eval = points[i, , drop = FALSE]

      arch_size = nrow(archive)

      # combine archive with the point and compute the hypervolume contribution of this point
      if (!is.null(arch_size) && arch_size > 0) {
        to_eval = rbind(to_eval, archive)
      }

      #eaf::hv_contributions(to_eval, reference = ref_point, maximise = FALSE)[1]
      emoa::dominated_hypervolume(t(to_eval), ref_point)
    }))

    sel_surv = order(hvc, decreasing = TRUE)[seq_len(n_select)]
  }

  return(sel_surv)
}


# points = rbind(
#   y1 = runif(10),
#   y2 = runif(10)
# )
# #?emoa::nds_hv_selection(points, 1)
# emoa::nds_rank(points)
# #emoa::dominated_hypervolume(points)
# ecr::selNondom(points, n.select = 9)

nds_hv_rank = function(points, n_select) {

  hv = function(points, index) {
    # difference between hv of all points vs hv of all points except index
    emoa::dominated_hypervolume(points) -
      emoa::dominated_hypervolume(points[, -index])
  }

  # front indeces of every point
  tied_ranks = emoa::nds_rank(points)
  # the index of the highest front in the end selection
  last_sel_front = min(which(cumsum(table(tied_ranks)) >= n.select))
  # order of the elements in the last selected front
  rank_last_sel_front = apply(points, 1, function(x) {})
}

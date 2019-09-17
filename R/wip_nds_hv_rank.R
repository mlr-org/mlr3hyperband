set.seed(123)

points = rbind(
  y1 = runif(10),
  y2 = runif(10)
)
emoa::nds_hv_selection(points)
emoa::nds_rank(points)
#emoa::dominated_hypervolume(points)

nds_hv_rank = function(points, n_select) {

  get_hv = function(points, index) {
    # difference between hv of all points vs hv of all points except index
    emoa::dominated_hypervolume(points) -
      emoa::dominated_hypervolume(points[, -index])
  }

  # front indeces of every point
  front_ranks = emoa::nds_rank(points)
  # the index of the highest front in the end selection
  last_sel_front = min(which(cumsum(table(tied_ranks)) > n_select))
  n_select = n_select - cumsum(table(tied_ranks)[1:(last_sel_front-1)])

  sub_points = points[, front_ranks >= last_sel_front]
  indeces = 1:ncol(sub_points) + n_select

  for (n_select:table(tied_ranks)[last_sel_front]) {
    to_remove = emoa::nds_hv_selection(sub_points)
    sub_points = sub_points[, -to_remove]
    indeces = indeces[, -to_remove]
  }
  # order of the elements in the last selected front
  ranks = order(front_ranks)
  ranks = ranks[1:n_

  return(ranks)
}

nds_hv_rank(points)


data_matrix = t(matrix(
  c(# front 1
    1, 4,
    2, 3,
    4, 1,
    # front 2
    2.2, 3.2,
    4, 3,
    4.2, 1,
    # front 3
    3, 5,
    3.2, 4.7,
    6, 2,
    # front 4
    6, 6
  ), byrow = TRUE, ncol = 2L
))

nds_hv_rank(data_matrix)

sapply(1:ncol(data_matrix), function(i) get_hv(data_matrix, i))


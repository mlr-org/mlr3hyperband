context("nds_selection")


test_that("nds_selection", {

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

  # only the hypervolume contribution of the first front elements was manually
  # calculated, so we can only check for membership in each front, but not for
  # the exact truth
  expect_subset(
    nds_selection(data_matrix, 1),
    2:3
  )

  expect_equal(
    nds_selection(data_matrix, 2),
    2:3
  )

  expect_equal(
    nds_selection(data_matrix, 3),
    1:3
  )

  expect_subset(
    nds_selection(data_matrix, 4),
    1:6
  )

  expect_subset(
    nds_selection(data_matrix, 5),
    1:6
  )

  expect_equal(
    nds_selection(data_matrix, 6),
    1:6
  )

  expect_subset(
    nds_selection(data_matrix, 7),
    1:9
  )

  expect_subset(
    nds_selection(data_matrix, 8),
    1:9
  )

  expect_equal(
    nds_selection(data_matrix, 9),
    1:9
  )

  expect_equal(
    nds_selection(data_matrix, 10),
    1:10
  )

  # maximize (the front numbers above are not accurate anymore, so manual
  # estimation of the set containing the solution is required)
  expect_equal(
    nds_selection(data_matrix, 1, minimize = FALSE),
    10
  )

  expect_subset(
    nds_selection(data_matrix, 2, minimize = FALSE),
    10:7
  )

  expect_subset(
    nds_selection(data_matrix, 3, minimize = FALSE),
    10:7
  )

  expect_subset(
    nds_selection(data_matrix, 4, minimize = FALSE),
    5:10
  )

  expect_subset(
    nds_selection(data_matrix, 5, minimize = FALSE),
    10:4
  )

  expect_subset(
    nds_selection(data_matrix, 6, minimize = FALSE),
    10:4
  )

  expect_subset(
    nds_selection(data_matrix, 7, minimize = FALSE),
    4:10
  )

  expect_subset(
    nds_selection(data_matrix, 8, minimize = FALSE),
    1:10
  )

  expect_subset(
    nds_selection(data_matrix, 9, minimize = FALSE),
    1:10
  )

  expect_equal(
    nds_selection(data_matrix, 10, minimize = FALSE),
    1:10
  )


  # check if ref_point is not broken
  # maybe alter the test so the ref_point actually gives different result
  expect_equal(
    nds_selection(data_matrix, 1, ref_point = c(10, 10), minimize = FALSE),
    10
  )

  expect_equal(
    nds_selection(data_matrix, 2, ref_point = c(0, 0)),
    2:3
  )


  # check if more dimensions break something
  # 3 dimensional
  set.seed(123)
  points = rbind(
    y1 = runif(10),
    y2 = runif(10),
    y3 = runif(10)
  )

  expect_equal(
    nds_selection(points, 10),
    1:10
  )

  expect_equal(
    nds_selection(points, 10, rep(10, 3)),
    1:10
  )

  expect_equal(
    nds_selection(points, 10, minimize = FALSE),
    1:10
  )

  expect_equal(
    nds_selection(points, 10, rep(0, 3), FALSE),
    1:10
  )

  # 4 dimensional
  points = rbind(points, y4 = runif(10))

   expect_equal(
    nds_selection(points, 10),
    1:10
  )

  expect_equal(
    nds_selection(points, 10, rep(10, 4)),
    1:10
  )

  expect_equal(
    nds_selection(points, 10, minimize = FALSE),
    1:10
  )

  expect_equal(
    nds_selection(points, 10, rep(0, 4), FALSE),
    1:10
  )

})

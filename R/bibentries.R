format_bib = function(...) {
  str = vapply(list(...), function(entry) tools::toRd(bibentries[[entry]]), FUN.VALUE = "")
  paste0(str, collapse = "\n\n")
}

#' @importFrom utils bibentry
bibentries = c(
    li_2018 = bibentry("article",
      author  = "Lisha Li and Kevin Jamieson and Giulia DeSalvo and Afshin Rostamizadeh and Ameet Talwalkar",
      title   = "Hyperband: A Novel Bandit-Based Approach to Hyperparameter Optimization",
      journal = "Journal of Machine Learning Research",
      year    = "2018",
      volume  = "18",
      number  = "185",
      pages   = "1-52",
      url     = "https://jmlr.org/papers/v18/16-558.html"
  )
)

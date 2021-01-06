is_dom <- function(point, points){
  any(apply(point > points, 1, all))
}

dominated_by = function(point, points){
  apply(point > points, 2, all)
}

undominated = function(points, colnames){
  objectives = points[, ..colnames]
  t_objectives = t(objectives)

  dom_ids = apply(apply(objectives, 1, function(x) dominated_by(x, t_objectives)), 2, any)
  points[!dom_ids,,drop=FALSE]
}

strictly_better_idx = function(x, k){
  ordered_rank = rank(x)[order(x)]

  is_better = FALSE
  for(i in k:1){
    if(ordered_rank[i] < ordered_rank[i+1]){
      is_better = TRUE
      break
    }
  }

  if(!is_better){
    return(0)
  }else{
    return(i)
  }
}

generate_pairwise_labels <- function(dataset = NULL, x = NULL, y = NULL){
  
  label_df = pairwise_comparisons(data   = dataset, 
                                  paired = T,
                                  x      = {{x}}, 
                                  y      = {{y}}) |> 
    mutate(groups = purrr::pmap(.l = list(group1, group2), .f = c),
           result = sapply(expression, function(x) deparse(x[[2]])),
           label  = ifelse(p.value < 0.001, 
                           '"***"',#"italic(p)[\"Holm\" - adj.] < \"0.001\"",
                           ifelse(p.value < 0.01, 
                                  '"**"',
                                  ifelse(p.value < 0.05,
                                         '"*"',
                                         "italic(n.s.)")#sprintf("italic(p)[\"Holm\" - adj.] == \"%01g\"", round(p.value, 3))
                                  )
                           )
           ) 
  
  return(label_df)
  
}
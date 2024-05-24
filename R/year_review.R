year_review <- function(data=NULL,
                        var=NULL,
                        filterValue=NULL,
                        byVar=NULL){
  
  d <- data %>% 
    group_by(year,.data[[var]]) %>% 
    summarize(n = n()) %>% 
    mutate(pct=n/sum(n)) %>% 
    filter(pct<1)
  
  if(!is.null(filterValue)){
    d <- d %>% filter(.data[[var]]==filterValue)
  }
  
  g <- ggplot(d,aes(x=year,y=pct,color=factor(.data[[var]])))+
    geom_line()+
    geom_point()+
    geom_smooth(method="lm")+
    scale_color_discrete(name=stringr::str_wrap(sjlabelled::get_label(d[,2])[[1]],width=20),
                         labels=names(labelled::get_value_labels(d[,2])[[1]]))
  return(g)
}

bar <- function(data=NULL,
                var=NULL,
                grp=NULL,
                facet.wrap=FALSE,
                rel.pct=FALSE,
                title=NULL,
                subtitle=NULL,
                na.rm=FALSE,
                stack=FALSE,
                stack.fill=FALSE,
                color.reverse=FALSE,
                show.values=FALSE,
                palette = "Spectral"){
  
  require(haven)
  require(ggplot2)
  require(dplyr)
  require(RColorBrewer)
  require(stringr)

  data <- data %>% 
    dplyr::select({{var}},{{grp}}) %>% 
    dplyr::rename(var = {{var}},
                  grp = {{grp}})
  
  if(na.rm==TRUE){
    data <- data %>% filter(!is.na(var))
    if(!is.null(grp)){
      data <- data %>% filter(!is.na(grp))  
    }
  }
  
  if(is.null(grp)){
    data <- data %>% group_by(var)
  }else{
    data <- data %>% group_by(grp,var)
  }
  data <- data %>% 
    dplyr::summarize(count = n()) %>%  
    dplyr::mutate(pct = count/sum(count)) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(val = ifelse(rel.pct==TRUE,pct,count)) %>% 
    dplyr::mutate(val.labels = ifelse(rel.pct==TRUE,
                               scales::percent(pct),
                               scales::comma(count)))
  
  if(haven::is.labelled(data$var)){
    var.label <- attributes(data$var)$label
    data$var <- haven::as_factor(data$var)
  }
  
  if(!is.null(grp) && haven::is.labelled(data$grp)){
    grp.label <- attributes(data$grp)$label
    data$grp <- haven::as_factor(data$grp)
  }
  
  if(!is.null(grp)){
    if(facet.wrap==TRUE){
      fill <- data$var 
      fill.label <- var.label
    }else{
      fill <- data$grp
      fill.label <- grp.label
    }
  }else{
    fill <- data$var
    fill.label <- var.label
  }

  g <- ggplot(data,aes(x=var,fill=fill,y=val)) + 
    geom_col(position = ifelse(stack==FALSE,
                               "dodge2",
                               ifelse(stack.fill==FALSE,
                                      "stack",
                                      "fill")))

  if(show.values==TRUE){
    g <- g + geom_text(aes(label=val.labels),vjust=-.5)
  }
  
    g <- g + scale_y_continuous(labels = ifelse(rel.pct==TRUE,
                                                scales::percent,
                                                scales::comma))

  g <- g + scale_x_discrete(labels = scales::label_wrap(20))
  
  ifelse(color.reverse==TRUE,
         cr <- -1,
         cr <- 1)
  
  g <- g + scale_fill_brewer(name=stringr::str_wrap(fill.label,30),
                             labels=scales::label_wrap(30),
                             palette = palette,
                             direction = cr)
  
  if(facet.wrap==TRUE){
    g <- g + theme(axis.text.x = element_blank(),
                   axis.ticks.x = element_blank())
  }else{
    g <- g + xlab(stringr::str_wrap(var.label,60))
    g <- g + theme(axis.text.x = element_text(angle=45,
                                              vjust=1,
                                              hjust=1))
  }
  
  if(facet.wrap==TRUE){
    if(is.null(grp)){
      stop("Must include grp variable to use facet.wrap.")
    }
    g <- g + facet_wrap(vars(stringr::str_wrap(grp,20)))
  }
  
  return(g)
    
}
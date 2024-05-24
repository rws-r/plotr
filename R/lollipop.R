lollipop <- function(data = NULL,
                     var = NULL,
                     grp = NULL,
                     type = "pct",
                     year = NULL,
                     title = NULL,
                     subtitle = NULL,
                     na.rm = F,
                     sortByCount = F,
                     sortByVariance = F,
                     print_data = F
){
  require(dplyr)
  require(tidyr)
  
  if(exists("yearsValid")){
    if("year" %in% names(data)){
      check <- yearsValid(data,var)
      check_grp <- yearsValid(data,grp)
      if(!(year %in% check$year)){
        stop(paste("That variable doesn't exist for that year. Try ",paste(check$year,collapse=", "),"instead."))
      }
      if(!(year %in% check_grp$year)){
        stop(paste("That grouping variable doesn't exist for that year. Try ",paste(check_grp$year,collapse=", "),"instead."))
      }
    }
  }
  
  # Filter year
  if(!is.null(year))data <- data[data$year==year,]
 
  # Select rows
  data <- data[,c(var,grp)]
  varnames <- names(data)
  names(data) <- c("var","grp")
  if(haven::is.labelled(data$var)){
    label_var <- labelled::get_variable_labels(data$var)
    labels_var <- names(labelled::get_value_labels(data$var))
    title <- label_var
  }else{
    label_var <- NULL
    if(!is.null(levels(data$var))){
      labels_var <- levels(data$var)
    }else{
      labels_var <- unique(data$var)
    }
  }
  if(haven::is.labelled(data$grp)){
    label_grp <- labelled::get_variable_labels(data$grp)
    labels_grp <- names(labelled::get_value_labels(data$grp))
  }else{
    label_grp <- NULL
    if(!is.null(levels(data$var))){
      labels_grp <- levels(data$grp)
    }else{
      labels_grp <- unique(data$grp)
    }  }
  
  if(na.rm==T){
    data <- data %>% filter(!is.na(var),
                            !is.na(grp))
  }

  data <- data %>% 
    group_by(var,grp) %>% 
    summarize(count=n()) %>% 
    ungroup() %>% 
    group_by(grp) %>% 
    mutate(pct = count/sum(count))

  if(haven::is.labelled(data$grp)){
    data <- data %>% mutate(
      grp.values = grp,
      grp = labels_grp[grp.values]
    )
  }else{
    data <- data %>% mutate(
      grp.values = grp,
      grp = grp.values)
  }
  
  data <- data %>% 
    pivot_wider(
      names_from = var,
      values_from = c(count,pct)) %>% 
    ungroup() %>% 
    mutate(tot = count_1 + count_2,
           variance = abs(pct_2-pct_1))
  
  if(sortByVariance==T){
    data <- data %>% arrange((variance))
  }else if(sortByCount==T){
    data <- data %>% arrange(tot)
  }else{
    data <- data %>% arrange(desc(grp.values))
  }
  
  data <- data %>% mutate(grp = factor(grp,grp))
  
  colors <- c("darkgreen","darkred","lightgreen","salmon","lightblue","darkblue","darkorange","gold")
  colors <- colors[1:length(labels_var)]
  colorVals <- setNames(colors,labels_var)
  
  if(print_data==T)print(data)

  plot <- ggplot(data) +
    geom_segment(aes(x=grp,
                     xend=grp,
                     y=pct_1,
                     yend=pct_2)) +
    geom_point(aes(x=grp,
                   y=pct_1,
                   color=labels_var[1]),
               size=3) +
    geom_point(aes(x=grp,
                   y=pct_2,
                   color=labels_var[2]),
               size=3) +
    scale_x_discrete(name=varnames[2]) +
    scale_y_continuous(name=varnames[1],labels=scales::percent) +
    scale_color_manual(name = "Response",
                       breaks = c(labels_var[1],labels_var[2]),
                       values = colorVals) + 
    coord_flip() +
    labs(title=title,subtitle = paste("For year ",year,"",sep="")) +
    theme(legend.title=element_text(size=12),
         legend.text=element_text(size=10))

  
  return(plot)
}
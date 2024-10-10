# add options('box.path' = "C:/Users/leuve002/git/") to ~/.Rprofile and
# call this file with: box::use(box_modules/helper_functions[create_codebook])

create_codebook <- function(.df,stats = list(mean=mean,sd=sd,min=min,max=max,
                                             prop_miss=prop_miss)) {
  
  # takes a dataframe, and outputs a dataframe with variable names, labels
  # and some summ stats
  # useful to get a first sense of a data set.

  # import packages
  box::use(r/core[...])  
  box::use(dplyr[`%>%`, coalesce, mutate, bind_cols ])
  box::use(tibble[tibble, as_tibble_row])
  box::use(purrr[map, map_chr, map_dbl, list_rbind])
  
  labels <- tibble(var = colnames(.df),
                   label = map_chr(.df,function(x) coalesce(attributes(x)$label,"")),
                   type = map_chr(.df, typeof))
  
  
  prop_miss <- function(x,na.rm = TRUE) {
    mean(is.na(x))
  }
  
  stats_to_tibble <- function(var,stats) {
    map_dbl(stats,~ifelse(is.numeric(var),.x(var,na.rm = TRUE),NA)) %>%
      as_tibble_row()
  }
  
  sumstats <-
    .df %>%
    map(~stats_to_tibble(.x,stats)) %>%
    list_rbind() %>%
    mutate(across(where(is.numeric),
                  ~round(.x,2)))  
  
  bind_cols(labels,sumstats)
}



winsorize <- function(x, lower = 0.05, upper = 0.95){
  
  # remove outliers
  # I didn't want to include a whole library just for this.
  
  lower_bound <- quantile(x, lower)  # 5th percentile
  upper_bound <- quantile(x, upper)  # 95th percentile
  
  #Winsorize: Replace values outside the bounds with the respective bounds
  case_when(
    x < lower_bound ~ lower_bound,
    x > upper_bound ~ upper_bound,
    TRUE ~ x
  )
}



stacked_bar_plot <- function(df, fill, by, textsize = 20, 
                             legendposition = "bottom", legendrows = 2,
                             labs = NULL) {

  # takes a dataframe and two categorical variables (fill and by)
  # outputs a plot stacked bar chart, where:
  # the by variables will be on the y-axis
  # the x-axis will be counts of the fill variable
  # the plot will include counts of the fill variables
  # to include counts of the by variable, use count_label

  # import packages
  box::use(r/core[...])  
  box::use(dplyr[...])
  box::use(ggplot2[...])
  box::use(extrafont[...])
  # box::use(ggthemewur[...])

  
  
  # outputs a stacked bar chart
  df %>%
    ggplot(aes(y = {{ by }}, fill = {{ fill }})) +
    geom_bar(position = position_fill(reverse = TRUE)) +
    geom_text(stat = "count", 
              aes(label = after_stat(count)), 
              position = position_fill(vjust = 0.5, reverse = TRUE), 
              color = "white") +
    scale_x_continuous(labels = scales::percent_format()) +
    theme(text = element_text(size = textsize)) +
    theme(legend.position=legendposition) +
    guides(fill = guide_legend(nrow = legendrows, title.position="top", title.hjust = 0.5)) +
    labs(title = "", fill = "", x = "", y = "") +
    labs(!!!labs)
  
    # +
    # theme_wur() +
    # scale_fill_wur_discrete()
  
}


count_label <- function(vector) {

#  takes a factor or string vector
# outputs same vector, as a factor, with counts appended after a line break
# "category 1" -> "category1\nn=1"  
# useful for plotting

  # import packages
  box::use(dplyr[...])
  box::use(forcats[fct_recode])
  
  
  
  #takes a vector of strings or factor, 
  # output a factor vector with N = N included in the labels
  fct_recode(factor(vector), 
             !!!vector %>%
               as_tibble %>%
               group_by(value) %>%
               summarize(n = n()) %>%
               mutate(value = as.character(value),
                      newlabel = paste0(value,"\nn=",n)) %>%
               pull(value, name = newlabel))
  
}

load_data <- function(
    data_loc,form_id,form_schema = ruODK::form_schema(),xlsform) {

  box::use(r/core[...])  
  box::use(dplyr[...])
  box::use(tibble[...])
  box::use(purrr[...])
  box::use(readxl[...])
  box::use(tidyr[...])
  box::use(stringr[...])
  box::use(readr[...])
  box::use(here[...])
  box::use(janitor[...])


  # Fetch form schema
  #form_schema <- ruODK::form_schema()

  # get all the group names from the schema, so we can remove them from varnames
  group_names <- 
   form_schema %>%
   filter(type == "structure") %>%
   pull(name)

  clean_names <- function(df, group_names) {

    #remove group names using a regex containing all the group names
    pattern <- paste(paste0(group_names,"_?"), collapse = "|")
    colnames(df) <- gsub(pattern, "", colnames(df))

    # make sure colnames are unique
    colnames(df) <- make_clean_names(colnames(df), unique_sep = "_")
    
    return(df)
  }

  # label definition
  xls_form_choices <- 
    read_excel(xlsform,
               sheet = "choices") %>% 
    filter(!is.na(list_name)) 

  labels <-  
   xls_form_choices %>%
    distinct(list_name) %>%
    pull(list_name, name = list_name) %>% 
    map( ~ xls_form_choices %>%
           filter(list_name == .x ) %>%
           with(list(name = name, label = `label::English (en)`)))



  # variable mapping to labels
  xls_form_survey <- 
    read_excel(xlsform,
               sheet = "survey") 


  # a tibble of variable names and the labelset to apply to them
  select_one <-
    xls_form_survey %>%
    filter(str_detect(type,"^select_one")) %>%
    separate(type, into = c("type", "list"), sep = " ", extra = "merge") %>%
    filter(!str_detect(list,"^\\$\\{.+\\}$")) %>%
    filter(!str_detect(list,"\\.csv$")) %>%
    select(name,list)


  # function to apply labels to select_one questions
  label_select_one <- function(df,labels,vars){

    for (var in names(df)){
      if (var %in% vars$name){
        labelset <-
          select_one %>%
          filter(name == var) %>%
          pull(list) 

        df[[var]] <-  factor(df[[var]],
                             levels = labels[[labelset]]$name,
                             labels = labels[[labelset]]$label)

      }

    }
    df
  }

  table_names <-
   c("main",
      form_schema %>%
      filter(type == "repeat") %>%
      pull(name))


  file_names <- 
      paste0(c(form_id,
               paste(form_id,
                     table_names[2:length(table_names)],sep ="-")),
            ".csv")


  all_data <-
  file_names %>%
    set_names(table_names) %>%
    map(~ read_csv(here(data_loc,.x))) %>%
    map( ~ clean_names(.x,group_names)) %>%
    map(~ label_select_one(.x,labels = labels, vars = select_one))




    return(all_data)
}


# reshape the data to one wide data set using two function

# one to reshape a subtable to the shape of its parent

reshape_subtable <- function(df){
  box::use(r/core[...])  
  box::use(dplyr[...])
  box::use(tidyr[...])
  box::use(stringr[...])


  # you don't need to reshape something that doesn't have a parent...
  if ("parent_key" %in% names(df)) {
    output <- 
      df %>%
        mutate(key = str_extract(key, "\\[([0-9]+)\\]$", group = 1)) %>%
        pivot_wider(id_cols = parent_key, names_from = key, values_from = -parent_key) %>%
        rename(key = parent_key)

  } else {
    output <- df
  }
  
  output
}  

# and one two merge all subtables, recursively

merge_subtables <- function(.data,table = "main", form_schema = ruODK::form_schema()) {

  box::use(r/core[...])  
  box::use(dplyr[...])
  box::use(tibble[...])
  box::use(purrr[...])
  box::use(tidyr[...])
  box::use(stringr[...])

  # get a list of subtables
  get_parent <- function(path,name) {
    pattern <- paste0("(?<=/)(",paste(name, collapse = "|"),")(?=/)")
    map_chr(path,~{
            matches <-  str_match_all(.x, pattern)[[1]][,1]  %>% tail(1)
            if (length(matches) > 0) tail(matches, 1) else "main"
            }) 
  }

  subtables <-
      form_schema %>% 
      filter(type == "repeat") %>%
      mutate(parent = get_parent(path,name)) %>%
      select(name,parent) %>%
      filter(parent == table) %>%
      pull(name, name = name)


  # if no subtables, return reshaped subtable
  if (length(subtables) == 0) {
     output <-
       .data[[table]] %>%
       reshape_subtable()
     
  } else {
    # if there are subtables, run this function on all of them
    # (effectively reshaping all tables to fit their parents)
    # and merge it all togther
      output <-
     left_join(.data[[table]] %>% reshape_subtable(),
               subtables %>%
                 map(~merge_subtables(.data,.x,form_schema)) %>%
                 reduce(~left_join(.x,.y,by = join_by(key))),
               by = join_by(key))
  }
  output
}



get_group_duration <- function(audit_log,group, winsorize = FALSE, kobo = FALSE) {

  box::use(r/core[...])  
  box::use(dplyr[...])
  box::use(stringr[str_detect, str_extract])

  # kobo works a bit different than ODK; for now it just has a flag, but
  # I guess we can clean this up or automatically detect if we're using kobo
  # or ruODK

  if (!kobo){
    return <-
      audit_log %>%
      filter(str_detect(node, group)) %>%
      filter(event %in% c("question","group questions")) %>%
      mutate(duration = (end - start) / (1000 * 60)) %>% 
      {
        if (winsorize) {
          box::use(./utils)
          {.} %>% 
            mutate(name = str_extract(node, "[^/]+$")) %>%
            mutate(duration = utils$winsorize(duration),.by = name)
        } else {
          {.}

        }
      } %>%
      summarize(group = group, duration = sum(duration,na.rm = TRUE), .by = `instance ID`) 
  } else {
    return <-
      audit_log %>%
      filter(str_detect(node, group)) %>%
      filter(event %in% c("question","group questions")) %>%
      mutate(duration = as.numeric((end - start),"mins")) %>%
      {
        if (winsorize) {
          box::use(./utils)
          {.} %>% 
            mutate(name = str_extract(node, "[^/]+$")) %>%
            mutate(duration = utils$winsorize(duration),.by = name)
        } else {
          {.}

        }
      } %>%
      summarize(group = group, duration = sum(duration,na.rm = TRUE), .by = "_id") 
  } 
  return
}



separate_labelled <- function(df, var, choices, labelset, labels = NULL, prefix = NULL) {


  box::use(r/core[...])  
  box::use(rlang[...])  

  box::use(dplyr[...])
  box::use(stringr[str_detect])
  box::use(purrr[...])


  choiceset <-
    choices %>% 
    filter(`list name` == labelset)

  create_var <- function(value){
  
  if (!quo_is_null(enquo(labels))) {
    value_label <-
      choiceset %>%
      filter(as.character(name) == as.character(value)) %>%
      pull({{ labels }})
  } else {
    value_label <- value
  }

  if (is.null(prefix)) {
    new_var_name <- paste0(as_label(enquo(var)), "_", value_label)
  } else {
    new_var_name <- paste0(prefix, value_label)

  }
  
  df %>%
    select({{var}}) %>%
    mutate(
      !!sym(new_var_name) := 1 * str_detect( {{ var }}, paste0("(^| )", value, "( |$)"))
    ) %>%
    select( - {{ var }})
  } 

  bind_cols(
    df,
    choiceset %>%
      pull(name) %>%
      map( ~create_var(.x))
  )

}

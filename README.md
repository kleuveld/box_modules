# Introduction

[This repo](https://github.com/kleuveld/box_modules) contains two R scripts with helper functions, utils.R and plots.R and (see function index).
These scripts can be called using the [`box`](https://github.com/klmr/box) package, 
in effect working just like a 

# How to use

- [Read how to use the box package](https://klmr.me/box/articles/box.html).
- Clone this repo, or copy the files anywhere on your device, for example: `c:\path\to\box_modules`
- add `options('box.path' = "C:/path/to/")`  to
 [`~/.Rprofile`](https://support.posit.co/hc/en-us/articles/360047157094-Managing-R-with-Rprofile-Renviron-Rprofile-site-Renviron-site-rsession-conf-and-repos-conf) 
 (NB: that's the parent folder of the folder where your scripts are in!)
- In the script where you want to use a function use `box::use(box_modules/utils)` so the functions are loaded.
- Call the function using the form `script$function()`, 
so for example to winsorize a variable use `mutate(var = utils$winsorize(var)`
- To find the modules using [`here`](https://here.r-lib.org/),
make sure the modules are within your project folder and
add `options(box.path = here::here(""))` to the 
`.Rprofile` of your project. Use `box::use(relativepath/to/box_modules/utils)`
to load a module, where the relative path starts at the project root.

# Function index

For now you'll need to have a look at the actual function to see how to use them.
Better documentation may be coming.

- utils.R: utility functions to manage data. 
   - create_codebook: creates a "codebook"-dataframe containing variable labels and summ stats of data frame.
   - winsorize: winsorize a vector
   - count_label: adds counts to the labels of factors, useful for some plots
   - truncate_sample: truncates a vector by resampling out-of-bounds observations
   from within-bounds observations. Useful for generating fake data.
   - isid: checks if a data frame is uniquely identified by the variables 
   supplied, and stops executing scripts if it is not.
   Useful after generating ID codes to ensure they are valid.
   - colourer: for generating a function that can be used in flextable::bg(),
   based on a domain and palette supplied as arguments:
   Example use: `bg(ft_2, bg = utils$colourer(domain = c(0,5), palette = c("transparent","red), part = "body")`
   - conditional_format: for colouring flextable columns, based on actual data.
   Example use: `utils$conditional_format(ft_2, .cols = c("var1"."var2"), palette = c("red","transparent","red"))`
   - find_var: for finding a variable in a data set.
   Example use: `utils$find_var(df, "hh_")` returns the names of variables containing 
   `hh_` in dataset `df`.
   - group_differences: produces a flextable of differences between groups.
   Example use: `utils$group_differences(df,outcomes = c("yield", "income"), treatment = "treat")`
   to compute the difference in outcomes between treatment and control. Optionally provide
   arguments `group` (for heterogeneous effects), `labels` (named vector providing mapping
   between variable names and labels), `clusters`, `weights`, `controls`, and `footer`.
   - generate_ids: generate id codes based on ref data. 
   Example use: `utils$generate_ids(district, community, farmer)` generates nested IDs
   `district_id` (1, 2, ...), `community_id` (1-1, 1-2, 2-1, ...) and `farmer_id` 
   (1-1-1,...).
   - summstats: creates a quick tibble of summstats, respecting groups.
   `df %>% group_by(group) %>% summstats()` creates a tibble with n, min, p5, median, 
   p95, and max values. Summary functions can be provided by providing a named list  
   as the `.fns` argument, e.g. `.fns = list(mean = ~mean(.x, na.rm = true))`
   - replace: change value conditionally, while handling NAs elegantly. 
   Example usage: `df %>% mutate(x = utils$replace(x,NA,x == 99))` replaces all instances
   of `99` in a variable with NA.
- plots.R: plots:
  - stacked_bar_plot: a stacked bar chart.
  - stacked_bar_plot_wur: the same, but in WUR housestyle. Requires the `[ggthemewur](https://git.wur.nl/wmrkdown/ggthemewur)` package.
- odk_helpers.R
  - load_data(): get data exported by `ruODK::submission_export()`, load and label it. Arguments:
    - data_loc: the location where `ruODK` put your data.
    - form_id: the form_id
    - form_schema: the schema of the form. Defaults to `ruODK::form_schema()`
    - xlsform: the xlsform that contains your form definition.
  Returns a list of dataframes.
  - reshape_subtable: takes a dataframe, and pivots it wider to the shape of its parent.
  - merge_subtables: create one wide dataset of a table and all its subtables.
Arguments:
    - .data: list as outputed by `load_data()`
    - table: name of the table to merge subtables to. 
Defaults to "main", to create one wide data frame of the entire data set.
    - form_schema: the schema of the form. Defaults to `ruODK::form_schema()`
  - get_group_duration: get durations of groups from the audit log.  Arguments:
    - audit_log: path to audit log csv
    - group: group name to get duration of 
    - winsorize: set to TRUE to winsorize durations. Defaults to FALSE. 
  Requires the utlis.R file to be present in the same folder.

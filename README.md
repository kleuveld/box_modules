# Introduction

[This repo](https://github.com/kleuveld/box_modules) contains three R scripts with helper functions, utils.R, plots.R and esitmators.R (see function index).
These scripts can be called using the `[box](https://github.com/klmr/box)` package, 
in effect working just like a 

# How to use

- [Read how to use the box package](https://klmr.me/box/articles/box.html).
- Clone this repo, or copy the files anywhere on your device, for example: `c:\path\to\box_modules`
- add `options('box.path' = "C:/path/to/")` 
(so the parent folder of the path where the scripts are) to
 `[~/.Rprofile](https://support.posit.co/hc/en-us/articles/360047157094-Managing-R-with-Rprofile-Renviron-Rprofile-site-Renviron-site-rsession-conf-and-repos-conf)`
- In the script where you want to use a function use `box::use(box_modules/utils)` so the functions are loaded.
- Call the function using the form `script$function()`, 
so for example to winsorize a variable use `mutate(var = utils$winsorize(var)`


# Function index

- utils.R: utility function to manage data. 
   - create_codebook: creates a "codebook"-dataframe containing variable labels and summ stats of data frame.
   - winsorize: winsorize a vector
   - count_label: adds counts to the labels of factors, useful for some plots
- plots.R: plots:
  - stacked_bar_plot: a stacked bar chart.
  - stacked_bar_plot_wur: the same, but in WUR housestyle. Requires the `[ggthemwure](https://git.wur.nl/wmrkdown/ggthemewur)` package.

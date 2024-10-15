# add options('box.path' = "C:/Users/leuve002/git/") to ~/.Rprofile and
# call this file with: box::use(box_modules/estimators[glm_robust])


# this function estimates a probit model, and 
# then computes the cluster-robust standad errors using
# clubSandwich and coeftest
# it returns a glm_robust object, which is just a modified 
# coeftest object
glm_robust <- function(formula,family,data,cluster, type = "CR2") {
  box::use(r/core[...])  
  box::use(dplyr[...])
  box::use(lmtest[...])
  box::use(clubSandwich[...])

  probit <- 
    glm(formula, 
        family = family, 
        data = data)
  
  model <-
    probit%>%
    coeftest(.,
             vcovCR({.}, 
                    cluster = data[[cluster]], 
                    type = type), 
             save = TRUE) 
  
  
  # this computes the number of unique clusters in the data used
  # for the original mode
  attr(model,"nclusters") <- 
    data[row.names(model.frame(probit)),cluster] %>% unique() %>% nrow()
  
  class(model) <- "glm_robust"
  model
}

# this is the custom tidy methods for glm_robust objects
# it returns a dataframe with coefficients
tidy.glm_robust <- function(x, ...){
  box::use(r/core[...])  
  box::use(dplyr[...])
  
  x[,] %>% as_tibble() %>%
    mutate(term = attr(x,"dimnames")[[1]]) %>%
    select(term,
           estimate = Estimate,
           std.error = `Std. Error`,
           statistic = `z value`,
           p.value = `Pr(>|z|)`) 
}


# this is the glance method. It returns a data frame with 
# the number of obserations, log likelihood and number of clusters
glance.glm_robust <- function(x, ...){
  box::use(r/core[...])  
  box::use(dplyr[...])
  
  tibble(nobs = attr(x,"nobs"),
         logLik = as.numeric(attr(x,"logLik")),
         nclusters = attr(x,"nclusters")
  )
}


# register the methods
box::use(broom[glance])
box::register_S3_method("glance", "glm_robust", glance.glm_robust)

box::use(broom[tidy])
box::register_S3_method("tidy", "glm_robust", tidy.glm_robust)

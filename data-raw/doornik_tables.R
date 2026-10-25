## code to prepare `doornik_tables` dataset goes here

Hnames <- c("H_z", "H_c", "H_lc", "H_l", "H_ql")

get_tsdyn <- function(name) getFromNamespace(name, "tsDyn")

## Table 7: asymptotic mean (6 rows) / variance (5 rows) of the TRACE stat.
tab7_mean <- get_tsdyn("doornik_tab7_mean")
tab7_var  <- get_tsdyn("doornik_tab7_var")

## Table 8: asymptotic mean / variance of the MAX EIGENVALUE stat (5 rows).
tab8_mean <- get_tsdyn("doornik_tab8_mean")
tab8_var  <- get_tsdyn("doornik_tab8_var")

## Table 9: small-sample correction, TRACE only (7 rows).
tab9_mean <- get_tsdyn("doornik_tab9_mean")
tab9_var  <- get_tsdyn("doornik_tab9_var")

## preserve existing sysdata.rda contents, if any
existing_env <- new.env()
if (file.exists("R/sysdata.rda")) {
  load("R/sysdata.rda", envir = existing_env)
}
existing_names <- ls(existing_env)
list2env(as.list(existing_env), envir = environment())

## ... your Hnames / get_tsdyn() / tab7_mean.../tab9_var code as before ...

all_names <- union(existing_names,
                   c("tab7_mean", "tab7_var", "tab8_mean", "tab8_var",
                     "tab9_mean", "tab9_var", "Hnames"))
save(list = all_names, file = "R/sysdata.rda")

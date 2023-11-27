f_in <- "~/Downloads/scimagojr 2022.csv"
df_sjr <- readr::read_csv2(f_in)

df_sjr <- df_sjr[ , c("Title",	"Issn", "SJR", "H index")]

f_out <- paste0("inst/extdata/fixed_",
                janitor::make_clean_names(basename(f_in)),
                '.csv')
readr::write_csv(df_sjr, f_out)

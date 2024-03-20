magic <- function(start, pull_start, retirement_years, social) {
  df = data.frame()
  for (i in 1:retirement_years) {
    inf_pull <- pull_start * 1.029^(i-1)      # adjust amount to pull by inflation
    start = start * 1.05                  # Starting amount, X HYSA
    social = social * 1.029                   # social security payments, inflation adj
    pull = pull_start                         # amount pulling out every year, inflation adj
    pull = inf_pull - social                  # amount pulling out subtracted by social security payments that year
    start = start - pull      
    df = rbind(df, start)
    if (start < 0 ) {
      break
    }
  }
  df
}

magic(3500000, 219000, 30, 30000) ##takeaway is you need around 4.4 million starting to pull out 219,000 a month and be good.

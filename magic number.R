magic <- function(x, z, age, age_death) {
df = data.frame()
years <- age_death - age
for (i in 1:years) {
  z = z * 1.025
  x = x - z
  x = x * 1.043
  df = rbind(df, x)
  age <- age + 1
}
df
}

magic(2000000, 80000, age = 57, age_death = 92)

### social security, age 62.

magic <- function(start, pull_start, retirement_years, social) {
  df = data.frame()
  for (i in 1:retirement_years) {
    inf_pull <- pull_start * 1.025^(i-1) # adjust amount to pull by inflation
    start = start * 1.044     # Starting amount, 4% HYSA
    social = social * 1.025   # social security payments, inflation adj
    pull = pull_start       # amount pulling out every year, inflation adj
    pull = inf_pull - social      # amount pulling out subtracted by social security payments that year
    start = start - pull      
    df = rbind(df, start)
  }
  df
}

magic(3500000, 110000, 100, 500)

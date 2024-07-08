#.libPaths("/data/home/kyleb/Rlibs")

library(dplyr)
library(bbr)
library(devtools)

data <- read.csv("inst/extdata/acop.csv")
data$FRUIT <- rep(c("banana", "apple", "pear"), length.out = nrow(data))
write.csv(data, "inst/extdata/acop2.csv")

devtools::load_all()

mod <- read_model("inst/model/nonmem/basic/2")

bbr:::nm_data_filter(mod)

# Reading data file: acop2.csv
# rows: 799
# cols: 12

# A tibble: 0 × 12
# ℹ 12 variables: V1 <int>, ID <int>, TIME <dbl>,
#   MDV <int>, EVID <int>, DV <dbl>, AMT <int>,
#   SEX <int>, WT <dbl>, ETN <int>, NUM <int>,
#   FRUIT <chr>


mod <- read_model("inst/model/nonmem/basic/1")

bbr:::nm_data_filter(mod)

# # A tibble: 779 × 10
# ID  TIME   MDV  EVID    DV   AMT   SEX    WT   ETN
# <int> <dbl> <int> <int> <dbl> <int> <int> <dbl> <int>
#   1     1  0        0     0  1.22     0     1  51.6     1
# 2     1  0.25     0     0 12.6      0     1  51.6     1
# 3     1  0.5      0     0 11.2      0     1  51.6     1
# 4     1  0.75     0     0 17.7      0     1  51.6     1
# 5     1  1        0     0 21.9      0     1  51.6     1
# 6     1  1.25     0     0 16.2      0     1  51.6     1
# 7     1  1.5      0     0 18.7      0     1  51.6     1
# 8     1  1.75     0     0 23.8      0     1  51.6     1
# 9     1  2        0     0 16.5      0     1  51.6     1
# 10     1  2.25     0     0 17.3      0     1  51.6     1
# # ℹ 769 more rows
# # ℹ 1 more variable: NUM <int>
# # ℹ Use `print(n = ...)` to see more rows


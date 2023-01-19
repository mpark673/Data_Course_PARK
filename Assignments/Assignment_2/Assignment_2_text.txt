#Assignment 2
# Number 4
csv_files <- list.files(path = "Data",pattern = ".csv")

# Number 5
length(csv_files)

# Number 6
df <- read.csv("Data/wingspan_vs_mass.csv")

# Number 7

?head()
head(df,n=5)

# Number 8
list.files(path = "Data",
           recursive = TRUE,
           pattern = "^b",
           full.names = TRUE,
           ignore.case = FALSE)
# Number 9
b_files <- list.files(path = "Data",
                      recursive = TRUE,
                      pattern = "^b",
                      full.names = TRUE,
                      ignore.case = FALSE)
for(i in b_files){print(readLines(i,n=1))}

# Number 10
list.files(path = "Data",
           recursive = TRUE,
           pattern = ".csv$",
           full.names = TRUE)
csv_files <- list.files(path = "Data",
                        recursive = TRUE,
                        pattern = ".csv$",
                        full.names = TRUE)
for(i in csv_files){print(readLines(i,n=1))}

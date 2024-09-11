
#4

csv_files <- list.files( path = 'Data/', pattern = '.csv', full.names = TRUE, recursive = TRUE)

#5

length(csv_files)

#6

df <- read.csv('Data/wingspan_vs_mass.csv')

#7

head(df, n=5L)

#8

list.files(path = 'Data/', pattern = '^b', full.name = TRUE, recursive = TRUE)

files_starting_with_b <- list.files(path = 'Data/', pattern = '^b', full.name = TRUE, recursive = TRUE)

#9

for (file in files_starting_with_b) { 
  first_line <- readLines(file, n = 1)
cat("File:", file, "\nFirst line:", first_line, "\n\n")
  }

#10

for (file in csv_files) {
  first_line <- readLines(file, n = 1)
  cat("File:", file, "\nFirst line:", first_line, "\n\n")
}

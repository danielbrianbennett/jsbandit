## Set version number (make sure we only get the version of interest)
version.number <- 2.2

## Set directory parameters
base.directory <- "C:\\Users\\dbennett1\\Google Drive\\Works in Progress\\JSBANDIT\\Bandit\\analysis\\"
directory.separator <- "\\"
file.name <- "datadump_16Jan2016_1254PM.csv"

## Load datafile
input.file <- paste(base.directory, file.name, sep = directory.separator)
setwd(base.directory)
input.file <- "datadump_16Jan2016_1254PM.csv"

# this is the location you want the python parser to write the parsed CSV file
output.file <- "jsbandit.csv"
file.create(paste(base.directory,output.file, sep = directory.separator))

# run 'python parser.py input_file output_file' 
system(paste('python parser.py', input.file, output.file))
# should print: Done parsing!

# read the results of parsing into R
parsed.data <- read.csv(file=output.file, header=T, stringsAsFactors=F)

# separate choice data from demographic data
unsorted.data <- parsed.data[which(!is.na(parsed.data$pointsWon)),]
unsorted.data <- unsorted.data[unsorted.data$language != "fakeparticipant",]
unsorted.data <- unsorted.data[unsorted.data$version == version.number,]

# resort data frame into a more legible format
sorted.data <- unsorted.data[order(unsorted.data$ID, unsorted.data$block, unsorted.data$trial),c(10,20,12,3,14,1,2,6)]

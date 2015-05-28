readAndCleanLineData <- function(file, boyNames, girlNames, 
                                 boyLabel = boyNames[1], 
                                 girlLabel = girlNames[1]) {
    # read line message text file into data frame
    data <- readLineData(file)
    
    # classify the names of senders into two labels
    data <- unifySenderNames(data, boyNames, girlNames, boyLabel, girlLabel)
    
    # clean and generate data
    data <- formatData(data, c(boyLabel, girlLabel))
    data <- generateExtraInfos(data)
}

readLineData <- function(file) {
    # column names
    colNames <- c("time", "name", "content")
    
    # read text file to data frame
    data <- read.csv(file, header = FALSE, fill = TRUE, sep = "\t", quote = "",
                     stringsAsFactors = FALSE, colClasses = rep("character", 3),
                     blank.lines.skip = TRUE, col.names = colNames)
}

unifySenderNames <- function(data, boyNames, girlNames, boyLabel, girlLabel) {
    data[data$name %in% boyNames,]$name <- boyLabel
    data[data$name %in% girlNames,]$name <- girlLabel
    data
}

formatData <- function(data, nameLabels) {
    # convert 24 o'clock to 0 o'clock
    data$fullTime <- gsub("24:", "0:", data$time)
    
    # sometimes, the file is imperfect or the data is for system usage, 
    # so the beginning rows are useless
    skipHeader <- TRUE
    
    # the cursor which points to previous valid row
    validCursor <- 1
    
    # row numbers of useless rows
    rmRow <- numeric(0)
    
    # clean data begin
    for(i in seq_len(nrow(data))) {
        
        if(grepl(x = data[i,]$fullTime, pattern = "^[0-9]+/[0-9]+/[0-9]+")) {
            # proper date format, store it
            
            # the data of next row will be useful, no need to skip it anymore
            skipHeader <- FALSE
            
            # store the date for the following rows
            currentDate <- gsub("\\(\\S+\\)", "", data[i,]$fullTime)
            
            # transfer Chinese year to Common Era
            currentYear <- as.numeric(gsub("\\/[0-9]+", "", currentDate))
            if(nchar(currentYear) != 4) {
                currentDate <- gsub("^[0-9]+\\/", paste(currentYear + 1911, "/", 
                                                        sep = ""), currentDate)
            }
            
            # this row is valid, but what we need is only the date
            validCursor <- i
            rmRow <- append(rmRow, i)
            
        } else if (grepl(x = data[i,]$fullTime, pattern = "^[0-9]+:[0-9]+$") 
                   & (data[i,]$name %in% nameLabels)) {
            # this row is valid message data
            
            if(!skipHeader) {
                # it means there is the date data, so combine them
                data[i,]$fullTime <- paste(currentDate, data[i,]$fullTime, 
                                           collapse = " ")
                validCursor <- i
            } else {
                # there is no date info, so skip the message data
                rmRow <- append(rmRow, i)
            }
        } else {
            # garbage data or multi-line content
            if(!skipHeader) {
                # multi-line content, combine with the content of validCusor
                data[validCursor,]$content <-
                    paste(data[validCursor,]$content, data[i,]$time, 
                          data[i,]$name, data[i,]$content, collapse = " ")
            }
            rmRow <- append(rmRow, i)
        }
    }
    # remove useless rows and cols
    data <- data[-rmRow,]
    row.names(data) <- seq_len(nrow(data))
    data
}

generateExtraInfos <- function(data) {
    # count the number of content
    # be careful, sometimes the encoding exception will happen
    data$words <- nchar(data$content)
    
    # the format of fullTime is "2014/05/17 22:46"
    data$year <- factor(substr(data$fullTime, start = 1, stop = 4))
    data$month <- factor(substr(data$fullTime, start = 6, stop = 7))
    data$day <- factor(substr(data$fullTime, start = 9, stop = 10))
    data$hour <- factor(substr(data$fullTime, nchar(data$fullTime) - 4, 
                               nchar(data$fullTime) - 3))
    data$minute <- factor(substr(data$fullTime, nchar(data$fullTime) - 1, 
                                 nchar(data$fullTime)))
    data$weekday <- factor(weekdays(as.Date(data$fullTime, "%Y/%m/%d %H:%M")))
    
    # reduce data size
    data$name <- factor(data$name)
    
    # return data
    data[, c(4, 2, 3, 5, 6:11)]
}

example <- function() {
    # a simple example
    
    # Mark Lee and Mr.Lee are the same person
    readAndCleanLineData("data/test.txt", c("Mark Lee", "Mr.Lee"), "MiKu")
}
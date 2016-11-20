#### Loading Packages ####
library(rjson)

#### Unzipping Slack Data ####
data.dir <- 'data'
if (!file.exists(data.dir))  dir.create(data.dir)
unzip('Polar Slack export Jun 28 2016.zip', exdir = data.dir)

#### Loading Slack Data ####
# Select channels and dates to load
load.channels <- list.files(data.dir)[!grepl('\\.', list.files(data.dir))]
load.start.date <- as.Date('2015-07-01')
load.end.date <- as.Date('2016-06-29')

# Loading messages
load.dates <- seq(load.start.date, load.end.date, by = 'day')
msgs.list <- NULL
for (c in 1:length(load.channels)) {  # looping through channels
    print(paste('Loading:', load.channels[c]))
    load.channel.folder <- paste0(data.dir, '/', load.channels[c], '/')

    for (d in 1:length(load.dates)) {  # looping through dates (files)
        date.file <- paste0(load.dates[d], '.json')
        
        if (date.file %in% list.files(load.channel.folder)) {  # if file exists
            msgs.file <- fromJSON(
                file = paste0(load.channel.folder, date.file))
            msgs.file <- lapply(msgs.file, function (x) {
                x$channel <- load.channels[c]
                x
            })
            msgs.list <- c(msgs.list, msgs.file)
        }
    }
}

# Loading user info
users.list <- fromJSON(file = paste(data.dir, 'users.json', sep = '/'))

#### Loading Supplementary Data ####
# Loading proportion of messages by channel type
msgs.ch.type <- read.csv('messages_by_channel_type.csv', stringsAsFactors = FALSE)

# Loading additional user info
add.user.info <- read.csv('additional_user_info.csv', stringsAsFactors = FALSE)

# Loading holiday info
holidays <- read.csv('holidays.csv', stringsAsFactors = FALSE)

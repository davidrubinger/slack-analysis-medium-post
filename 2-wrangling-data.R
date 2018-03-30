#### Loading Slack Data and Packages ####
source('1-loading-data.R')
library(dplyr)
library(tidyr)
library(lubridate)
library(bizdays)
library(stringr)
library(tm)
library(DescTools)

#### Defining Data Wrangling Functions ####
# Function to extract recurring elements of a list
ExtractElement <- function (data.list, element) {
    # Inserting NAs for missing items of element
    data.list <- lapply(data.list, function (x) {
        if (is.null(x[[element]]))  x[[element]] <- NA
        x
    })
    # Extracting element
    element.list <- lapply(data.list, function (x)  x[[element]])
    element.list  # outputting
}

# Function to extract recurring elements of a reaction list
ExtractReactionElement <- function (reactions.list, element) {
    element.list <- lapply(reactions.list, function (x)
        ifelse(is.na(x), NA, ExtractElement(x, element)))
    element.list
}

# Function to format names in consistent way
FormatNames <- function (names) {
    tolower(stripWhitespace(names))
}

#### Wrangling User Data ####
users <- data.frame(
    user_id = users.list$id,
    user_name = users.list$name,
    is_bot = users.list$is_bot,
    email = users.list$profile$email,
    stringsAsFactors = FALSE) %>%
    mutate(is_bot = ifelse(is.na(is_bot), FALSE, is_bot),
           is_polar_employee = grepl('polar', email))
add.user.info <- add.user.info %>%
    mutate(full_name = FormatNames(paste(first_name, last_name)))
    
#### Wrangling Message Data ####
# Setting parameters
drop.subtypes <- c(
    'bot_add', 'bot_disable', 'bot_enable', 'bot_remove', 'channel_archive',
    'channel_join', 'channel_leave', 'channel_name', 'channel_purpose',
    'channel_topic', 'channel_unarchive', 'file_mention', 'me_message',
    'pinned_item', 'reminder_add', 'sh_room_created')
bots <- c('USLACKBOT')
biz.day.start.hour <- 9
biz.day.end.hour <- 17

# Messages
msgs <- data.frame(
    dt = unlist(ExtractElement(msgs.list, 'ts')),
    channel = unlist(ExtractElement(msgs.list, 'channel')),
    user_id = unlist(ExtractElement(msgs.list, 'user')),
    bot_id = unlist(ExtractElement(msgs.list, 'bot_id')),
    bot_message_user_name = unlist(ExtractElement(msgs.list, 'username')),
    text = unlist(ExtractElement(msgs.list, 'text')),
    subtype = unlist(ExtractElement(msgs.list, 'subtype')),
    stringsAsFactors = FALSE) %>%
    mutate(bot_message_user_name = FormatNames(bot_message_user_name)) %>%
    left_join(select(distinct(add.user.info, user_id, .keep_all = TRUE), user_id, full_name),
              c('bot_message_user_name' = 'full_name')) %>%
    replace_na(list(subtype = '')) %>%
    mutate(
        user_id = ifelse(!is.na(user_id.x), user_id.x, user_id.y),
        user_id = ifelse(
            subtype == 'file_comment',
            gsub('<|@|\\|', '', str_extract(text, '^<@.*?\\|')), user_id)) %>%
    select(-user_id.x, -user_id.y) %>%
    left_join(distinct(users, user_id), 'user_id') %>%
    left_join(select(distinct(add.user.info, user_id, .keep_all = TRUE), -user_name), 'user_id') %>%
    filter(!(
        subtype %in% drop.subtypes | user_id %in% bots | is_polar_employee == FALSE |
            (!(bot_message_user_name %in% unique(add.user.info$full_name)) &
                 subtype == 'bot_message'))) %>%
    mutate(dt = as.numeric(gsub('\\..*', '', dt)),
           dt_tor = as.POSIXct(dt, tz = 'America/Toronto', origin = '1970-01-01'),
           dt_lon = as.POSIXct(dt, tz = 'Europe/London', origin = '1970-01-01'),
           dt_syd = as.POSIXct(dt, tz = 'Australia/Sydney', origin = '1970-01-01'),
           date_tor = as.Date(dt_tor, tz = 'America/Toronto'),
           date_lon = as.Date(dt_lon, tz = 'Europe/London'),
           date_syd = as.Date(dt_syd, tz = 'Australia/Sydney'),
           location = ifelse(user_id == 'U0311GF9E' & date_tor < '2015-10-15',
                             'toronto', location),
           location = ifelse(user_id == 'U035AKULP' & date_tor < '2015-11-12',
                             'toronto', location),
           is_biz_day_tor = is.bizday(date_tor, Calendar(
               holidays[holidays$location %in% c('global', 'toronto'), 'date'],
               load.start.date, load.end.date, weekdays = c('saturday', 'sunday'))),
           is_biz_day_nyc = is.bizday(date_tor, Calendar(
               holidays[holidays$location %in% c('global', 'new_york'), 'date'],
               load.start.date, load.end.date, weekdays = c('saturday', 'sunday'))),
           is_biz_day_lon = is.bizday(date_lon, Calendar(
               holidays[holidays$location %in% c('global', 'uk', 'london'), 'date'],
               load.start.date, load.end.date, weekdays = c('saturday', 'sunday'))),
           is_biz_day_gla = is.bizday(date_lon, Calendar(
               holidays[holidays$location %in% c('global', 'uk', 'glasgow'), 'date'],
               load.start.date, load.end.date, weekdays = c('saturday', 'sunday'))),
           is_biz_day_syd = is.bizday(date_syd, Calendar(
               holidays[holidays$location %in% c('global', 'sydney'), 'date'],
               load.start.date, load.end.date, weekdays = c('saturday', 'sunday'))),
           local_hr = ifelse(
               location %in% c('toronto', 'new_york', 'washington'),
               hour(dt_tor), ifelse(
                   location %in% c('london', 'glasgow'), hour(dt_lon), ifelse(
                       location == 'sydney', hour(dt_syd), NA))),
           is_biz_hour_local = ifelse(
               local_hr < biz.day.start.hour | local_hr >= biz.day.end.hour,
               FALSE, ifelse(location == 'toronto' & is_biz_day_tor, TRUE, ifelse(
                   location %in% c('new_york', 'washington') & is_biz_day_nyc,
                   TRUE, ifelse(location == 'london' & is_biz_day_lon, TRUE, ifelse(
                       location == 'glasgow' & is_biz_day_gla, TRUE, ifelse(
                           location == 'sydney' & is_biz_day_syd, TRUE, FALSE)))))),
           text = ifelse(subtype == 'file_comment',
                         gsub('^<@.*?> commented on.*>:', '', text), text),
           text = ifelse(
               subtype == 'file_share',
               gsub('^<@.*?> (uploaded|shared) a file: <.*?>| and commented: ',
                    '', text), text),
           n_words = StrCountW(text))

# Reactions given
reactions.given <- data.frame(
    user_id = unlist(ExtractReactionElement(ExtractElement(
        msgs.list, 'reactions'), 'users')), stringsAsFactors = FALSE) %>%
    filter(!is.na(user_id)) %>%
    group_by(user_id) %>%
    summarize(n_reactions_given = n())

#### Aggregating by User####
# By user
msgs.user <- msgs %>%
    group_by(user_id, user_name, gender, is_engineer) %>%
    summarize(n_msgs = n(), avg_n_words = median(n_words, na.rm = TRUE)) %>%
    left_join(reactions.given, 'user_id') %>%
    replace_na(list(n_reactions_given = 0))

# By user and location - for those who have worked in multiple locations
msgs.user.loc <- msgs %>%
    group_by(user_id, user_name, location) %>%
    summarize(n_msgs = n(), n_msgs_biz_hours = sum(is_biz_hour_local)) %>%
    mutate(prop_msgs_biz_hours = n_msgs_biz_hours / n_msgs,
           region = ifelse(location %in% c('toronto', 'new_york', 'washington'),
                           'Canada/US', ifelse(
                               location %in% c('london', 'glasgow'), 'UK',
                               ifelse(location == 'sydney', 'Australia', 'NA'))))

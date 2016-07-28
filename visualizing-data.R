#### Loading Slack Data and Packages ####
source('wrangling-data.R')
library(ggplot2)

#### Visualizing Data ####
plots.dir <- 'plots'
if (!file.exists(plots.dir))  dir.create(plots.dir)
theme_set(theme_bw())

# Messages by channel type
msgs.ch.type$channel_type <- factor(
    msgs.ch.type$channel_type,
    levels = c('public_channel', 'private_channel', 'direct_message'),
    labels = c('Public Channels', 'Private Channels', 'Direct Messages'))
col.palette <- c('lightskyblue', 'deepskyblue4')
ggplot(
    msgs.ch.type,
    aes(channel_type, proportion*100, fill = channel_type == 'Public Channels')) +
    geom_bar(stat = 'identity', width = 0.5) +
    scale_fill_manual(values = col.palette) +
    lims(y = c(0, 100)) +
    labs(x = '', y = '%',
         title = '% of Messages Sent by Channel Type') +
    guides(fill = FALSE)
ggsave(filename = paste0(plots.dir, '/plot-msgs-channel-type.png'), width = 6,
       height = 4, units = 'in', scale = 1.2)

# Reactions given by gender
top.n <- 20
msgs.user$user_name <- factor(
    msgs.user$user_name,
    levels = arrange(ungroup(msgs.user), n_reactions_given)$user_name)
ggplot(
    msgs.user %>% top_n(top.n, n_reactions_given),
    aes(user_name, n_reactions_given, fill = gender)) +
    geom_bar(stat = 'identity') +
    coord_flip() +
    scale_fill_manual(values = col.palette, name = '',
                      labels = c('Female', 'Male')) +
    labs(x = 'Individual', y = 'Reactions',
         title = paste('Top', top.n, 'Individuals by Reactions Given')) +
    theme(axis.text.y = element_blank())
ggsave(filename = paste0(plots.dir, '/plot-reactions-gender.png'), width = 6,
       height = 4, units = 'in', scale = 1.2)

# Number of messages and average words by dept
min.n.msgs.user <- 100
ggplot(
    msgs.user %>% filter(n_msgs >= min.n.msgs.user),
    aes(n_msgs, avg_n_words, colour = is_engineer)) +
    geom_point(size = 3) +
    scale_colour_manual(values = col.palette, name = '',
                        labels = c('Non-engineer', 'Engineer')) +
    lims(x = c(0, 4000)) +
    labs(x = 'Messages', y = 'Words per Message',
         title = 'Messages Sent and Average Words per Message by Individual')
ggsave(filename = paste0(plots.dir, '/plot-msgs-words.png'), width = 6,
       height = 4, units = 'in', scale = 1.2)

# Proportion of messages sent during business
min.n.msgs.user <- 100
msgs.user.loc$region <- factor(
    msgs.user.loc$region,
    levels = c('Canada/US', 'UK', 'Australia'),
    labels = c('Canada/US\n(Eastern Time)', 'UK', 'Australia'))
col.palette <- c('lightskyblue1', 'deepskyblue2', 'deepskyblue4')
ggplot(
    msgs.user.loc %>% filter(n_msgs >= min.n.msgs.user),
    aes(prop_msgs_biz_hours*100, fill = region)) +
    geom_histogram(binwidth = 5, colour = 'white') +
    scale_fill_manual(values = col.palette, name = 'Region') +
    scale_y_continuous(breaks = seq(0, 10, by = 2)) +
    labs(x = '%', y = 'Number of Individuals',
         title = 'Frequency of Individuals by % of Messages Sent\nDuring Business Hours')
ggsave(filename = paste0(plots.dir, '/plot-msgs-biz-hours.png'), width = 6,
       height = 4, units = 'in', scale = 1.2)

#### Other Statistics Used in Post ####
# Total messages
nrow(msgs)

# Average proportion of messages sent during business hours
median(filter(msgs.user.loc, n_msgs >= min.n.msgs.user,
              region == 'Canada/US\n(Eastern Time)')$prop_msgs_biz_hours)
median(filter(msgs.user.loc, n_msgs >= min.n.msgs.user,
              region %in% c('UK', 'Australia'))$prop_msgs_biz_hours)

# Differences for employees that moved from Toronto
filter(msgs.user.loc, user_id == 'U0311GF9E' & location == 'toronto')$prop_msgs_biz_hours -
    filter(msgs.user.loc, user_id == 'U0311GF9E' & location != 'toronto')$prop_msgs_biz_hours
filter(msgs.user.loc, user_id == 'U035AKULP' & location == 'toronto')$prop_msgs_biz_hours -
    filter(msgs.user.loc, user_id == 'U035AKULP' & location != 'toronto')$prop_msgs_biz_hours

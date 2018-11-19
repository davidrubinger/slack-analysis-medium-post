# Analysis Behind Medium Post

Related post:
[4 Things I Learned About Workplace Communication from Slack](https://medium.com/@david.rubinger/4-things-i-learned-about-workplace-communication-from-slack-81329a83ceba)

### Getting Data

###### Slack

- Obtain zip file of public messages from Slack through your team administrator and save it in your working directory. The file will be called something like `Polar Slack export Jun 28 2016.zip`
- Breakdown of messages by channel type comes from your team's Slack statistics page

###### Other

- Create CSV of relevant holidays and dates
- Create CSV of additional user info containing features like gender, department, and time zone

### Analyzing Data

- Once you've got all the raw data in your working directory, run the `visualizing-data.R` script. That will output all plots to the folder `plots` in your working directory



library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(ggplot2)

## Load in the massive ratings files but filter for Election Day

# Define a vector with paths to all rating files
ratings_urls <- c(
  "https://ton.twimg.com/birdwatch-public-data/2024/11/08/noteRatings/ratings-00000.tsv",
  "https://ton.twimg.com/birdwatch-public-data/2024/11/08/noteRatings/ratings-00001.tsv",
  "https://ton.twimg.com/birdwatch-public-data/2024/11/08/noteRatings/ratings-00002.tsv",
  "https://ton.twimg.com/birdwatch-public-data/2024/11/08/noteRatings/ratings-00003.tsv",
  "https://ton.twimg.com/birdwatch-public-data/2024/11/08/noteRatings/ratings-00004.tsv",
  "https://ton.twimg.com/birdwatch-public-data/2024/11/08/noteRatings/ratings-00005.tsv",
  "https://ton.twimg.com/birdwatch-public-data/2024/11/08/noteRatings/ratings-00006.tsv",
  "https://ton.twimg.com/birdwatch-public-data/2024/11/08/noteRatings/ratings-00007.tsv",
  "https://ton.twimg.com/birdwatch-public-data/2024/11/08/noteRatings/ratings-00008.tsv",
  "https://ton.twimg.com/birdwatch-public-data/2024/11/08/noteRatings/ratings-00009.tsv",
  "https://ton.twimg.com/birdwatch-public-data/2024/11/08/noteRatings/ratings-00010.tsv",
  "https://ton.twimg.com/birdwatch-public-data/2024/11/08/noteRatings/ratings-00011.tsv",
  "https://ton.twimg.com/birdwatch-public-data/2024/11/08/noteRatings/ratings-00012.tsv",
  "https://ton.twimg.com/birdwatch-public-data/2024/11/08/noteRatings/ratings-00013.tsv",
  "https://ton.twimg.com/birdwatch-public-data/2024/11/08/noteRatings/ratings-00014.tsv",
  "https://ton.twimg.com/birdwatch-public-data/2024/11/08/noteRatings/ratings-00015.tsv"
)

# Define the target date start and end in EST (November 5, 2024 EST)
start_time_est <- as.POSIXct("2024-11-05 00:00:00", tz = "America/New_York")
end_time_est <- as.POSIXct("2024-11-05 23:59:59", tz = "America/New_York")

# Convert start and end time to milliseconds since epoch (UTC)
start_time_utc_ms <- as.numeric(start_time_est) * 1000
end_time_utc_ms <- as.numeric(end_time_est) * 1000

# Initialize an empty list to store data from November 5
election_day_data <- list()

# Process each URL
for (url in ratings_urls) {
  # Read in data from the URL in chunks using fread
  data <- fread(url)
  
  # Filter for rows within the specified UTC milliseconds range
  election_day <- data[createdAtMillis >= start_time_utc_ms & createdAtMillis <= end_time_utc_ms]
  
  # Append the filtered data to our list
  election_day_data[[url]] <- election_day
}

# Combine all election day data into one data frame
combined_election_day_data <- rbindlist(election_day_data, fill = TRUE)

# Display the first few rows of the combined data
print(head(combined_election_day_data))
# Now we have all the ratings for the specific date
# Now let's bring in the notes and status history

# Load the notes and note status history data
# Load the notes data and filter based on the milliseconds range
notes_data <- fread("https://ton.twimg.com/birdwatch-public-data/2024/11/08/notes/notes-00000.tsv") %>% 
  filter(createdAtMillis >= start_time_utc_ms & createdAtMillis <= end_time_utc_ms) %>% # filter by date
  mutate(tweet_url = paste0("https://twitter.com/i/web/status/", tweetId)) # create a link to easily access the tweet

note_status_data <- fread("https://ton.twimg.com/birdwatch-public-data/2024/11/08/noteStatusHistory/noteStatusHistory-00000.tsv") %>% 
  filter(createdAtMillis >= start_time_utc_ms & createdAtMillis <= end_time_utc_ms) 

notes_selected <- notes_data %>%
  select(noteId, summary, tweetId, tweet_url)

# Perform a left join with note_status_data to get everything from the status dataset
combined_data <- note_status_data %>%
  left_join(notes_selected, by = "noteId")

# Let's consider notes with lots of votes but did not meet the threshold

# Rename election day to something we understand
ratings_data <- combined_election_day_data

# Before we go further let's export everything

write_csv(ratings_data,"ratings_data.csv")
write_csv(combined_data, "notes_and_status.csv")

# Summarize ratings data to get counts of "helpful", "not helpful", and total ratings for each noteId
ratings_summary <- ratings_data %>%
  group_by(noteId) %>%
  summarize(
    helpful_count = sum(helpfulnessLevel == "HELPFUL", na.rm = TRUE),
    not_helpful_count = sum(helpfulnessLevel == "NOT_HELPFUL", na.rm = TRUE),
    total_ratings = n()  # Count of all ratings
  ) %>%
  ungroup()

# Join the summarized ratings back to the combined_data
combined_data <- combined_data %>%
  left_join(ratings_summary, by = "noteId")
  
notes_status_ratings <- combined_data %>%   
  arrange(desc(total_ratings))

# Export our final data set

write_csv(notes_status_ratings, "all_cn_data.csv")

# How many helpful notes were there on election day?

helpful_percentage <- notes_status_ratings %>%
  summarize(
    total_notes = n(),
    helpful_notes = sum(currentStatus == "CURRENTLY_RATED_HELPFUL", na.rm = TRUE)
  ) %>%
  mutate(helpful_percentage = (helpful_notes / total_notes) * 100)

# Display the result
print(helpful_percentage$helpful_percentage)

# Get helpful notes

helpful_notes <- notes_status_ratings %>% 
  filter(currentStatus == "CURRENTLY_RATED_HELPFUL")

# Look for notes that have lots of ratings but have not become public

# Define a threshold for "lots of helpful ratings"
helpful_threshold <- 50  # Example: at least 10 helpful ratings

# Filter notes that have lots of helpful ratings but are not currently rated as helpful
notes_with_lots_of_helpful_not_marked_helpful <- combined_data %>%
  filter(helpful_count >= helpful_threshold & currentStatus != "CURRENTLY_RATED_HELPFUL") %>% 
  arrange(desc(helpful_count))

# Display the filtered results
print(notes_with_lots_of_helpful_not_marked_helpful)

# Let's look for coordinated behavior

# Convert createdAtMillis to POSIXct format for date-time manipulation
ratings_data <- combined_election_day_data %>%
  mutate(ratingDateTime = as.POSIXct(createdAtMillis / 1000, origin = "1970-01-01"))

# Define a shorter time window for grouping
time_window <- 10  # in minutes

#  Group by noteId and time window, and count "helpful" and "not helpful" ratings
coordinated_behavior_data <- ratings_data %>%
  mutate(time_bin = floor_date(ratingDateTime, unit = "10 minutes")) %>%
  group_by(noteId, time_bin) %>%
  summarize(
    helpful_count = sum(helpfulnessLevel == "HELPFUL"),
    not_helpful_count = sum(helpfulnessLevel == "NOT_HELPFUL")
  ) %>%
  ungroup()

# Define thresholds for flagging suspicious activity in a time window
threshold_helpful_in_window <- 10  
threshold_not_helpful_in_window <- 10  

# Filter for notes with suspicious activity based on thresholds
suspicious_helpful_activity <- coordinated_behavior_data %>%
  filter(helpful_count >= threshold_helpful_in_window)

suspicious_not_helpful_activity <- coordinated_behavior_data %>%
  filter(not_helpful_count >= threshold_not_helpful_in_window)

# Merge with notes data to get the note summary and tweet ID
suspicious_helpful_activity_with_details <- suspicious_helpful_activity %>%
  left_join(notes_status_ratings, by = "noteId")

suspicious_not_helpful_activity_with_details <- suspicious_not_helpful_activity %>%
  left_join(notes_data %>% select(noteId, summary, tweetId), by = "noteId")

# Let's plot some weird notes

note_ids_to_plot <- c("1853897015752864227", "1853893990174892471")  # Replace with actual IDs

# Filter data for the specified notes
filtered_data <- combined_election_day_data %>%
  filter(noteId %in% note_ids_to_plot)

# Convert createdAtMillis to POSIXct format for date-time manipulation if not already done
filtered_data <- filtered_data %>%
  mutate(ratingDateTime = as.POSIXct(createdAtMillis / 1000, origin = "1970-01-01"))

# Create binary indicators for "helpful" and "not helpful" based on helpfulnessLevel
filtered_data <- filtered_data %>%
  mutate(
    helpful = ifelse(helpfulnessLevel == "HELPFUL", 1, 0),
    not_helpful = ifelse(helpfulnessLevel == "NOT_HELPFUL", 1, 0)
  )

# Calculate cumulative counts of "helpful" and "not helpful" ratings over time for each note
cumulative_data <- filtered_data %>%
  arrange(noteId, ratingDateTime) %>%
  group_by(noteId) %>%
  mutate(
    helpful_cumulative = cumsum(helpful),
    not_helpful_cumulative = cumsum(not_helpful)
  ) %>%
  ungroup()

# Let's plot a few notes that have suspicious ratings bursts

ggplot(cumulative_data, aes(x = ratingDateTime)) +
  # Lines and shaded areas for helpful and not helpful ratings
  geom_line(aes(y = helpful_cumulative, color = "Helpful"), size = 1.2) +
  geom_line(aes(y = not_helpful_cumulative, color = "Not Helpful"), size = 1.2) +
  geom_area(aes(y = helpful_cumulative, fill = "Helpful"), alpha = 0.3) +
  geom_area(aes(y = not_helpful_cumulative, fill = "Not Helpful"), alpha = 0.3) +
  
  # Custom colors for the lines and shaded areas using Teal and Coral
  scale_color_manual(values = c("Helpful" = "#00796B", "Not Helpful" = "#FF6F61")) +
  scale_fill_manual(values = c("Helpful" = "#00796B", "Not Helpful" = "#FF6F61")) +
  
  # Title, subtitle, and axis labels with styling
  labs(
    title = "Cumulative Ratings Over Time for Community Notes",
    subtitle = "Tracking helpful and not helpful ratings to identify patterns",
    x = "Date/Time (UTC)",
    y = "Cumulative Ratings",
    color = "Ratings Type",
    fill = "Ratings Type"
  ) +
  
  # Facet wrap by noteId to see each note's ratings individually
  facet_wrap(~ noteId, scales = "free_y") +
  
  # Theme for a cleaner, more modern look
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 12),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "gray85"),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 14, face = "bold")
  ) +
  
  # Customize the x-axis for clear date and time formatting
  scale_x_datetime(date_labels = "%Y-%m-%d %H:%M", date_breaks = "3 hours")

# Those actually don't look to bad!

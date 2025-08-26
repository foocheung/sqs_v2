library(ggplot2)

# Data: convert minutes+seconds into seconds
df <- data.frame(
  Plates = c(2, 4, 6, 15),
  Duration_sec = c(
    18.49,
    20.21,
    43.88,
    60 + 53.2  # 1 min 53.2s = 113.2 sec
  )
)

# Add a label for pretty printing
df$Label <- paste0(df$Duration_sec, "s")

# Plot
ggplot(df, aes(x = Plates, y = Duration_sec)) +
  geom_point(size = 4, color = "steelblue") +
  geom_line(color = "steelblue") +
  geom_text(aes(label = Label), vjust = -1) +
  labs(
    title = "Time for Report Generated",
    subtitle = "Mac laptop • M1 chip • 64 GB • macOS 14.7.6",
    x = "Number of Plates",
    y = "Duration (seconds)"
  ) +
  theme_minimal(base_size = 14)


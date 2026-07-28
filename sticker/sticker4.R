# Automatically verify, install, and update key mapping and visual dependencies
required_packages <- c("ggplot2", "sf", "mapSpain", "hexSticker", "shadowtext", "showtext", "sysfonts")

# Safe helper to handle package checks and installations
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    # Ensure the modern curl transport layer is updated to prevent namespace errors
    if (pkg == "mapSpain" && !requireNamespace("curl", quietly = TRUE)) {
      install.packages("curl", repos = "https://cloud.r-project.org")
    }
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

library(ggplot2)
library(sf)
library(mapSpain)
library(hexSticker)
library(shadowtext)

# Set up clean typography rendering with graceful fallbacks
font_family <- "sans"
if (requireNamespace("showtext", quietly = TRUE)) {
  tryCatch({
    library(showtext)
    # Register "Fredoka" - a friendly, geometric rounded sans-serif matching the original design
    sysfonts::font_add_google("Fredoka", "fredoka")
    showtext_auto()
    font_family <- "fredoka"
  }, error = function(e) {
    message("Could not load Google Font. Falling back to default bold system sans-serif.")
  })
}

# Define custom displacement coordinates to shift the Canary Islands to the bottom-right, just under the Balearic Islands
# Natural Canary center: ~(-15.5, 28.5) -> Shifted to: ~(3.0, 35.0)
# This requires a horizontal offset of +18.5 and vertical offset of +6.5
displace_offset <- c(13.6, 2.5)

# Fetch provincial boundaries with the custom Canary displacement vector
spain_provinces <- mapSpain::esp_get_prov(moveCAN = displace_offset)

# Resolve the RStudio IDE trailing-comma bracket subsetting parser bug using built-in subset()
# We exclude the North African autonomous cities of Ceuta and Melilla to keep the map clean
spain_provinces <- subset(spain_provinces, !cpro %in% c("51", "52"))

# Dissolve internal borders using st_union to get clean, modern mainland and island silhouettes
spain_mainland <- sf::st_union(spain_provinces)

# Retrieve the official Canary Islands subset bounding box and displace it using the exact same coordinates
# "left" style creates the classic 3-sided chamfered divider line (top, diagonal, and left) matching indemogR_2.png
can_box <- mapSpain::esp_get_can_box(style = "left", moveCAN = displace_offset)

# Generate a clean, mathematically matched dataset representing 10 traditional age cohorts
set.seed(42)
pyramid_data <- data.frame(
  Age = factor(rep(1:10, 2), labels = paste0(seq(0, 90, by = 10), "-", seq(9, 99, by = 10))),
  Sex = rep(c("M", "F"), each = 10),
  Population = c(
    -25, -34, -42, -50, -45, -38, -30, -22, -15, -8,  # Left side (Males)
    24,  33,  41,  49,  46,  39,  31,  23,  16,  9   # Right side (Females)
  )
)

# Construct the demographic overlay inside a separate minimal plot coordinate space
g_pyramid <- ggplot(pyramid_data, aes(x = Age, y = Population, fill = Sex)) +
  geom_col(width = 0.82, show.legend = FALSE) +
  coord_flip() + # Locks the population pyramid in its classic horizontal orientation
  scale_fill_manual(values = c("M" = "#0D9488", "F" = "#D97706")) + # Emerald-Teal and Deep Rust-Orange
  theme_void() +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.margin = margin(0, 0, 0, 0)
  )

# Build the core vector graphic combining Spain's map, the Canary box, the pyramid, and the outlined title
g_map <- ggplot() +
  # Draw dissolved peninsular Spain and islands with soft warm colors
  geom_sf(data = spain_mainland, fill = "#FFFBEB", color = "#D97706", linewidth = 0.15) +
  
  # Draw the Canary Islands subset frame at its custom bottom-right location
  geom_sf(data = can_box, color = "#D97706", linewidth = 0.25, linetype = "solid") +
  
  # Overlay the population pyramid centered over the inland Madrid plateau
  annotation_custom(
    grob = ggplotGrob(g_pyramid),
    xmin = -6.2, xmax = -0.7,
    ymin = 37.9, ymax = 42.3
  ) +
  
  # Inject the bold package title using high-contrast white fill with a deep-blue outline
  # Midpoint of X-axis limits is mathematically at -2.1, ensuring perfect centering
  annotate(
    geom = "shadowtext",
    x = -2.3, y = 44.5,
    label = "inedemogR",
    family = font_family,
    fontface = "plain",
    size = 30,
    color = "#FFFFFF",
    bg.color = "#1F4E79", # Sleek corporate blue outline matching indemogR.png
    bg.r = 0.06 # Border outline radius
  ) +
  
  # Tighten coordinate limits around the new peninsular and shifted island boundaries
  coord_sf(
    xlim = c(-10.3, 4.4), 
    ylim = c(34.8, 45.1),
    expand = FALSE
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.margin = margin(0, 0, 0, 0)
  )

# Compile and write the professional custom sticker to disk as 'inedemogR_sticker.png'
hexSticker::sticker(
  subplot = g_map,
  package = "", # Package title drawn directly on the map layer to prevent clipping
  s_x = 0.95, 
  s_y = 0.92,
  s_width = 1.63, # Expanded map size to comfortably fill the hexagon container
  s_height = 1.63,
  h_fill = "#EFF6FF", # Light, ice-blue background matches the reference logo's modern palette
  h_color = "#2c3e50", # Bold, primary blue border outline
  h_size = 1.9,
  filename = "inedemogR_sticker.png",
  dpi = 600
)

message("Success! The fine-tuned hex logo has been generated and saved as 'inedemogR_sticker.png'.")

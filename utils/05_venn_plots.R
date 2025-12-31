library(stringr)

showtext_auto()

# Define all columns and their groups
all_columns <- tibble(
  group = rep(c("iv", "hfo", "ppe"), each = 10),
  term = c(paste0("iv3c_", 1:10), paste0("hfo2c_", 1:10), paste0("ppe1c_", 1:10))
) %>%
  mutate(
    item = as.integer(str_extract(term, "(?<=_)\\d+"))
  )

# Get pretty labels for all terms
all_columns <- all_columns %>%
  mutate(
    pretty_label = sapply(term, function(col) {
      label <- attr(survey_results[[col]], "label")
      paste(strwrap(label, width = 30), collapse = "\n")
    })
  )

# Define custom replacements
custom_labels <- c(
  NA, # 1: no change
  "Hospital or organization policy", # 2
  "Colleagues at my institution",    # 3
  "Colleagues at other institutions",# 4
  "ED medical leadership guidance",  # 5
  "ED nursing leadership guidance",  # 6
  NA, # 7: no change
  "Information from social media, online", # 8
  "Anecdotes about patient outcomes", # 9
  NA  # 10: no change
)

## Create indicators ----------------------------------------------------------
sets_df <- survey_results %>%
  transmute(
    PPE1 = (ppe1a == 1 & ppe1b %in% c(2,3)),
    PPE0 = (ppe1a == 0 & ppe1b %in% c(2,3)),
    HFO1 = (hfo2a == 1 & hfo2b %in% c(2,3)),
    HFO0 = (hfo2a == 0 & hfo2b %in% c(2,3)),
    IV1  = (iv3a  == 1 & iv3b  %in% c(2,3)),
    IV0  = (iv3a  == 0 & iv3b  %in% c(2,3)),
    PPE_any = (ppe1a %in% c(0,1) & ppe1b %in% c(2,3)),
    HFO_any = (hfo2a %in% c(0,1) & hfo2b %in% c(2,3)),
    IV_any  = (iv3a  %in% c(0,1) & iv3b  %in% c(2,3))
  ) %>%
  mutate(across(everything(), ~replace_na(., FALSE)))

## --- DEFINE venn_list (missing before) ---
## Use whichever trio you want to plot:
## For “answered 1”:
venn_list <- list(
  PPE = which(sets_df$PPE1),
  HFO = which(sets_df$HFO1),
  IV  = which(sets_df$IV1)
)
## (If you want “answered 0”, swap to PPE0/HFO0/IV0; for “any”, use PPE_any/HFO_any/IV_any.)

## --- Helpers you already have (unchanged except they need sf/tidyr loaded) ---
get_xy <- function(df) {
  if ("geometry" %in% names(df)) {
    coords <- sf::st_coordinates(df)
    out <- cbind(sf::st_drop_geometry(df), x = coords[,1], y = coords[,2])
    as.data.frame(out)
  } else {
    df <- as.data.frame(df)
    xcols <- intersect(names(df), c("x","X","x1","label_x","xc","center.x","geom_x"))
    ycols <- intersect(names(df), c("y","Y","y1","label_y","yc","center.y","geom_y"))
    stopifnot(length(xcols) > 0, length(ycols) > 0)
    df$x <- df[[xcols[1]]]
    df$y <- df[[ycols[1]]]
    df
  }
}
get_set_df <- function(venn_list) {
  vd  <- ggVennDiagram::Venn(venn_list)
  dat <- ggVennDiagram::process_data(vd)
  sl  <- get_xy(dat$setLabel)
  label_col <- intersect(names(sl), c("name","label","set","Set","text"))[1]
  stopifnot(length(label_col) > 0)
  sl %>%
    transmute(x, y, label = .data[[label_col]]) %>%
    distinct(label, .keep_all = TRUE)
}
get_cnt_df <- function(venn_list) {
  vd  <- ggVennDiagram::Venn(venn_list)
  dat <- ggVennDiagram::process_data(vd)
  rl  <- get_xy(dat$regionLabel)
  count_col <- intersect(names(rl), c("count","value","n"))[1]
  stopifnot(length(count_col) > 0)
  rl$count <- rl[[count_col]]
  total_n <- sum(rl$count, na.rm = TRUE)
  rl$label_txt <- if (is.na(total_n) || total_n == 0) {
    as.character(rl$count)
  } else {
    sprintf("%d (%.1f%%)", rl$count, 100 * rl$count / total_n)  # one decimal place
  }
  rl[, c("x","y","count","label_txt")]
}
strip_text_layers <- function(p) {
  p$layers <- Filter(function(lyr) {
    !inherits(lyr$geom, "GeomText") &&
      !inherits(lyr$geom, "GeomLabel") &&
      !inherits(lyr$geom, "GeomSfText") &&
      !inherits(lyr$geom, "GeomSfLabel")
  }, p$layers)
  p
}

## --- Base shapes (no labels) ---
base_shapes <- ggVennDiagram(
  venn_list,
  label_alpha    = 0,   # hide region labels
  set_label_size = 0    # hide set labels
) +
  scale_fill_gradient(low = "white", high = "#1f77b4") +
  theme_void(base_family = "Nunito") +
  theme(
    legend.position = "none",
    text = element_text(size = 14, family = "Nunito", color = "gray30"),
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_blank()
  )
base_clean <- strip_text_layers(base_shapes)

## --- Build custom text data & (optional) nudge one label ---
set_df <- get_set_df(venn_list) %>%
  mutate(y = ifelse(label == "IV", y - 0.5, y))  # move IV down; tweak value as needed
cnt_df <- get_cnt_df(venn_list)

## --- Compose final plot ---
implementation_venn <- base_clean +
  geom_text(
    data = cnt_df,
    aes(x, y, label = label_txt),
    inherit.aes = FALSE,
    family = "Nunito",
    size = 13,
    color = "gray10"
  ) +
  geom_text(
    data = set_df,
    aes(x, y, label = label),
    inherit.aes = FALSE,
    family = "Nunito",
    fontface = "bold",
    size = 22,
    color = "gray30"
  )

implementation_venn

# ggsave("plots/implementation_venn.png", implementation_venn, width = 6, height = 5, dpi = 300)


# # Count reasons cited for each innovation
# 
# count_reasons <- function(df, prefix, all_columns, custom_labels = NULL, items = 1:10) {
#   cols <- paste0(prefix, items)
#   missing <- setdiff(cols, names(df))
#   if (length(missing)) stop("Missing columns: ", paste(missing, collapse=", "))
#   
#   # Binary 0/1 (NA treated as 0)
#   m <- df[, cols, drop = FALSE]
#   m[] <- lapply(m, function(x) as.integer(!is.na(x) & x == 1))
#   
#   # Summed counts
#   counts <- colSums(m, na.rm = TRUE)
#   
#   out <- tibble(
#     term  = cols,
#     count = counts
#   )
#   
#   # Add labels from all_columns
#   out <- out %>%
#     left_join(all_columns %>% dplyr::select(term, pretty_label, item), by = "term")
#   
#   # Apply custom_labels overrides (by item index)
#   if (!is.null(custom_labels)) {
#     for (i in seq_along(custom_labels)) {
#       if (!is.na(custom_labels[i])) {
#         out$pretty_label[out$item == i] <- custom_labels[i]
#       }
#     }
#   }
#   
#   out %>% 
#     dplyr::select(term, pretty_label, count) %>%
#     arrange(desc(count))
# }
# 
# # Example usage:
# iv_reason_counts  <- count_reasons(survey_results, "iv3c_",  all_columns, custom_labels)
# hfo_reason_counts <- count_reasons(survey_results, "hfo2c_", all_columns, custom_labels)
# ppe_reason_counts <- count_reasons(survey_results, "ppe1c_", all_columns, custom_labels)
# 
# ppe_reason_counts
# hfo_reason_counts
# iv_reason_counts
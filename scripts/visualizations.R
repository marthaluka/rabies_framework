

# Libraries ####

require(pacman)
pacman::p_load(tidyverse,
               patchwork,
               cowplot,
               ggrepel)


params <- read.csv("./data/country_params_MLedited.csv")
country_results <- readRDS("./output/country_results.rds")

# Common definitions ----------------------------------------------------------

scenario_codes <- c("SQ", "PEP", "MDV", "IBCM", "IBCM_MDV")
intervention_codes <- scenario_codes[-1]

scenario_labels <- c("SQ" = "SQ", "PEP" = "Improved PEP", "MDV" = "MDV", "IBCM" = "IBCM", "IBCM_MDV" = "IBCM + MDV")

scenario_colours <- c("SQ" = "#4D4D4D", "Improved PEP" = "#E69F00", "MDV" = "#0072B2", "IBCM" = "#CC79A7", "IBCM + MDV" = "#009E73")

# SQ parameter data used throughout ------------------------------------------

sq <- params |>
  filter(scenario == "SQ") |>
  mutate(pop = parse_number(as.character(pop)),
         hdr_mean = (HDR1 + HDR2) / 2,
         dog_pop = pop / hdr_mean,
         location = paste(country, Administrative_unit, sep = " · "),
         result_id = paste(country, Administrative_unit, sep = "_"),
         result_name = paste(country, Administrative_unit, "SQ", sep = "_"),
         pPEP = pSeek_exposure * pStart_exposure,
         dog_vax_cov = unowned_prop * base_vax_cov_unowned + (1 - unowned_prop) * base_vax_cov_owned)

location_order <- sq$location
sq$location <- factor(sq$location, levels = rev(location_order))

settings <- sq |> transmute(setting = as.character(location), result_id, pop, dog_pop)

# FIGURE 3 ###########
## FIGURE 3AB: SETTING CHARACTERISTICS ###########

# Shared appearance -------------------------------------------------------

point_colour <- "#2A7766"
interval_colour <- "#A8C7BD"
text_colour <- "#17212B"
grid_colour <- "#DCE3E9"

# Low PEP access = purple; high PEP access = green
pep_colours <- c("goldenrod", "lightgoldenrod", "#A6DBA0", "#1B7837")
pep_limits <- range(sq$pPEP, na.rm = TRUE)

pep_colour_scale <- function() {
  scale_colour_gradientn(colours = pep_colours, values = seq(0, 1, length.out = length(pep_colours)), limits = pep_limits, oob = scales::squish, labels = scales::label_percent(accuracy = 1), name = "PEP access")
}

fig3_theme <- theme_classic(base_size = 10) +
  theme(text = element_text(colour = text_colour),
        axis.text = element_text(size = 8, colour = text_colour),
        axis.title = element_text(size = 9, colour = text_colour),
        legend.title = element_text(size = 9, colour = text_colour),
        legend.text = element_text(size = 8, colour = text_colour),
        # legend.key.height = unit(4, "mm"),
        # legend.key.width = unit(9, "mm"),
        plot.title = element_text(face = "bold", size = 10, colour = text_colour, margin = margin(b = 8)),
        plot.tag = element_text(face = "bold", size = 11, colour = text_colour),
        plot.margin = margin(7, 9, 7, 9))

outcome_theme <- fig3_theme +
  theme(panel.grid.major.x = element_line(colour = grid_colour, linewidth = 0.35),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())

plot3A <- ggplot(sq, aes(x = hdr_mean, y = dog_vax_cov, colour = pPEP, shape = rig_avail)) +
  geom_point(size = 2.5, stroke = 0.8) +
  geom_text_repel(aes(label = result_id), colour = text_colour, size = 2.65, show.legend = FALSE, max.overlaps = Inf, seed = 123) +
  scale_x_continuous(labels = scales::label_number(accuracy = 1), expand = expansion(mult = c(0.05, 0.12))) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1), expand = expansion(mult = c(0.05, 0.12))) +
  pep_colour_scale() +
  labs(x = "Human-to-dog ratio", y = "Dog vaccination coverage", shape = "RIG available") +
  fig3_theme

plot3B <- ggplot(sq, aes(x = pPEP, y = bpi_per100k, colour = pPEP, shape = rig_avail)) +
  geom_point(size = 3, stroke = 0.8) +
  geom_text_repel(aes(label = result_id), colour = text_colour, size = 2.65, show.legend = F, max.overlaps = Inf, seed = 123) +
  scale_x_continuous(limits = c(0, 1), labels = scales::label_percent(accuracy = 1), expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_log10(labels = scales::label_comma(), breaks = scales::breaks_log(n = 4), expand = expansion(mult = c(0.08, 0.15))) +
  pep_colour_scale() +
  labs(x = "PEP access after exposure", y = "Bite patients per 100k", shape = "RIG available") +
  theme(legend.position = "bottom")+
  fig3_theme

fig3AB <- plot3A + plot3B +
  plot_layout(ncol = 2, guides = "collect") &
  theme(legend.position = "right")

fig3AB


# # A. Reported bite incidence
# 
# bpi_max <- max(sq$bpi_per100k, na.rm = TRUE)
# 
# p_bpi <- ggplot(sq, aes(x = bpi_per100k, y = location)) +
#   geom_segment(aes(x = 1, xend = bpi_per100k, yend = location), colour = interval_colour, linewidth = 0.55) +
#   geom_point(colour = point_colour, size = 2) +
#   geom_text(aes(label = bpi_label, hjust = bpi_hjust), nudge_y = 0.20, size = 2.5, colour = text_colour) +
#   scale_x_log10(position = "top", limits = c(1, bpi_max * 1.15), breaks = c(1, 10, 100, 1000), labels = scales::label_comma()) +
#   labs(title = "Reported bite incidence\nper 100k (log scale)") +
#   plot_theme +
#   theme(axis.text.y = element_text(size = 9, colour = text_colour))
# 
# # B. PEP access
# 
# p_pep_access <- ggplot(sq, aes(x = pPEP, y = location)) +
#   geom_point(colour = point_colour, size = 2) +
#   geom_text(aes(label = pPEP_label), hjust = -0.20, size = 2.5, colour = text_colour) +
#   scale_x_continuous(position = "top", limits = c(0, 1.05), breaks = seq(0, 1, 0.25), labels = scales::label_percent(accuracy = 1), expand = c(0, 0)) +
#   labs(title = "PEP access") +
#   plot_theme
# 
# # C. Human:dog ratio
# 
# hdr_max <- max(120, sq$HDR2, na.rm = TRUE)
# 
# p_hdr <- ggplot(sq, aes(y = location)) +
#   geom_segment(aes(x = HDR1, xend = HDR2, yend = location), colour = point_colour, linewidth = 1, lineend = "round") +
#   geom_point(aes(x = HDR1), colour = point_colour, size = 2) +
#   geom_point(aes(x = HDR2), colour = point_colour, size = 2) +
#   geom_text(aes(x = hdr_label_x, label = hdr_label, hjust = hdr_hjust), size = 2.5, colour = text_colour) +
#   scale_x_continuous(position = "top", limits = c(0, hdr_max), breaks = pretty(c(0, hdr_max), n = 5), expand = c(0, 0)) +
#   labs(title = "Human:dog ratio") +
#   plot_theme
# 
# # D. Baseline dog vaccination
# 
# p_vax <- ggplot(sq, aes(x = dog_vax_cov, y = location)) +
#   geom_segment(aes(x = 0, xend = dog_vax_cov, yend = location), colour = interval_colour, linewidth = 0.55) +
#   geom_point(colour = point_colour, size = 2) +
#   geom_text(aes(label = dog_vax_label), hjust = -0.20, size = 2.5, colour = text_colour) +
#   scale_x_continuous(position = "top", limits = c(0, 0.52), breaks = seq(0, 0.5, 0.1), labels = scales::label_percent(accuracy = 1), expand = c(0, 0)) +
#   labs(title = "Dog vaccination") +
#   plot_theme
# 
# # E–F. Binary indicators
# 
# binary_panel <- function(variable, title) {
#   ggplot(sq, aes(x = 1, y = location)) +
#     geom_text(aes(label = .data[[variable]]), size = 2.5, colour = text_colour) +
#     scale_x_continuous(position = "top", limits = c(0.5, 1.5), breaks = NULL) +
#     labs(title = title) +
#     plot_theme +
#     theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5))
# }
# 
# p_free <- binary_panel("free_pep", "Free PEP")
# p_rig <- binary_panel("rig_avail", "RIG available")
# 
# fig3A <- p_bpi + p_pep_access + p_hdr + p_vax + p_free + p_rig + plot_layout(widths = c(1.55, 1.15, 1.20, 1.20, 0.48, 0.58))



## FIGURE 3C: ANNUAL STATUS-QUO OUTCOMES ########

summarise_annual <- function(mat, denominator = 1, multiplier = 1) {
  x <- rowMeans(mat, na.rm = TRUE) * multiplier / denominator
  q <- quantile(x, probs = c(0.025, 0.5, 0.975), na.rm = TRUE)
  tibble(LL = unname(q[1]), Median = unname(q[2]), UL = unname(q[3]))
}

summarise_cost_per_death_averted <- function(cost_mat, deaths_averted_mat) {
  stopifnot(identical(dim(cost_mat), dim(deaths_averted_mat)))
  
  total_cost <- rowSums(cost_mat, na.rm = FALSE)
  total_deaths_averted <- rowSums(deaths_averted_mat, na.rm = FALSE)
  
  cost_per_death_averted <- ifelse(total_deaths_averted > 0, total_cost / total_deaths_averted, NA_real_)
  q <- quantile(cost_per_death_averted, probs = c(0.025, 0.5, 0.975), na.rm = TRUE)
  
  tibble(LL = unname(q[1]), Median = unname(q[2]), UL = unname(q[3]))
}



sq_results <- map_dfr(seq_len(nrow(sq)), \(i) {
  r <- country_results[[sq$result_name[i]]]
  
  bind_rows(
    summarise_annual(r$ts_deaths, sq$pop[i], 1e5) |> mutate(metric = "Human deaths"),
    summarise_annual(r$ts_cost_per_year, sq$pop[i], 1e5) |> mutate(metric = "Total cost"),
    summarise_annual(r$ts_rabid_dogs, sq$dog_pop[i], 1e3) |> mutate(metric = "Rabid dogs"),
    summarise_annual(r$ts_PEP_vials, sq$pop[i], 1e5) |> mutate(metric = "PEP vials"),
    summarise_cost_per_death_averted(r$ts_cost_per_year, r$ts_deaths_averted) |> mutate(metric = "Cost per death averted")
  ) |>
    mutate(location = sq$location[i], .before = 1)
}) |>
  mutate(location = factor(location, levels = rev(location_order)))


number_1 <- scales::label_number(accuracy = 0.1, big.mark = ",")
number_0 <- scales::label_number(accuracy = 1, big.mark = ",")
money <- scales::label_dollar(accuracy = 1, prefix = "$", big.mark = ",")

result_panel <- function(metric_name, panel_title, label_function, show_locations = FALSE) {
  plot_data <- sq_results |> filter(metric == metric_name) |> mutate(median_label = label_function(Median))
  
  p <- ggplot(plot_data, aes(y = location)) +
    geom_segment(aes(x = LL, xend = UL, yend = location), colour = interval_colour, linewidth = 1, lineend = "round") +
    geom_point(aes(x = Median), colour = point_colour, size = 2.5) +
    geom_text(aes(x = UL, label = median_label), hjust = -0.12, size = 2.5, colour = text_colour) +
    scale_x_continuous(labels = label_function, expand = expansion(mult = c(0.04, 0.24))) +
    labs(title = panel_title, x = NULL, y = NULL) +
    outcome_theme
  
  if (show_locations) p <- p + theme(axis.text.y = element_text(size = 9, colour = text_colour))
  
  p
}

p_deaths <- result_panel("Human deaths", "Annual human deaths\nper 100,000", number_1, TRUE)
p_cost <- result_panel("Total cost", "Annual cost\nper 100,000", money)
p_rabid_dogs <- result_panel("Rabid dogs", "Annual rabid dogs\nper 1,000 dogs", number_1)
p_pep_vials <- result_panel("PEP vials", "Annual PEP vials\nper 100,000", number_0)
p_cost_per_death <- result_panel("Cost per death averted", "Cost per death averted",
  money)

fig3C <- p_deaths + p_cost + p_rabid_dogs + p_pep_vials + p_cost_per_death +
  plot_layout(widths = c(1.2, 1.5, 1.2, 1.2, 1.3))

fig3C

fig3 <- ((plot3A + plot3B) / plot_spacer() / wrap_elements(full = fig3C)) +
  plot_layout(heights = c(0.9, 0.08, 1)) +
  plot_annotation(tag_levels = "A")

fig3


pdf("./output/fig3.pdf", width = 12, height = 7)
print(fig3)
dev.off()


# FIGURE 5: RELATIVE IMPACT VS STATUS QUO #############

summarise_relative_impact <- function(sq_mat, intervention_mat) {
  sq_total <- rowSums(sq_mat, na.rm = TRUE)
  int_total <- rowSums(intervention_mat, na.rm = TRUE)
  relative_impact <- ifelse(sq_total == 0, NA_real_, 100 * (int_total - sq_total) / sq_total)
  q <- quantile(relative_impact, probs = c(0.025, 0.5, 0.975), na.rm = TRUE)
  tibble(LL = unname(q[1]), Median = unname(q[2]), UL = unname(q[3]))
}

setting_tbl <- sq |> transmute(setting = as.character(location), result_id, pop, dog_pop)

intervention_codes <- c("PEP", "MDV", "IBCM", "IBCM_MDV")
metrics <- c("Deaths" = "ts_deaths", "Costs" = "ts_cost_per_year")

impact <- tidyr::crossing(setting = setting_tbl$setting, scenario = intervention_codes, outcome = names(metrics)) |>
  left_join(setting_tbl |> select(setting, result_id), by = "setting") |>
  mutate(metric = unname(metrics[outcome])) |>
  rowwise() |>
  mutate(summary = list(summarise_relative_impact(country_results[[paste0(
    result_id, "_SQ")]][[metric]], country_results[[paste0(result_id, "_", scenario)]][[metric]]))) |>
  ungroup() |>
  unnest(summary) |>
  mutate(setting = factor(setting, levels = rev(setting_tbl$setting)), 
         scenario = factor(scenario_labels[scenario], levels = scenario_labels[intervention_codes]))

plot_relative_impact <- function(data, x_title, colour = "#216653") {
  ggplot(data, aes(x = Median, y = setting)) +
    geom_vline(xintercept = 0, colour = "grey55", linewidth = 0.5) +
    geom_errorbar(aes(xmin = LL, xmax = UL), orientation = "y", width = 0, linewidth = 0.8, colour = colour) +
    geom_point(size = 2.8, colour = colour) +
    facet_grid(cols = vars(scenario)) +
    scale_x_continuous(labels = scales::label_number(accuracy = 1, suffix = "%")) +
    labs(x = x_title, y = NULL) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(), strip.text = element_text(face = "bold", size = 11), axis.text.y = element_text(colour = "black"), axis.title.x = element_text(margin = margin(t = 8)))
}

deaths_impact <- impact |>
  filter(outcome == "Deaths") |>
  mutate(exceeds_xlim = UL > 200)

fig5A <- plot_relative_impact(deaths_impact, "Percentage change in cumulative deaths relative to SQ") +
  geom_text(data = filter(deaths_impact, exceeds_xlim), aes(x = 195, y = setting, label = "*"), inherit.aes = FALSE, size = 5, fontface = "bold", colour = "#216653") +
  coord_cartesian(xlim = c(-150, 200))

fig5B <- impact |>
  filter(outcome == "Costs") |>
  plot_relative_impact("Percentage change in cumulative cost relative to SQ")

fig5 <- (fig5A / plot_spacer() / fig5B) +
  plot_layout(heights = c(1, 0.08, 1)) +
  plot_annotation(tag_levels = "A", caption = "* Upper 95% uncertainty limit exceeds the displayed range in panel A.")

fig5



pdf("./output/fig5.pdf", width = 10, height = 6)
print(fig5)
dev.off()

# FIGURE 6: TEMPORAL TRAJECTORIES #########

summarise_annual <- function(mat, denominator = 1, multiplier = 1) {
  x <- sweep(mat, 1, rep(denominator, nrow(mat)), "/") * multiplier
  q <- apply(x, 2, quantile, probs = c(0.025, 0.5, 0.975), na.rm = TRUE)
  tibble(year = seq_len(ncol(mat)), LL = q[1, ], Median = q[2, ], UL = q[3, ])
}

temporal <- map_dfr(seq_len(nrow(settings)), \(i) {
  
  map_dfr(scenario_codes, \(s) {
    
    r <- country_results[[paste0(settings$result_id[i], "_", s)]]
    
    bind_rows(
      summarise_annual(r$ts_rabid_dogs, settings$dog_pop[i], 1e3) |> mutate(panel = "Rabid dogs"),
      summarise_annual(r$ts_exp_start, settings$pop[i], 1e5) |> mutate(panel = "PEP starts"),
      summarise_annual(r$ts_deaths, settings$pop[i], 1e5) |> mutate(panel = "Deaths"),
      summarise_annual(r$ts_positive_tests, settings$pop[i], 1e3) |> mutate(panel = "Dogs tested positive")
    ) |>
      mutate(setting = settings$setting[i], scenario = scenario_labels[s])
  })
}) |>
  mutate(setting = factor(setting, levels = settings$setting),
         scenario = factor(scenario, levels = scenario_labels))

plot_temporal_column <- function(data, panel_name, column_title, show_settings = FALSE) {
  
  ggplot(filter(data, panel == panel_name), aes(x = year, y = Median, colour = scenario, fill = scenario, group = scenario)) +
    geom_ribbon(aes(ymin = LL, ymax = UL), alpha = 0.10, colour = NA) +
    geom_line(linewidth = 0.7) +
    facet_grid(rows = vars(setting), #scales = "free_y", 
               switch = "y") +
    scale_colour_manual(values = scenario_colours) +
    scale_fill_manual(values = scenario_colours) +
    scale_x_continuous(breaks = c(1, 5, 10)) +
    labs(title = column_title, x = "Year", y = NULL, colour = NULL, fill = NULL) +
    guides(fill = "none", colour = guide_legend(nrow = 1, override.aes = list(linewidth = 1.2))) +
    theme_minimal(base_size = 10) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = "grey90", linewidth = 0.3),
          strip.background = element_blank(),
          strip.placement = "outside",
          strip.text.y.left = if (show_settings) element_text(angle = 0, hjust = 1, size = 13) else element_blank(),
          plot.title = element_text(face = "bold", hjust = 0.5, size = 11),
          panel.spacing.y = unit(0.5, "lines"),
          legend.position = "bottom")
}

fig6_rabid <- plot_temporal_column(temporal, "Rabid dogs", "Rabid dogs\nper 1,000 dogs", TRUE)
fig6_pep <- plot_temporal_column(temporal, "PEP starts", "Exposures starting PEP\n per100k")
fig6_deaths <- plot_temporal_column(temporal, "Deaths", "Human deaths\nper 100k")
fig6_dogsTested <- plot_temporal_column(temporal, "Dogs tested positive", "Dogs tested positive\nper1,000 dogs")


fig6 <- (fig6_rabid + fig6_pep + fig6_deaths + fig6_dogsTested +
           plot_layout(ncol = 4, widths = c(1.2, 1, 1, 1), 
                       guides = "collect")) & theme(legend.position = "bottom")

fig6

pdf("./output/fig6.pdf", width = 11, height = 14)
print(fig6)
dev.off()

# FIGURE 4: 3 PANEL DEATHS AVERTED/ DEATHS/COSTS ##########

# Summarise country_results 

# Median cumulative value across simulations
median_cumulative <- function(result, variable) {
  x <- result[[variable]]
  if (is.null(x)) {return(0)}
  
  stopifnot(is.matrix(x))
  
  median(rowSums(x, na.rm = FALSE),na.rm = TRUE)
}


# Median cumulative sum of several matrices
median_cumulative_sum <- function(result, variables) {
  matrices <- purrr::map(variables,
                         ~ result[[.x]]) |>
    purrr::compact()
  
  if (length(matrices) == 0L) {return(0)}
  
  total_matrix <- Reduce(`+`, matrices)
  
  median(rowSums(total_matrix, na.rm = FALSE), na.rm = TRUE)
}


# Link each result to its setting and scenario
result_lookup <- params |>
  mutate(result_name = paste(
    country, Administrative_unit, scenario, sep = "_"),
    setting = paste(country, Administrative_unit, sep = " · ")
    ) |>
  select(result_name, country, Administrative_unit, setting, scenario)


# Summarise each country × scenario result
figure_summary <- purrr::imap_dfr(
  country_results,
  function(result, result_name) {
    tibble(
      result_name = result_name,
      # Deaths averted
      deaths_averted_pep = median_cumulative(result,"ts_deaths_averted_PEP"),
      deaths_averted_mdv = median_cumulative(result, "ts_deaths_averted_MDV"),
      # Costs
      cost_pep = median_cumulative_sum(
        result, c("ts_cost_PEP_per_year", "ts_RIG_cost_per_year")
        ),
      cost_mdv = median_cumulative(result, "ts_MDV_campaign_cost"),
      cost_ibcm = median_cumulative(result, "ts_ibcm_costs"),
      # Total deaths
      total_deaths = median_cumulative(result, "ts_deaths")
    )
  }
) |>
  left_join(result_lookup, by = "result_name") |>
  mutate(scenario_label = unname(scenario_labels[scenario]),
    # Reverse levels so SQ appears at the top of the plot
    scenario_label = factor(scenario_label, 
                            levels = rev(unname(scenario_labels[scenario_codes]))
                            ),
    # Display costs in millions
    across(starts_with("cost_"), ~ .x / 1e6))

figure_summary

# Colours for intervention components
component_colours <- c(
  "MDV"  = "#C44E52",
  "PEP"  = "#4C72B0",
  "IBCM" = "#CC79A7"
)

stack_order <- c("MDV", "PEP", "IBCM")

make_contribution_figure <- function(setting_name,data = figure_summary) {
  plot_data <- data |>
    filter(setting == setting_name)
  
  if (nrow(plot_data) == 0L) {
    stop(paste("No results found for:", setting_name), call. = FALSE)
  }
  
  # Setting-specific SQ reference values
  sq_reference <- plot_data |>
    filter(scenario == "SQ") |>
    transmute(deaths_averted = deaths_averted_pep + deaths_averted_mdv,
              costs = cost_pep + cost_mdv + cost_ibcm,
              deaths = total_deaths)
  
  if (nrow(sq_reference) != 1L) {
    stop("Exactly one SQ result is required for each setting.", call. = FALSE)
  }
  
  # Deaths-averted contributions
  deaths_long <- plot_data |>
    select(scenario_label, PEP = deaths_averted_pep, MDV = deaths_averted_mdv) |>
    pivot_longer(cols = c(PEP, MDV), names_to = "component", values_to = "value") |>
    mutate(component = factor(component, levels = stack_order))
  
  # Cost contributions
  costs_long <- plot_data |>
    select(scenario_label, PEP = cost_pep, MDV = cost_mdv, IBCM = cost_ibcm) |>
    pivot_longer(cols = c(PEP, MDV, IBCM), 
                 names_to = "component",
                 values_to = "value") |>
    mutate(component = factor(component, levels = stack_order))
  
  common_theme <- theme_classic(base_size = 10) +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_text(size = 9),
          plot.margin = margin(5, 8, 5, 8))
  
  # A. Deaths averted
  p_deaths_averted <- ggplot(deaths_long, aes(x = value, y = scenario_label, fill = component)) +
    geom_col(width = 0.7, position = position_stack(reverse = TRUE)) +
    geom_vline(xintercept = sq_reference$deaths_averted, linetype = "dashed", 
               colour = "black", linewidth = 0.5) +
    scale_fill_manual(values = component_colours, breaks = stack_order, drop = FALSE) +
    scale_x_continuous(labels = scales::label_comma(), expand = expansion(mult = c(0.02, 0.08))) +
    labs(title = setting_name,x = "Deaths averted") +
    guides(fill = "none") +
    common_theme
  
  # B. Costs
  p_costs <- ggplot(costs_long, aes(x = value, y = scenario_label, fill = component)) +
    geom_col(width = 0.7, position = position_stack(reverse = TRUE)) +
    geom_vline(xintercept = sq_reference$costs, linetype = "dashed", 
               colour = "black", linewidth = 0.5) +
    scale_fill_manual(values = component_colours, breaks = stack_order, drop = FALSE) +
    scale_x_continuous(labels = scales::label_comma(accuracy = 0.1),
                       expand = expansion(mult = c(0.02, 0.08))) +
    labs(x = "Costs (million US$)", fill = NULL) +
    common_theme +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank()
          )
  
  # C. Total deaths
  p_total_deaths <- ggplot(plot_data, aes(x = total_deaths, y = scenario_label)) +
    geom_col(fill = "grey70", width = 0.7) +
    geom_vline(xintercept = sq_reference$deaths, linetype = "dashed",
               colour = "black", linewidth = 0.5) +
    scale_x_continuous(labels = scales::label_comma(),
                       expand = expansion(mult = c(0.02, 0.08))) +
    labs(x = "Total deaths") +
    common_theme +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
  
  # Combine panels
  combined_plot <- p_deaths_averted + p_costs + p_total_deaths +
    #patchwork::plot_layout(nrow = 1, widths = c(1.4, 1, 1), guides = "collect") +
    patchwork::plot_layout(nrow = 1, widths = c(1.4, 1, 1), guides = "auto") +
    patchwork::plot_annotation(title = setting_name)
  
   combined_plot #&
  #   theme(legend.position = "bottom")
}

unique(figure_summary$setting)

make_contribution_figure(
  "India · KeralaState"
)

make_contribution_figure(
  "Kenya · National" 
)

### Combine #####


# Settings in the order they appear in figure_summary
setting_names <- figure_summary |>
  distinct(setting) |>
  pull(setting) |>
  as.character()


# Create one contribution figure per setting
Fig4_by_setting <- setting_names |>
  set_names() |>
  purrr::map(
    ~ make_contribution_figure(
      setting_name = .x,
      data = figure_summary
    )
  )


# Combine all settings vertically
Fig4_all <- patchwork::wrap_plots(
  Fig4_by_setting,
  ncol = 2,
  guides = "collect"
) &
  theme(
    legend.position = "bottom"
  )

Fig4_all


ggsave(
  filename = "./output/Fig4.pdf",
  plot = Fig4_all,
  width = 11,
  height = 1.25 * length(setting_names),
  units = "in",
  limitsize = FALSE
)



# COST-EFFECTIVENESS ACCEPTABILITY CURVES ##########

wtp_values <- seq(100, 120000, by = 1000)

calculate_ceac <- function(result_id) {
  
  deaths <- sapply(scenario_codes, \(s) rowSums(country_results[[paste0(result_id, "_", s)]]$ts_deaths, na.rm = TRUE))
  costs <- sapply(scenario_codes, \(s) rowSums(country_results[[paste0(result_id, "_", s)]]$ts_cost_per_year, na.rm = TRUE))
  
  colnames(deaths) <- colnames(costs) <- scenario_codes
  
  deaths_averted <- deaths[, "SQ"] - deaths
  incremental_costs <- costs - costs[, "SQ"]
  
  map_dfr(wtp_values, \(wtp) {
    nhb <- deaths_averted - incremental_costs / wtp
    is_best <- nhb == apply(nhb, 1, max, na.rm = TRUE)
    weights <- is_best / rowSums(is_best)
    tibble(wtp = wtp, scenario = scenario_codes, probability = colMeans(weights, na.rm = TRUE))
  })
}

ceac_data <- map2_dfr(settings$result_id, settings$setting, \(result_id, setting_name) calculate_ceac(result_id) |> mutate(setting = setting_name)) |>
  mutate(setting = factor(setting, levels = settings$setting),
         scenario = factor(scenario_labels[scenario], levels = scenario_labels))

fig6 <- ggplot(ceac_data, aes(x = wtp, y = probability, colour = scenario)) +
  geom_line(linewidth = 0.9) +
  facet_wrap(vars(setting), ncol = 3) +
  scale_colour_manual(values = scenario_colours) +
  scale_x_continuous(limits = c(100, 110000), breaks = c(100, 25000, 50000, 75000, 100000), labels = scales::label_dollar(scale_cut = scales::cut_short_scale()), expand = expansion(mult = c(0, 0.01))) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25), labels = scales::label_percent(), expand = expansion(mult = c(0, 0.02))) +
  labs(x = "Willingness to pay per death averted", y = "Probability of highest net health benefit", colour = NULL) +
  guides(colour = guide_legend(nrow = 1, override.aes = list(linewidth = 1.3))) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "grey90", linewidth = 0.3),
        strip.text = element_text(face = "bold", size = 10),
        legend.position = "bottom",
        legend.justification = "center",
        axis.title = element_text(size = 11))

fig6

pdf("./output/fig6.pdf", width = 10, height = 9)
print(fig6)
dev.off()


# TABLE 4.2: Projected cumulative human rabies deaths over 10 years ------------

scenario_codes <- c("SQ", "PEP", "MDV", "IBCM", "IBCM_MDV")
scenario_names <- c("SQ" = "Status quo", "PEP" = "Improved PEP", "MDV" = "MDV", "IBCM" = "IBCM", "IBCM_MDV" = "IBCM plus MDV")

table_settings <- params |> filter(scenario == "SQ") |> 
  transmute(Country = country, `Administrative unit` = Administrative_unit, result_id = paste(country, Administrative_unit, sep = "_"))

summarise_deaths <- function(result_id, scenario) {
  deaths <- rowSums(country_results[[paste0(result_id, "_", scenario)]]$ts_deaths, na.rm = TRUE)
  q <- round(quantile(deaths, probs = c(0.025, 0.5, 0.975), na.rm = TRUE))
  fmt <- scales::label_number(accuracy = 1, big.mark = ",")
  tibble(Median = q[2], LL = q[1], UL = q[3], value = paste0(fmt(q[2]), " (", fmt(q[1]), "-", fmt(q[3]), ")"))
}

table4_2_long <- tidyr::crossing(table_settings, scenario = scenario_codes) |>
  rowwise() |>
  mutate(summary = list(summarise_deaths(result_id, scenario))) |>
  ungroup() |>
  tidyr::unnest(summary) |>
  mutate(strategy = factor(scenario_names[scenario], levels = scenario_names))

table4_2 <- table4_2_long |>
  select(Country, `Administrative unit`, strategy, value) |>
  pivot_wider(names_from = strategy, values_from = value)

table4_2

write.csv(table4_2, "output/Table4_2_cumulative_deaths.csv", row.names = FALSE)

##########
















































# Visualization 1 - Predicted cases given X vaccination coverage ####

# Forecasting rabies cases over X years, given Y% dog vaccination coverage

## Deterministic ####

vax_coverage_over_x_years <- function(target_coverage, no_of_years){
  # assume it takes 3 years to hit target vaccination coverage
  list1<- map2(0, target_coverage, seq, length.out = 3)
  list2<- rep(target_coverage, (no_of_years-3))
  annual_vax_cov<- unlist(append(list1, list2))
  return(annual_vax_cov)
  }

# Set up prediction model
predict_cases_deterministic <- function(target_coverage, no_of_years, dog_pop){
  # model inputs
  vax_model <- readRDS("./data/cases_from_vax_model.rds")
  vax_case_model <- readRDS("./data/cases_from_vax+cases_model.rds")
  # model output
  cases <- rep(NA, no_of_years)
  vc_last_year <- vax_coverage_over_x_years(target_coverage, no_of_years)
  new_data <- data.frame(vax_last_year=vc_last_year[1],dogs=dog_pop)
  cases[1] <- mean(posterior_epred(vax_model, newdata = new_data))
  for(year in 2:no_of_years){
    new_data <- data.frame(vax_last_year=vc_last_year[year],cases_last_year=cases[year-1],dogs=dog_pop)
    cases[year] <- colMeans(posterior_epred(vax_case_model, newdata = new_data))
    }
  return(cases)
  }


predicted_cases_deterministic <- predict_cases_deterministic(target_coverage=0.7, no_of_years=8, dog_pop=200000)

plot(predicted_cases_deterministic,type="l", ylab="Cases",xlab="Year")


## Stochastic #####

# Set up model
predict_cases_stochastic <- function(nreps=1000, target_coverage, no_of_years, dog_pop){
  set.seed(0)
  # model inputs
  vax_model_samples <- read.csv("./data/cases_from_vax_par_samples.csv")
  vax_case_model_samples <- read.csv("./data/cases_from_vax+cases_par_samples.csv")
  # model output
  cases <- rep(NA, no_of_years)
  vc_last_year <- vax_coverage_over_x_years(target_coverage, no_of_years)
  cases_mat <- matrix(NA,nrow=nreps,ncol=no_of_years)

  # Simulate from models once for a district
  pars_sim <- vax_model_samples[sample.int(nrow(vax_model_samples),size=1),]
  mu <- exp(sum(pars_sim[1:2]*c(1,vc_last_year[1]),log(dog_pop)))
  cases[1] <- rnbinom(n=1,mu=mu,size=as.numeric(pars_sim[3]))
  pars_sim <- vax_case_model_samples[sample.int(nrow(vax_case_model_samples),size=1),]
  for(i in 2:no_of_years){
    mu <- exp(sum(pars_sim[1:3]*c(1,vc_last_year[i],log(cases[i-1]+1)),log(dog_pop)))
    cases[i] <- rnbinom(n=1,mu=mu,size=as.numeric(pars_sim[4]))
  }

  # How does this look of we do it many times?
  for(rep in 1:nreps){
    pars_sim <- vax_model_samples[sample.int(nrow(vax_model_samples),size=1),]
    mu <- exp(sum(pars_sim[1:2]*c(1,vc_last_year[1]),log(dog_pop)))
    cases_mat[rep,1] <- rnbinom(n=1,mu=mu,size=as.numeric(pars_sim[3]))
    pars_sim <- vax_case_model_samples[sample.int(nrow(vax_case_model_samples),size=1),]
    for(year in 2:no_of_years){
      mu <- exp(sum(pars_sim[1:3]*c(1,vc_last_year[year],log(cases[year-1]+1)),log(dog_pop)))
      cases_mat[rep,year] <- rnbinom(n=1,mu=mu,size=as.numeric(pars_sim[4]))
    }
  }

  lower <- upper <- med <- rep(NA,no_of_years) # get median and 95 percentile limits
  quantiles <- matrix(NA,nrow=3,ncol=no_of_years)
  for(year in 1:no_of_years){
    quantiles[,year] <- quantile(cases_mat[,year],c(0.025,0.5,0.975))
  }
  return(quantiles)
}

# dog_pop
dog_pop_df <- output_stoch_model[1:nrow(east_africa_shp),] %>%
  st_drop_geometry()  %>%
  dplyr::group_by(Country) %>%
  dplyr::summarise(dog_pop=sum(dog_population_mean),.groups = 'drop') %>%
  as.data.frame()

# plot
plot_prediction <- function(no_of_years=8, country){
  predicted_cases_stochastic <-predict_cases_stochastic(nreps=1000, target_coverage=0.7, no_of_years=8, dog_pop=dog_pop_df$dog_pop[dog_pop_df$Country==country])
  plot(predicted_cases_stochastic[2,],ylim=c(0,max(predicted_cases_stochastic)),type="l",bty="l",ylab="Cases",xlab="Year",
       main="Predicted cases with 70% dog vaccination")
  polygon(c(1:no_of_years,rev(1:no_of_years)),c(predicted_cases_stochastic[1,],rev(predicted_cases_stochastic[3,])),
          col=adjustcolor( "#E69F00", alpha.f = 0.4),border=adjustcolor( "#E69F00", alpha.f = 0.4))
  lines(predicted_cases_stochastic[2,],lwd=2,col="black")
}


plot_prediction(no_of_years=8, country = "Kenya")

# Visualization 2 - Slope chart comparing different policy choices ####

# Slope chart

   ## extract data
filter_policy_summary_df <- function(country, big_df){
  policy_summary_df <- output_stoch_model %>%
    dplyr::filter(Country == country,
                  vax_cov == 0) %>%
    st_drop_geometry() %>%
    dplyr::select(policy_choice, total_people_PEP_mean,
                  lives_saved_mean, rabies_deaths_mean) %>%
    #drop_na()  %>%
    dplyr::group_by(policy_choice) %>%
    dplyr::summarise(Total_people_PEP = sum(total_people_PEP_mean),
              Lives_saved = sum(lives_saved_mean),
              Rabies_deaths = sum(rabies_deaths_mean),
              .groups = 'drop') %>%
    gather(., outcome, no_of_people, Total_people_PEP:Rabies_deaths) %>%
    mutate_at(
      "policy_choice", recode,
      "Offered free of charge" = "Free of charge",
      "Offered under status quo" = "Status quo")

   ## use whole numbers
  policy_summary_df$no_of_people <- round(policy_summary_df$no_of_people, 0)

   ## add commas for aesthetics
  policy_summary_df$label <- scales::comma(policy_summary_df$no_of_people)
  
  return(policy_summary_df)
}



   ## plot

plot_b_function <- function(country, big_df){
  
  policy_summary_df <- filter_policy_summary_df(country, big_df)

  plot_b <- newggslopegraph(dataframe = policy_summary_df,
                Times = policy_choice,
                Measurement = no_of_people,
                Grouping = outcome,
                Data.label = label,
                Title = "Policy choices for PEP",
                SubTitle = NULL,
                Caption = NULL,
                LineThickness = 1.2,
                XTextSize = 10,    # Size of the times
                YTextSize = 3,     # Size of the groups
                TitleTextSize = 14,
                SubTitleTextSize = 12,
                CaptionTextSize = 10,
                TitleJustify = "center",
                SubTitleJustify = "right",
                CaptionJustify = "left",
                DataTextSize = 3.5,
                DataLabelLineSize = 0
                )
  return(plot_b)
}

plot_b_function(country="Kenya", big_df = output_stoch_model)

# Visualization 3 - Boxplot comparing costs of PEP administration choices ####
## extract data


filter_pep_admin_summary_df <- function(country, big_df){
  pep_admin_summary_df <- output_stoch_model %>%
    dplyr::filter(Country == country,
                vax_cov == 0,
                policy_choice == "Offered under status quo") %>%
    st_drop_geometry() %>%
    dplyr::select(total_PEP_intradermal_mean, total_PEP_intradermal_UL, total_PEP_intradermal_LL,
                total_PEP_intramuscular_mean, total_PEP_intramuscular_UL, total_PEP_intramuscular_LL) %>%
    drop_na() %>%
    summarise(across(everything(), ~ sum(.))) %>%
    mutate_all(.,function(col){15*col})   #USD value of PEP vials

  intraM <- data.frame("Intramuscular", pep_admin_summary_df$total_PEP_intramuscular_mean, pep_admin_summary_df$total_PEP_intramuscular_UL, pep_admin_summary_df$total_PEP_intramuscular_LL)
  names(intraM) <- c("PEP_admin_route", "Mean", "upper_limit", "lower_limit")
  intraD<- data.frame("Intradermal", pep_admin_summary_df$total_PEP_intradermal_mean, pep_admin_summary_df$total_PEP_intradermal_UL, pep_admin_summary_df$total_PEP_intradermal_LL)
  names(intraD) <- c("PEP_admin_route", "Mean", "upper_limit", "lower_limit")

  pep_admin_summary_df <- rbind(intraM, intraD)
  
  return(pep_admin_summary_df)
}

plot_c_function <- function(country, big_df){
  
  filter_pep_admin_summary_df(country, big_df) %>%
    ggplot(., aes(x=PEP_admin_route, y=Mean, color=PEP_admin_route)) +
    geom_point(size=3)+
    geom_errorbar(aes(ymin=lower_limit, ymax=upper_limit), width=.2,
                  position=position_dodge(0.05)) +
    scale_y_continuous(labels = scales::comma) +
    scale_color_manual(values=c('#E69F00', '#999999'))+
    labs(title = "Estimated national costs of PEP annually",
         y = "US dollars")+
    theme(
      axis.text.x = element_text(angle=0, color="black", size=13),
      axis.title.x = element_blank(),
      axis.title.y = element_text(color="black", size=14, face="bold"),
      axis.text.y = element_text(color="black", size=13),
      plot.title = element_text(size = 14, face = "bold"),
      legend.position = "none"
    )
}


plot_c_function(country="Kenya", big_df = output_stoch_model)


# create a grid of the three plots

Create_plot_grid <- function(no_of_years=8, big_df = output_stoch_model, country){
  a <- plot_prediction(no_of_years, country = country)
  b <- plot_b_function(country=country, big_df)
  c <- plot_c_function(country=country, big_df)
  
  plot_grid <- (b+c) / ~plot_prediction(no_of_years, country = country)
  
  return(plot_grid)
}


KE_plot <- Create_plot_grid(country="Kenya")
UG_plot <- Create_plot_grid(country="Uganda")
TZ_plot <- Create_plot_grid(country="Tanzania")


# remove objects we wont need downstream from memory
rm(list=setdiff(ls(), c("east_africa_shp",  "output_stoch_model", "output_det_model",
                        "vax_covs", "KE_plot", "UG_plot", "TZ_plot")))



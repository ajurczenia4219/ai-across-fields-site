# Builds compact, text-based SVG bar charts in R. Values come from the sources listed on the page.
blue <- "#1f4e8c"; light <- "#8aa6c8"; orange <- "#c2571a"; grey <- "#5b6675"; ink <- "#1c2430"

make_chart <- function(file, labels, values, cols, xlab, fmt, xmin, xmax, by) {
  W <- 720; left <- 210; right <- 70; pw <- W - left - right
  bh <- 38; gap <- 18; top <- 8; n <- length(values)
  H <- top + n * (bh + gap) + 62
  sx <- function(v) left + (v - xmin) / (xmax - xmin) * pw
  out <- character()
  add <- function(...) out <<- c(out, sprintf(...))
  add('<svg viewBox="0 0 %d %d" xmlns="http://www.w3.org/2000/svg" role="img" aria-label="%s" font-family="inherit">', W, H, xlab)
  axis_y <- top + n * (bh + gap)
  for (t in seq(xmin, xmax, by = by)) {
    add('<line x1="%.1f" x2="%.1f" y1="%d" y2="%d" stroke="#e2e6ec"/>', sx(t), sx(t), top, axis_y)
    add('<text x="%.1f" y="%d" text-anchor="middle" font-size="13" fill="%s">%s</text>', sx(t), axis_y + 18, grey, format(t))
  }
  for (i in seq_len(n)) {
    y <- top + (i - 1) * (bh + gap) + gap / 2
    v <- values[i]; x0 <- sx(0); x1 <- sx(v)
    add('<rect x="%.1f" y="%.1f" width="%.1f" height="%d" fill="%s" rx="2"/>', min(x0, x1), y, abs(x1 - x0), bh, cols[i])
    lines <- strsplit(labels[i], "\n")[[1]]
    for (k in seq_along(lines)) {
      ly <- y + bh / 2 + 5 + (k - (length(lines) + 1) / 2) * 16
      add('<text x="%d" y="%.1f" text-anchor="end" font-size="14" fill="%s">%s</text>', left - 12, ly, ink, lines[k])
    }
    if (v >= 0) add('<text x="%.1f" y="%.1f" font-size="15" font-weight="700" fill="%s">%s</text>', x1 + 8, y + bh / 2 + 5, ink, fmt(v))
    else        add('<text x="%.1f" y="%.1f" text-anchor="end" font-size="15" font-weight="700" fill="%s">%s</text>', x1 - 8, y + bh / 2 + 5, ink, fmt(v))
  }
  add('<line x1="%.1f" x2="%.1f" y1="%d" y2="%d" stroke="%s"/>', sx(0), sx(0), top, axis_y, grey)
  add('<text x="%.1f" y="%d" text-anchor="middle" font-size="13" fill="%s">%s</text>', left + pw / 2, axis_y + 44, ink, xlab)
  add('</svg>')
  writeLines(out, file)
}
pct <- function(v) paste0(v, "%")

make_chart("charts/adoption.svg", c("2024 survey", "2025 survey"), c(78, 88), c(light, blue),
           "% of organizations using AI in at least one business function", pct, 0, 100, 20)
make_chart("charts/software.svg", c("GitHub Copilot experiment", "METR trial: experienced\ndevelopers"), c(-55, 19), c(blue, orange),
           "% change in task completion time when AI was allowed",
           function(v) ifelse(v > 0, paste0("+", v, "%"), paste0(v, "%")), -80, 20, 20)
make_chart("charts/datascience.svg", c("Data cleaning, visualization\nand analysis", "Automating tasks", "Prediction or detection\nmodels"),
           c(67, 52, 52), rep(blue, 3), "% of data science practitioners reporting this AI use case", pct, 0, 80, 20)
make_chart("charts/cyber.svg", c("Little or no security\nAI and automation", "Extensive security AI\nand automation"), c(5.52, 3.62), c(orange, blue),
           "Average cost of a data breach (US$ millions)", function(v) sprintf("$%.2fM", v), 0, 6, 1)
make_chart("charts/aijobs.svg", c("Prior-year report", "2025 report (2024 data)"), c(25, 56), c(light, blue),
           "Average wage premium for jobs requiring AI skills (%)", pct, 0, 60, 10)
make_chart("charts/workers.svg", c("All support agents", "Novice and low-skilled\nagents"), c(14, 34), c(light, blue),
           "% increase in issues resolved per hour with an AI assistant", pct, 0, 40, 10)

library(shiny)
library(bslib)
library(reactable)
library(tidyverse)
library(ggplot2)

ui <- page_sidebar(
  title = "Hypothesis Test for Population Proportions",
  theme = bs_theme(
    primary = "#A90533",
    "navbar-bg" = "#A90533",
    "card-header-bg" = "#A90533",
    "card-header-color" = "white"
  ),
  tags$head(
    tags$style(HTML("
      .card-header {
        background-color: #A90533 !important;
        color: white !important;
        font-weight: bold;
      }
    "))
  ),
  sidebar = sidebar(
    selectInput(
      "test_type",
      "Test Type",
      choices = c(
        "One-Sample Proportion Test" = "one",
        "Two-Sample Proportion Test" = "two"
      )
    ),
    uiOutput("input_type_ui"),
    uiOutput("sample_input_ui"),
    numericInput(
      "alpha",
      "Significance Level (α)",
      value = 0.05,
      min = 0.0001,
      max = 0.5,
      step = 0.001
    ),
    selectInput(
      "alt",
      "Alternative Hypothesis",
      choices = c(
        "Two-Sided: ≠" = "two.sided",
        "Right-Sided: >" = "greater",
        "Left-Sided: <" = "less"
      ),
      selected = "two.sided"
    )
  ),

  layout_columns(
    col_widths = c(6, 6, 12),
    row_heights = c(1, 1, 2),
    card(
      full_screen = TRUE,
      card_header("Test Input Summary"),
      reactableOutput("table_inputs")
    ),
    card(
      full_screen = TRUE,
      card_header("Hypothesis Test Results"),
      reactableOutput("table_results")
    ),
    card(
      full_screen = TRUE,
      card_header("p-Value Plot"),
      plotOutput("pplot", height = "420px")
    )
  )
)

server <- function(input, output, session) {

  fmt <- function(x, d = 4) sprintf(paste0("%.", d, "f"), x)
  fmtp <- function(p) {
    ifelse(
      p < 1e-4,
      formatC(p, format = "e", digits = 2),
      formatC(p, format = "f", digits = 6)
    )
  }

  output$input_type_ui <- renderUI({
    if (input$test_type == "one") {
      radioButtons(
        "one_input_type",
        "Input Type",
        choices = c(
          "Sample Proportion" = "prop",
          "Number of Successes" = "count"
        ),
        selected = "prop"
      )
    } else {
      radioButtons(
        "two_input_type",
        "Input Type",
        choices = c(
          "Sample Proportions and Sample Sizes" = "prop",
          "Numbers of Successes and Sample Sizes" = "count"
        ),
        selected = "prop"
      )
    }
  })

  output$sample_input_ui <- renderUI({
    if (input$test_type == "one") {
      if (input$one_input_type == "prop") {
        tagList(
          numericInput(
            "p0",
            "Null Proportion (p₀):",
            value = 0.5,
            min = 0,
            max = 1,
            step = 0.01
          ),
          numericInput(
            "p_hat",
            "Sample Proportion (p̂):",
            value = 0.55,
            min = 0,
            max = 1,
            step = 0.001
          ),
          numericInput(
            "n",
            "Sample Size:",
            value = 100,
            min = 1,
            step = 1
          )
        )
      } else {
        tagList(
          numericInput(
            "p0",
            "Null Proportion (p₀):",
            value = 0.5,
            min = 0,
            max = 1,
            step = 0.01
          ),
          numericInput(
            "x",
            "Number of Successes:",
            value = 55,
            min = 0,
            step = 1
          ),
          numericInput(
            "n_count",
            "Sample Size:",
            value = 100,
            min = 1,
            step = 1
          )
        )
      }
    } else {
      if (input$two_input_type == "prop") {
        tagList(
          numericInput(
            "p1hat",
            "Group 1 Proportion (p̂₁):",
            value = 0.60,
            min = 0,
            max = 1,
            step = 0.001
          ),
          numericInput(
            "n1",
            "Group 1 Sample Size:",
            value = 120,
            min = 1,
            step = 1
          ),
          numericInput(
            "p2hat",
            "Group 2 Proportion (p̂₂):",
            value = 0.50,
            min = 0,
            max = 1,
            step = 0.001
          ),
          numericInput(
            "n2",
            "Group 2 Sample Size:",
            value = 100,
            min = 1,
            step = 1
          )
        )
      } else {
        tagList(
          numericInput(
            "x1",
            "Group 1 Successes:",
            value = 72,
            min = 0,
            step = 1
          ),
          numericInput(
            "n1_count",
            "Group 1 Sample Size:",
            value = 120,
            min = 1,
            step = 1
          ),
          numericInput(
            "x2",
            "Group 2 Successes:",
            value = 50,
            min = 0,
            step = 1
          ),
          numericInput(
            "n2_count",
            "Group 2 Sample Size:",
            value = 100,
            min = 1,
            step = 1
          )
        )
      }
    }
  })

  calc <- reactive({
    req(input$test_type, input$alpha, input$alt)

    alpha <- input$alpha
    alt <- input$alt

    if (input$test_type == "one") {
      req(input$p0)

      if (input$one_input_type == "prop") {
        req(input$p_hat, input$n)
        validate(
          need(input$p_hat >= 0 && input$p_hat <= 1,
               "Sample proportion must be between 0 and 1."),
          need(input$n >= 1, "Sample size must be at least 1.")
        )

        ph <- input$p_hat
        n <- input$n
        x <- round(ph * n)
      } else {
        req(input$x, input$n_count)
        validate(
          need(input$x >= 0, "Number of successes must be nonnegative."),
          need(input$n_count >= 1, "Sample size must be at least 1."),
          need(input$x <= input$n_count,
               "Number of successes cannot exceed the sample size.")
        )

        x <- input$x
        n <- input$n_count
        ph <- x / n
      }

      p0 <- input$p0

      se_test <- sqrt(p0 * (1 - p0) / n)
      se_ci <- sqrt(ph * (1 - ph) / n)
      z <- (ph - p0) / se_test

      if (alt == "two.sided") {
        pval <- 2 * pnorm(-abs(z))
        zstar <- qnorm(1 - alpha / 2)
        ci_lo <- ph - zstar * se_ci
        ci_hi <- ph + zstar * se_ci
      } else if (alt == "greater") {
        pval <- 1 - pnorm(z)
        zstar <- qnorm(1 - alpha)
        ci_lo <- ph - zstar * se_ci
        ci_hi <- NA
      } else {
        pval <- pnorm(z)
        zstar <- qnorm(1 - alpha)
        ci_lo <- NA
        ci_hi <- ph + zstar * se_ci
      }

      ci_lo <- if (!is.na(ci_lo)) max(0, ci_lo) else NA
      ci_hi <- if (!is.na(ci_hi)) min(1, ci_hi) else NA

      list(
        type = "one",
        input_type = input$one_input_type,
        p0 = p0,
        ph = ph,
        x = x,
        n = n,
        se_test = se_test,
        se_ci = se_ci,
        z = z,
        pval = pval,
        ci_lo = ci_lo,
        ci_hi = ci_hi,
        alpha = alpha,
        alt = alt
      )
    } else {
      if (input$two_input_type == "prop") {
        req(input$p1hat, input$n1, input$p2hat, input$n2)
        validate(
          need(input$p1hat >= 0 && input$p1hat <= 1,
               "Group 1 proportion must be between 0 and 1."),
          need(input$p2hat >= 0 && input$p2hat <= 1,
               "Group 2 proportion must be between 0 and 1."),
          need(input$n1 >= 1, "Group 1 sample size must be at least 1."),
          need(input$n2 >= 1, "Group 2 sample size must be at least 1.")
        )

        p1 <- input$p1hat
        n1 <- input$n1
        p2 <- input$p2hat
        n2 <- input$n2
        x1 <- round(p1 * n1)
        x2 <- round(p2 * n2)
      } else {
        req(input$x1, input$n1_count, input$x2, input$n2_count)
        validate(
          need(input$x1 >= 0, "Group 1 successes must be nonnegative."),
          need(input$x2 >= 0, "Group 2 successes must be nonnegative."),
          need(input$n1_count >= 1, "Group 1 sample size must be at least 1."),
          need(input$n2_count >= 1, "Group 2 sample size must be at least 1."),
          need(input$x1 <= input$n1_count,
               "Group 1 successes cannot exceed the sample size."),
          need(input$x2 <= input$n2_count,
               "Group 2 successes cannot exceed the sample size.")
        )

        x1 <- input$x1
        n1 <- input$n1_count
        x2 <- input$x2
        n2 <- input$n2_count
        p1 <- x1 / n1
        p2 <- x2 / n2
      }

      diff_hat <- p1 - p2
      p_pool <- (x1 + x2) / (n1 + n2)
      se_test <- sqrt(p_pool * (1 - p_pool) * (1 / n1 + 1 / n2))
      se_ci <- sqrt(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2)
      z <- diff_hat / se_test

      if (alt == "two.sided") {
        pval <- 2 * pnorm(-abs(z))
        zstar <- qnorm(1 - alpha / 2)
        ci_lo <- diff_hat - zstar * se_ci
        ci_hi <- diff_hat + zstar * se_ci
      } else if (alt == "greater") {
        pval <- 1 - pnorm(z)
        zstar <- qnorm(1 - alpha)
        ci_lo <- diff_hat - zstar * se_ci
        ci_hi <- NA
      } else {
        pval <- pnorm(z)
        zstar <- qnorm(1 - alpha)
        ci_lo <- NA
        ci_hi <- diff_hat + zstar * se_ci
      }

      list(
        type = "two",
        input_type = input$two_input_type,
        p1 = p1,
        n1 = n1,
        x1 = x1,
        p2 = p2,
        n2 = n2,
        x2 = x2,
        diff_hat = diff_hat,
        se_test = se_test,
        se_ci = se_ci,
        z = z,
        pval = pval,
        ci_lo = ci_lo,
        ci_hi = ci_hi,
        alpha = alpha,
        alt = alt
      )
    }
  })

  output$table_inputs <- renderReactable({
    cdat <- calc()

    if (cdat$type == "one") {
      reactable(
        tibble(
          parameter = c("Null proportion", "Successes", "Sample size", "Sample proportion"),
          value = c(
            fmt(cdat$p0, 4),
            cdat$x,
            cdat$n,
            fmt(cdat$ph, 4)
          )
        ),
        defaultColDef = colDef(align = "right"),
        columns = list(
          parameter = colDef(name = "Input"),
          value = colDef(name = "Value")
        )
      )
    } else {
      reactable(
        tibble(
          group = c("Group 1", "Group 2"),
          successes = c(cdat$x1, cdat$x2),
          sample_size = c(cdat$n1, cdat$n2),
          sample_proportion = c(fmt(cdat$p1, 4), fmt(cdat$p2, 4))
        ),
        defaultColDef = colDef(align = "right"),
        columns = list(
          group = colDef(name = "Group", align = "left"),
          successes = colDef(name = "Successes"),
          sample_size = colDef(name = "Sample Size"),
          sample_proportion = colDef(name = "Sample Proportion")
        )
      )
    }
  })

  output$table_results <- renderReactable({
    cdat <- calc()

    if (cdat$type == "one") {
      reactable(
        tibble(
          z_statistic = fmt(cdat$z, 4),
          p_value = fmtp(cdat$pval),
          standard_error_test = fmt(cdat$se_test, 6),
          standard_error_ci = fmt(cdat$se_ci, 6),
          lower_bound = ifelse(is.na(cdat$ci_lo), "", fmt(cdat$ci_lo, 4)),
          upper_bound = ifelse(is.na(cdat$ci_hi), "", fmt(cdat$ci_hi, 4))
        ),
        defaultColDef = colDef(align = "right"),
        columns = list(
          z_statistic = colDef(name = "Z-Statistic"),
          p_value = colDef(name = "p-Value"),
          standard_error_test = colDef(name = "SE (Test)"),
          standard_error_ci = colDef(name = "SE (CI)"),
          lower_bound = colDef(name = "Lower Bound"),
          upper_bound = colDef(name = "Upper Bound")
        )
      )
    } else {
      reactable(
        tibble(
          sample_difference = fmt(cdat$diff_hat, 6),
          z_statistic = fmt(cdat$z, 4),
          p_value = fmtp(cdat$pval),
          standard_error_test = fmt(cdat$se_test, 6),
          standard_error_ci = fmt(cdat$se_ci, 6),
          lower_bound = ifelse(is.na(cdat$ci_lo), "", fmt(cdat$ci_lo, 6)),
          upper_bound = ifelse(is.na(cdat$ci_hi), "", fmt(cdat$ci_hi, 6))
        ),
        defaultColDef = colDef(align = "right"),
        columns = list(
          sample_difference = colDef(name = "Sample Difference"),
          z_statistic = colDef(name = "Z-Statistic"),
          p_value = colDef(name = "p-Value"),
          standard_error_test = colDef(name = "SE (Test)"),
          standard_error_ci = colDef(name = "SE (CI)"),
          lower_bound = colDef(name = "Lower Bound"),
          upper_bound = colDef(name = "Upper Bound")
        )
      )
    }
  })

  output$pplot <- renderPlot({
    cdat <- calc()

    z <- cdat$z
    alt <- cdat$alt

    curve_df <- tibble(
      x = seq(-4, 4, length.out = 2000),
      y = dnorm(x)
    )

    z_abs <- abs(z)

    if (alt == "two.sided") {
      left_tail <- curve_df |> filter(x <= -z_abs)
      right_tail <- curve_df |> filter(x >= z_abs)
    } else if (alt == "greater") {
      shade_df <- curve_df |> filter(x >= z)
    } else {
      shade_df <- curve_df |> filter(x <= z)
    }

    y_peak <- max(curve_df$y)

    ggplot(curve_df, aes(x = x, y = y)) +
      {
        if (alt == "two.sided") {
          list(
            geom_area(data = left_tail, fill = "#F04E2A", alpha = 0.25),
            geom_area(data = right_tail, fill = "#F04E2A", alpha = 0.25)
          )
        } else {
          geom_area(data = shade_df, fill = "#F04E2A", alpha = 0.25)
        }
      } +
      geom_line(
        linewidth = 1.2,
        color = "#A90533"
      ) +
      {
        if (alt == "two.sided") {
          list(
            geom_segment(
              aes(x = -z_abs, xend = -z_abs, y = 0, yend = y_peak * 0.95),
              linetype = "dashed",
              color = "#F04E2A",
              linewidth = 0.9
            ),
            geom_segment(
              aes(x = z_abs, xend = z_abs, y = 0, yend = y_peak * 0.95),
              linetype = "dashed",
              color = "#F04E2A",
              linewidth = 0.9
            ),
            annotate(
              "text",
              x = 0,
              y = y_peak * 1.2,
              label = "Two-Sided p-Value",
              size = 5
            ),
            annotate(
              "text",
              x = -z_abs,
              y = -0.02 * y_peak,
              label = paste0("-", fmt(z_abs, 3)),
              color = "#A90533",
              vjust = 1,
              size = 5
            ),
            annotate(
              "text",
              x = z_abs,
              y = -0.02 * y_peak,
              label = fmt(z_abs, 3),
              color = "#A90533",
              vjust = 1,
              size = 5
            )
          )
        } else {
          list(
            geom_segment(
              aes(x = z, xend = z, y = 0, yend = y_peak * 0.95),
              linetype = "dashed",
              color = "#F04E2A",
              linewidth = 0.9
            ),
            annotate(
              "text",
              x = 0,
              y = y_peak * 1.2,
              label = "p-Value",
              size = 5
            ),
            annotate(
              "text",
              x = z,
              y = -0.02 * y_peak,
              label = fmt(z, 3),
              color = "#A90533",
              vjust = 1,
              size = 5
            )
          )
        }
      } +
      annotate(
        "text",
        x = 0,
        y = y_peak * 1.08,
        label = paste0("z = ", fmt(z, 4), ",  p-value = ", fmtp(cdat$pval)),
        color = "#A90533",
        size = 5
      ) +
      labs(
        x = "Standard Normal z",
        y = NULL
      ) +
      coord_cartesian(ylim = c(-0.08 * y_peak, 1.35 * y_peak)) +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.title = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        axis.text.y = element_blank(),
        axis.title.y = element_blank()
      )
  })
}

shinyApp(ui = ui, server = server)
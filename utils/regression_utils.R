library(broom)
library(broom.mixed)
library(dplyr)
library(data.table)
library(stringr)



prep_model_df <- function(model, data, dict = NULL, original_model = NULL) {

    if (is.null(original_model)) original_model = model

    terms = labels(terms(original_model))


    is_mixed = any(class(model) %in% c("lmerMod", "glmerMod"))

    clean = broom::tidy(model, conf.int = TRUE) %>%
        mutate(term = gsub("`", "", term))
    if (is_mixed) {
        clean = filter(clean, effect == "fixed")
        classes = sapply(getData(model), class)
    } else {
        classes = attr(terms(original_model), "dataClasses")
    }


    # class_attribute = ifelse(is_mixed, "dataClasses", "class")
    # # stop()
    # classes = attr(terms(model), class_attribute)



    fac_terms = names(classes)[classes == "factor"]
    other_terms = setdiff(terms, fac_terms)

    fac_classes = do.call(rbind, lapply(fac_terms, function(term) {
        data.frame(term = term, level = levels(data[[term]]), is_first = c(1, rep(NA, length(levels(data[[term]])) - 1)))
    }))



    df = data.frame(
        term = gsub("`", "", terms)
    ) %>%
        mutate(class = classes[term])
    if (length(fac_classes) > 0) {
        df = df %>%
            left_join(fac_classes, by = "term")
    } else {
        df = mutate(df, level = NA, is_first = NA)
    }
    df = df %>%
        mutate(reg_term = paste0(term, ifelse(is.na(level), "", level))) %>%
        left_join(clean, by = c("reg_term" = "term")) %>%
        mutate(reg_level = ifelse(is.na(level), term, level)) %>%
        mutate(reg_level = factor(reg_level, levels = unique(reg_level))) %>%
        mutate(group = ifelse(is.na(level), "Other", term)) %>%
        mutate(group = factor(group, levels = unique(group))) %>%
        mutate(
            lower = conf.low, #estimate - std.error * 1.96,
            upper = conf.high, #estimate + std.error * 1.96,
            is_significant = ifelse(lower > 0 | upper < 0, "Yes", "No")
        )

    return(df)
}


plot_subsummary_bar2 <- function(x, q, height, facet_x = NULL, facet_y = NULL) {
    dodge = position_dodge(width = 0.5)

    levs = levels(x$answer)
    even_inds = as.numeric(x$answer) %% 2 == 0
    x$text_vjust = (2 * even_inds - 0.5)
    adjustments = c(-0.75, -0.25, 1)
    x$text_vjust = x$text_vjust + adjustments[as.numeric(x$answer)]
    x$valueWrapped = strwrap_fac(x$value, width = 20)
    x$valueNum = as.numeric(x$valueWrapped)

    # stopifnot(is.factor(data$facet_y))
    y_vars = levels(x[[facet_y]])
    stopifnot(is.factor(x[[facet_y]]))

    ny = length(y_vars)

    x$y_val = x$valueNum + as.numeric(x[[facet_y]]) / (ny + 1)
    # stop()

    plt = ggplot(x, aes(x = mean, y = y_val, alpha = forcats::fct_rev(answer), fill = provtype)) +
        geom_bar(position="stack", stat="identity", width = 0.2, orientation = 'y') +
        geom_point(aes(x = pointmean), position = dodge, size = 1, show.legend = FALSE) +
        geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.5, position = dodge) +
        geom_text(
            aes(
                color = textColor,
                # vjust = text_vjust,
                vjust = -10,
                label = scales::percent_format(accuracy = 1)(pointmean),
                x = pointmean),
            size = 2,
            vjust = 2,
            hjust = 0.5,
            # position = dodge,
            show.legend = FALSE)
    # if (!is.null(facet_x) | !is.null(facet_y)) {
    #     facet_x = ifelse(is.null(facet_x), "", facet_x)
    #     facet_y = ifelse(is.null(facet_y), ".", facet_y)
    #     plt = plt +
    #         facet_grid(as.formula(paste0(facet_x, " ~ ", facet_y)), scales = "free", space = "free")
    # }
    plt = plt +
        scale_x_continuous(
                labels = scales::percent_format(accuracy = 1)) +
        xlab("Percent") +
        ylab("") +
        # ggtitle(paste0("Question: ", q)) +
        theme_bw() +
        theme(
            plot.title = element_text(hjust = 0.5),
            text = element_text(family = "Arial")) +
        # scale_fill_brewer(name = "Response", palette = "Greens", direction = -1) +
        scale_color_manual(values = c("black", "white"))


    ggsave(paste0("results/", q, ".png"), height = height, width = 7, dpi = 600)
    # ggsave(paste0("results/", q, ".pdf"), height = height, width = 7)
    # ggsave(paste0("results/", q, ".svg"), height = height, width = 7)
    # embed_fonts(paste0("results/", q, ".pdf"))
    plt
}


plot_subsummary_bar <- function(x, q,
        height, facet_x = NULL, facet_y = NULL, reverse = FALSE,
        scale_name = "Response",
        scale_palette = "Greens") {
    dodge = position_dodge(width = 0.5)

    print(x$mean)
    print(x$upper)

    if (reverse) {
        x$answer = forcats::fct_rev(x$answer)
        # x$mean = 1 - x$mean
        upper = x$upper
        x$upper = 1 - x$lower
        x$lower = 1 - upper
        x$pointmean = 1 - x$pointmean
        color_direction = 1
    } else {
        color_direction = -1
    }

    levs = levels(x$answer)

    odd_inds = as.numeric(x$answer) %% 2 == 1
    x$text_vjust = case_when(
        as.numeric(x$answer) == 1 ~ 1,
        as.numeric(x$answer) == 2 ~ -1.5,
        as.numeric(x$answer) == 3 ~ 1.5,
        as.numeric(x$answer) == 4 ~ 0,
        as.numeric(x$answer) == 5 ~ 3,
        TRUE ~ 0
    )
    # 4 * (odd_inds - 0.5) + 0.6
    # adjustments = c(-0.8, -0.6, -0.4, -0.2, 0)
    # x$text_vjust = x$text_vjust + adjustments[as.numeric(x$answer)]
    x$valueWrapped = strwrap_fac(x$value, width = 25)
    # x$text_y = as.numeric(x$valueWrapped)


    # print(x$text_y)




    plt = ggplot(x, aes(x = mean, y = valueWrapped, fill = forcats::fct_rev(answer))) +
        geom_bar(position="stack", stat="identity") +
        geom_point(aes(x = pointmean), position = dodge, size = 1, show.legend = FALSE) +
        geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.5, position = dodge) +
        geom_text(
            aes(
                color = textColor,
                vjust = text_vjust,
                # y = text_y,
                label = scales::percent_format(accuracy = 1)(pointmean),
                x = pointmean),
            size = 2,
            # vjust = 2,
            hjust = 0.5,
            # position = dodge,
            show.legend = FALSE)
    if (!is.null(facet_x) | !is.null(facet_y)) {
        facet_x = ifelse(is.null(facet_x), "", facet_x)
        facet_y = ifelse(is.null(facet_y), ".", facet_y)
        print(facet_y)
        plt = plt +
            facet_grid(
                as.formula(paste0(facet_x, " ~ ", facet_y)), scales = "free", space = "free",
                labeller = labeller(!!sym(facet_x) := label_wrap_gen(15)))
    }
    plt = plt +
        scale_x_continuous(
                labels = scales::percent_format(accuracy = 1)) +
        xlab("Cumulative Percent") +
        ylab("") +
        # ggtitle(paste0("Question: ", q)) +
        theme_bw() +
        theme(
            plot.title = element_text(hjust = 0.5),
            text = element_text(family = "Arial")) +
        scale_fill_brewer(name = scale_name, palette = scale_palette, direction = color_direction) +
        scale_color_manual(values = c("black", "white"))


    ggsave(paste0("results/", q, ".png"), height = height, width = 8, dpi = 600)
    # ggsave(paste0("results/", q, ".pdf"), height = height, width = 7)
    # ggsave(paste0("results/", q, ".svg"), height = height, width = 7)
    # embed_fonts(paste0("results/", q, ".pdf"))
    plt
}

# plot_dfs(data_2020, "Information Sources 2020")

weighted_propci <- function(x, w) {
    # x = design$variables[[var]]
    if (length(x) == 0) {
        stop("No observations for variable")
    }
    if (is.character(x)) {
        stopifnot(all(unique(x) %in% c("0", "1", "")))
        x = as.numeric(x)
    }

    obs_inds = !is.na(x)
    x = x[obs_inds]
    w = w[obs_inds]

    n = length(x)
    stopifnot(length(w) == n)
    sw = sum(w)

    neff = sw * sw / sum(w * w)
    mu = sum(x * w) / sw

    za = qnorm(1 - 0.05 / 2)
    z2 = za * za
    pm = za / (2 * neff) * sqrt(4 * neff * mu * (1 - mu) + z2)
    ci = (1 / (1 + z2 / neff)) * (mu + z2 / (2 * neff) + c(-1, 1) * pm)

    # ci = wilson.ci(mu * neff, neff)
    c(mean = mu, lower = ci[1], upper = ci[2])
}

prep_subsummary <- function(
    dt, q, bys, use_weight = FALSE, weight_var = "weight"
) {
    responses = levels(dt[[q]])
    bydts = lapply(bys, function(by) {

        if (is.null(by)) {
            # stop()
        } else if (is.vector(by) & length(by) > 1) {
            new_by = paste(by, collapse=' by ')
            dt[[new_by]] = do.call(paste, c(dt[, by, with = FALSE], sep = ': '))
            by = new_by
        }
        dt$q = dt[[q]]

        stopifnot(all(names(responses) == 1:4))
        bydt = dt

        # ws = ifelse(use_weight, dt[[weight_var]], rep(1, nrow(dt)))
        if (!use_weight) {
            bydt[[weight_var]] = 1
        }



        if (!is.null(by)) {
            bydt = bydt %>%
                group_by(!!sym(by))
        }



        bydt = bydt %>%
            reframe(ord_perc(q, !!sym(weight_var), responses)) %>%
            ungroup()



        if (is.null(by)) {
            by = "Overall"
            bydt = cbind(data.table(Overall = rep(by, nrow(bydt))), bydt)
            bydt$variable = "Ovrl."
        } else {
            bydt$variable = by
        }


        # rename column
        setnames(bydt, by, "value")
        bydt
    })

    bydt = do.call(rbind, bydts)
    # bydt$answer = factor(bydt$anser, levels = responses)
    bydt$variable = factor(bydt$variable, levels = unique(bydt$variable))
    bydt
    # %>%
    #     filter(value != "NA") %>%
    #     mutate(pretty = make_pretty(as.character(variable), dict))
}


ord_perc <- function(x, w, responses) {
    x = as.numeric(x)
    df = data.frame(
        level = 1:length(responses),
        answer = factor(responses, levels = responses),
        # at_most = c(1, 2, 3 ),
        mean = NA,
        lower = NA,
        upper = NA,
        pointmean = NA
    )
    for (i in 1:nrow(df)) {
        if (i < nrow(df)) {
            y = as.numeric(x <= df$level[i])
            percs = weighted_propci(y, w)
            df$pointmean[i] = percs['mean']
            df$lower[i] = percs['lower']
            df$upper[i] = percs['upper']
            df$textColor[i] = df$answer[i] == "Some"
        }


        z = as.numeric(x == df$level[i])
        df$mean[i] = weighted_propci(z, w)['mean']
    }
    df
}


make_pretty <- function(x, dict) {
    print(x)
    if (is.factor(x)) {
        levs = levels(x)
        x = as.character(x)
    } else {
        levs = unique(x)
    }


    for (i in seq_along(levs)) {
        if (levs[i] %in% dict$variable) {
            lev = dict$pretty[dict$variable == levs[i]]
            x[x == levs[i]] = lev
            levs[i] = lev
            # print(dict$variable == x[i])
            # print(dict$pretty[dict$variable == x[i]])
            # x[i] = dict$pretty[dict$variable == x[i]]
            # print(x[i])
            # stop()
        }
    }
    factor(x, levels = levs)
}

strwrap_fac <- function(x, width = 20) {
    # print(x)
    if (!is.factor(x)) {
        x = factor(x, levels = unique(x))
    }
    levs = levels(x)

    factor(str_wrap(x, width = width), levels = str_wrap(levs, width = width))
}
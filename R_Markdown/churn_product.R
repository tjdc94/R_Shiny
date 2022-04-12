source('~/Desktop/Signal_Redshift_Conn.R')
library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)
library(tidyr)
library(scales)

qry <- "

WITH salesforce_meta AS (

	SELECT DISTINCT
		dim_organisations.salesforce_id,
		CASE 
			WHEN dim_organisations.industry_vertical IN ('Agency', 'Financial', 'NGO/Non-profit') 
			THEN dim_organisations.industry_vertical 
			ELSE 'Other' 
		END as industry_vertical,
		dim_organisations.region,
		dim_organisations.annual_revenue_band
	FROM
		analytics.dim_organisations
		INNER JOIN analytics.current_organisations ON current_organisations.id = dim_organisations.natural_key
	WHERE
		dim_organisations.is_current
		AND NOT dim_organisations.is_ecosystem_client

),

platform_minutes AS (

	SELECT
		agg.day as full_date,
		dim_organisations.salesforce_id,
		dim_organisations.natural_key,
	  	COALESCE(SUM(agg.user_active_minutes_total) OVER (PARTITION BY dim_organisations.natural_key ORDER BY agg.day ROWS BETWEEN 89 PRECEDING AND CURRENT ROW), 0) AS platform_minutes_90day,
	  	COALESCE(SUM(agg.emails_delivered_count) OVER (PARTITION BY dim_organisations.natural_key ORDER BY agg.day ROWS BETWEEN 89 PRECEDING AND CURRENT ROW), 0) AS emails_delivered_90day
	FROM
		analytics.fct_aggregate_product_usage_by_organisation_daily as agg
		INNER JOIN analytics.dim_organisations ON dim_organisations.surrogate_key = agg.organisations_sk

),

total_platform_minutes AS (

	SELECT
		full_date,
		salesforce_id,
		SUM(platform_minutes_90day)::INTEGER as platform_minutes_90day,
		SUM(emails_delivered_90day)::INTEGER as emails_delivered_90day
	FROM
		platform_minutes
	GROUP BY 1,2

),

spine AS (

	SELECT
		current_sf_opportunities.id as opportunity_id,
		current_sf_opportunities.service_start_date,
		current_sf_opportunities.closed_date,
		current_sf_opportunities.stage_name,
		current_sf_opportunities.salesforce_id,
		current_sf_opportunities.organisation_account_names,
		round(current_sf_opportunities.arr_value_gbp, 0)::INTEGER as arr_value_gbp,
		current_sf_opportunities.primary_reason_for_loss,
		current_sf_opportunities.comments
	FROM
		analytics.current_sf_opportunities
	WHERE
		current_sf_opportunities.stage_name IN ('Closed - Lost', 'Closed - Won')
		AND current_sf_opportunities.opportunity_type = 'Renewal'
		-- AND (current_sf_opportunities.primary_reason_for_loss ILIKE 'Functionality%' or current_sf_opportunities.primary_reason_for_loss ILIKE 'Content%')
		AND current_sf_opportunities.closed_date >= '2020-01-01'::DATE
		AND current_sf_opportunities.arr_value_gbp > 0
		
),

users_at_renewal AS (

	SELECT
		spine.opportunity_id,
		spine.salesforce_id,
		spine.closed_date,
		COUNT(DISTINCT dim_users.natural_key)::INTEGER as external_user_count
	FROM
		spine
		INNER JOIN analytics.dim_organisations ON dim_organisations.salesforce_id = spine.salesforce_id
			AND dim_organisations.valid_from <= spine.closed_date - interval '1 DAYS'
			AND dim_organisations.valid_to > spine.closed_date - interval '1 DAYS'
			AND dim_organisations.is_activated
			AND NOT dim_organisations.state IN ('INTERNAL')
		INNER JOIN analytics.dim_users ON dim_users.organisations_natural_key = dim_organisations.natural_key
			AND dim_users.valid_from <= spine.closed_date - interval '1 DAYS'
			AND dim_users.valid_to > spine.closed_date - interval '1 DAYS'
			AND dim_users.is_activated
			AND dim_users.has_platform_access
			AND NOT dim_users.is_internal_user
	GROUP BY
		1,2,3

)

SELECT
	spine.*,
	salesforce_meta.region,
	total_platform_minutes.platform_minutes_90day::INTEGER,
	total_platform_minutes.emails_delivered_90day::INTEGER,
	COALESCE(users_at_renewal.external_user_count, 0)::INTEGER as external_users_at_renewal
FROM
	spine
	INNER JOIN salesforce_meta ON salesforce_meta.salesforce_id = spine.salesforce_id
	-- User count at renewal
	LEFT JOIN users_at_renewal ON users_at_renewal.opportunity_id = spine.opportunity_id
	-- Product
	LEFT JOIN total_platform_minutes ON total_platform_minutes.full_date = spine.closed_date
		AND total_platform_minutes.salesforce_id = spine.salesforce_id
ORDER BY
	closed_date DESC;

"

data <- dbGetQuery(pconn_r, qry)

as.tbl(data)

d <- data %>% 
  mutate(
    reason_for_loss = case_when(
      stage_name == 'Closed - Won' ~ 'Closed - Won',
      stage_name == 'Closed - Lost' ~ paste0('Loss: ', str_extract(primary_reason_for_loss, "\\w+")),
    ),
    primary_reason_for_loss = ifelse(stage_name == 'Closed - Won', 'Won', primary_reason_for_loss)
  )

bg <- "#F9F9F9"
p <- ggplot(data = d %>% filter(reason_for_loss %in% c('Closed - Won', 'Loss: Content', 'Loss: Functionality'))) +
  geom_point(
    aes(
      x = emails_delivered_90day, 
      y = platform_minutes_90day,
      fill = reason_for_loss,
      size = arr_value_gbp,
      alpha = reason_for_loss,
      text = paste(
        paste0(organisation_account_names, ' (', salesforce_id, ')'), 
        paste(paste('£', format(round(arr_value_gbp), big.mark = ','), sep = "")), 
        paste("Emails Delivered:", format(emails_delivered_90day, big.mark = ',')),
        paste("Platform Minutes:", format(platform_minutes_90day, big.mark = ',')),
        paste("------------"),
        paste0(primary_reason_for_loss),
        paste0("Notes: ", str_wrap(comments, 45)),
        sep = "\n"
      )
  ), pch = 21, col = '#FFFFFF') +
  scale_x_continuous(trans='log10', labels = scales::comma) +
  scale_y_continuous(trans='log10') +
  scale_size_continuous(range = c(2, 20)) +
  scale_fill_manual(
    values = c('Closed - Won' = 'grey', 'Loss: Content' = '#3a6dee', 'Loss: Functionality' = '#45ceac')
  ) +
  scale_alpha_manual(
    values = c('Closed - Won' = .3, 'Loss: Content' = 1, 'Loss: Functionality' = 1)
  ) +
  labs(fill = "") +
  xlab("Emails Delivered") + ylab("Platform Minutes") + 
  theme(panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = bg, colour = bg),
        panel.background = element_rect(fill = bg, colour = bg),
        axis.text = element_text(size = 12, colour = 'grey30'),
        axis.title = element_text(size = 14, colour = 'grey30'),
        plot.title = element_text(size = 18, hjust = .5),
        plot.subtitle = element_text(size = 14),
        axis.line = element_line(colour = 'black'),
        legend.background = element_rect(fill = bg),
        legend.title = element_blank(),
        text = element_text(family = "Andale Mono")) +
  guides(alpha = FALSE, fill = guide_legend(override.aes = list(size = 5)))


ggplotly(p,  tooltip = c('text')) %>%
  plotly::config(displayModeBar = F) %>%
  layout(
    title = list(text = paste0('<br>Emails Delivered & Platform Minutes', '<br>','<sup>Data 90 Days Prior to Loss</sup>')),
    xaxis = list(showspikes = T, spikecolor = 'grey30', spikesides = FALSE, spikethickness = 1, spikemode = 'across'),
    yaxis = list(showspikes = T, spikecolor = 'grey30', spikesides = FALSE, spikethickness = 1, spikemode = 'across'),
    hoverlabel = list(font = list(family = 'Andale Mono', size = 12), bgcolor = bg)
  ) %>% 
  style(hoverinfo = "skip", traces = 1)

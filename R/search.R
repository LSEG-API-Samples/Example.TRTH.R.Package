historical_criteria <- function(start_date_time, end_date_time, ...) {
  # Build URL
  url <- sprintf("%s/Search/HistoricalCriteriaSearch", getOption("dss_url"))
}

fixed_income_criteria <- function(bond_types = NULL,
                                  countries = NULL,
                                  expiry_month = NULL,
                                  coupon_rate = NULL,
                                  coupon_rate_min = NULL,
                                  coupon_rate_max = NULL,
                                  maturity_date_start = NULL,
                                  maturity_date_end = NULL
                                  ) {
  criteria_body(bond_type_codes = bond_types,
                country_codes = countries,
                option_month_codes = expiry_month,
                coupon_rate = coupon_rate,
                coupon_rate_min = coupon_rate_min,
                coupon_rate_max = coupon_rate_max,
                maturity_date_start = maturity_date_start,
                maturity_date_end = maturity_date_end)
}

options_criteria <- function(option_type_codes = NULL,
                             strike_price = NULL,
                             strike_price_min = NULL,
                             strike_price_max = NULL,
                             expiry_date_start = NULL,
                             expiry_date_end = NULL
                             ) {
  criteria_body(option_type_codes = NULL,
                strike_price = NULL,
                strike_price_min = NULL,
                strike_price_max = NULL,
                expiry_date_start = NULL,
                expiry_date_end = NULL)
}

criteria_body <- function(start_date_time = NULL,
                          end_date_time = NULL,
                          bond_type_codes = NULL,
                          contributor_ids = NULL,
                          country_codes = NULL,
                          currency_codes = NULL,
                          exchange_codes = NULL,
                          future_month_codes = NULL,
                          instrument_type_codes = NULL,
                          option_month_codes = NULL,
                          option_type_codes = NULL,
                          coupon_rate = NULL,
                          coupon_rate_min = NULL,
                          coupon_rate_max = NULL,
                          strike_price = NULL,
                          strike_price_min = NULL,
                          strike_price_max = NULL,
                          expiry_date_start = NULL,
                          expiry_date_end = NULL,
                          maturity_date_start = NULL,
                          maturity_date_end = NULL
                          ) {
  me <- list(
    Request=list(
      Range=list(
        Start=start_date_time,
        End=end_date_time
      )
    )
  )
  if (!is.null(bond_type_codes)) {
    me$Request$BondTypeCodes <- I(as.character(bond_type_codes))
  }
  if (!is.null(contributor_ids)) {
    me$Request$ContributorIds <- I(as.character(contributor_ids))
  }
  if (!is.null(country_codes)) {
    me$Request$CountryCodes <- I(as.character(country_codes))
  }
  if (!is.null(currency_codes)) {
    me$Request$CurrencyCodes <- I(as.character(currency_codes))
  }
  if (!is.null(exchange_codes)) {
    me$Request$ExchangeCodes <- I(as.character(exchange_codes))
  }
  if (!is.null(future_month_codes)) {
    me$Request$FutureMonthCodes <- I(as.character(future_month_codes))
  }
  if (!is.null(instrument_type_codes)) {
    me$Request$InstrumentTypeCodes <- I(as.character(instrument_type_codes))
  }
  if (!is.null(option_month_codes)) {
    me$Request$OptionMonthCodes <- I(as.character(option_month_codes))
  }
  if (!is.null(option_type_codes)) {
    me$Request$OptionTypeCodes <- I(as.character(option_type_codes))
  }
  if (!is.null(coupon_rate)) {
    me$Request$CouponRate$Value <- coupon_rate
  }
  if (!is.null(coupon_rate_min)) {
    me$Request$CouponRate$Min <- coupon_rate_min
  }
  if (!is.null(coupon_rate_max)) {
    me$Request$CouponRate$Max <- coupon_rate_max
  }
  if (!is.null(strike_price)) {
    me$Request$StrikePrice$Value <- strike_price
  }
  if (!is.null(strike_price_min)) {
    me$Request$StrikePrice$Min <- strike_price_min
  }
  if (!is.null(strike_price_max)) {
    me$Request$StrikePrice$Max <- strike_price_max
  }
  if (!is.null(expiry_date_start)) {
    me$Request$ExpiryDate$Start <- expiry_date_start
  }
  if (!is.null(expiry_date_end)) {
    me$Request$ExpiryDate$End <- expiry_date_end
  }
  if (!is.null(maturity_date_start)) {
    me$Request$MaturityDate$Start <- maturity_date_start
  }
  if (!is.null(maturity_date_end)) {
    me$Request$MaturityDate$End <- maturity_date_end
  }


  class(me) <- "criteria_search_body"
  return(me)
}

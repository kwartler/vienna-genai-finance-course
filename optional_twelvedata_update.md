# README update: Twelve Data (optional)

Drop these blocks into the course README wherever they fit. Two placements are recommended:

1. A short bullet in the pre-module environment setup list, marked optional.
2. A dedicated section under "APIs and services" (or similar) explaining what the service is, when to use it, and how it compares to FinancialModelingPrep (which is already in the pre-module setup).

---

## Suggested pre-module setup bullet

Add this line to the pre-module Environment Setup list alongside the existing newsapi.org, financialmodelingprep, and Google AI Studio entries:

> - **(Optional)** Create a free account at https://twelvedata.com and generate an API key from your dashboard. Twelve Data is an alternative to FinancialModelingPrep for fetching daily OHLC bars in the browser side of the SPA builds. You only need one of the two. Either way, save the key as `TWELVE_DATA` in `~/.Renviron` and (with a `VITE_` prefix) in the SPA project's `.env`. Never commit either file.

---

## Suggested "APIs and services" section

### Price data for the SPA builds: Twelve Data or FinancialModelingPrep

**Why this section exists:** the morning R work fetches prices from Yahoo Finance through `quantmod`, which is comfortable inside R but cannot be called from a browser tab (CORS blocks it). Our Repo 1 and Repo 2 single page applications need the same daily OHLC bars from JavaScript, so they call a browser friendly REST endpoint instead. Two services will work, and you only need one.

**Option A: FinancialModelingPrep (already in the pre-module setup).** Historical daily OHLC available on the free tier. Students already have a key from Day 1 setup, so this is the zero extra effort path.

**Option B: Twelve Data (optional alternative).** Free Basic plan grants 800 API credits per day and 8 per minute. One `/time_series` request costs 1 credit. Slightly cleaner response shape for classroom use and a generous free tier if a student runs into FMP rate limits.

Both return the same OHLC bars for the same trading day within a small rounding tolerance. Pick whichever your SPA fetches from and keep the other as a fallback.

**If you choose Twelve Data:**

- Sign up: https://twelvedata.com
- Endpoint: `https://api.twelvedata.com/time_series` with `interval=1day`, a `symbol`, and a date range.
- Environment variable name: `TWELVE_DATA`

Setup for R work: add to `~/.Renviron` and restart RStudio:

```
TWELVE_DATA=your_key_here
```

Setup for SPA work: add to your project's `.env` file (already gitignored in the vibe-genai-spa-template):

```
VITE_TWELVE_DATA=your_key_here
```

The Vite `VITE_` prefix is required so the key is exposed to the browser build. Without it, Vite strips the variable out at build time.

**Reference script:** `lessons/day1/day1_scripts/twelvedata_ohlc.R` fetches daily OHLC for one ticker and one date range, prints the first and last five rows, and points out how the response compares to the Yahoo output in `A_data_prep.R`. Run it once with your own key on Day 1 if you plan to route your SPA through Twelve Data, so the browser side does not surprise you later.

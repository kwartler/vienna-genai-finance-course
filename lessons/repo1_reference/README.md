# Signal Explainer, Repo 1 Reference

This is a working implementation of the Day 1 afternoon build (14:00 to 17:00), matching the functional spec in the course syllabus: a form for ticker, moving average windows, RSI period, and an OpenRouter API key, three charts (moving average crossover, MACD, RSI), and an AI generated research note.

## An important constraint: CORS

A browser blocks a `fetch()` to any API that does not send CORS headers, unlike the R scripts this morning, which ran server side and never hit this restriction. Yahoo Finance's chart endpoint does not send those headers, so this app uses Financial Modeling Prep instead, which does. That lets `main.js` call it directly from the browser, with no proxy.

Enter your FMP key in the app's "Financial Modeling Prep API key" field, alongside the OpenRouter key. Get a free key from your FMP dashboard at https://site.financialmodelingprep.com/. The key stays in the browser and is not committed to the repo.

## A known inconsistency worth knowing about

The RSI worked example in this morning's Technical Indicators in R materials divides average gain and average loss by the count of up days or down days specifically. The actual formula, and what this app and `TTR::RSI()` in R both compute, divides by the full window size instead. Both land in a similar place directionally, but the numbers will not match exactly if a student checks the arithmetic by hand against this morning's slide.

## Running locally

Step by step, the first time:

1. Open a terminal. On a Mac, press `Cmd + Space`, type `Terminal`, and press `Enter`.

2. Change into this project folder, the one that contains `package.json`. For example:

   ```
   cd ~/Desktop/vienna-genai-finance-course/lessons/repo1_reference
   ```

3. Install the dependencies. You only need to do this once:

   ```
   npm install
   ```

4. Start the local development server:

   ```
   npm run dev
   ```

5. The terminal prints a local address, usually `http://localhost:5173`. Open that address in your web browser. The page reloads automatically every time you save a file.

To stop the server, click back on the terminal and press `Ctrl + C`.

## Deploying

Push a version tag to trigger the GitHub Actions build and publish to GitHub Pages:

```
git tag v0.1.0
git push origin v0.1.0
```

## Where this fits

This is a complete, working reference implementation, not the blank scaffold students clone at the start of class. Decide where it lives before committing:

- As the instructor's live demo, kept separate from `vienna-genai-spa-template` (Repo B), which should stay a blank starting point so the building exercise is not spoiled for students who look at the repo.
- Or, if you are comfortable with students seeing a finished example ahead of time, merged into the shared template repo directly.

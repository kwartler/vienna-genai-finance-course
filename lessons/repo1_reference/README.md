# Signal Explainer, Repo 1 Reference

This is a working implementation of the Day 1 afternoon build (14:00 to 17:00), matching the functional spec in the course syllabus: a form for ticker, moving average windows, RSI period, and an OpenRouter API key, three charts (moving average crossover, MACD, RSI), and an AI generated research note.

## An important constraint: CORS

Yahoo Finance's chart endpoint does not send CORS headers, so a browser based `fetch()` call gets blocked by the browser, unlike the R scripts this morning, which ran server side and never hit this restriction. `main.js` routes the request through a public CORS proxy (`corsproxy.io`) to work around this.

Public proxies can be slow or rate limited, especially with a full class hitting them around the same time. Test this the morning of class. If it is unreliable, the easiest fallback is swapping in a different proxy such as `https://api.allorigins.win/raw?url=`, changing only the `PROXY` constant at the top of `main.js`.

## A known inconsistency worth knowing about

The RSI worked example in this morning's Technical Indicators in R materials divides average gain and average loss by the count of up days or down days specifically. The actual formula, and what this app and `TTR::RSI()` in R both compute, divides by the full window size instead. Both land in a similar place directionally, but the numbers will not match exactly if a student checks the arithmetic by hand against this morning's slide.

## Running locally

```
npm install
npm run dev
```

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

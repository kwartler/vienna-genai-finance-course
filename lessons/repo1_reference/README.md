# Repo 1, Signal Explainer (reference build)

The finished single page application for Repo 1, built up from the course template. Enter a ticker, MACD and RSI windows, and your two API keys, then click Analyze. The app pulls daily OHLC from Twelve Data, computes MACD and RSI in the browser, draws a candlestick chart plus separate MACD and RSI plots, and asks OpenRouter for a research note on the current signals.

This is the reference students compare against after vibe coding their own version from the template.

> **⚠️ First-run setup (do this once): in your repo go to Settings → Pages and set Source to "GitHub Actions".** Without it your deployed page will be broken or unstyled. Full steps in [SETUP.md](SETUP.md).

## Local development

Step by step, the first time:

1. Open a terminal. On a Mac, press `Cmd + Space`, type `Terminal`, and press `Enter`.

2. Change into the folder you cloned, the one that contains `package.json`. For example, if it is on your Desktop:

   ```bash
   cd ~/Desktop/your-repo-name
   ```

3. Install the dependencies. You only need to do this once:

   ```bash
   npm install
   ```

4. Start the local development server:

   ```bash
   npm run dev
   ```

5. The terminal prints a local address, usually `http://localhost:5173`. Open that address in your web browser. The page reloads automatically every time you save a file.

To stop the server, click back on the terminal and press `Ctrl + C`.

## API keys

This app calls two services, and needs a key for each before it returns anything. **Both keys are entered in the app's form fields at run time. Neither is stored in the code.**

1. **Twelve Data (price data):** get a free key at https://twelvedata.com/pricing. The free plan covers all US stocks and ETFs, capped at 8 requests per minute and 800 per day.
2. **OpenRouter (the AI research note):** get a key at https://openrouter.ai/.

Because no key is ever written into a file or committed, this repo is safe to make public and the deployed page is safe to share, including with prospective employers. Each visitor supplies their own keys, which stay in their browser tab only and are cleared on reload.

> Note: because this is a static app with no server, a typed key is sent straight from the browser to Twelve Data/OpenRouter over HTTPS while the app runs. That is fine for a classroom or portfolio demo. A production app would add a backend proxy so keys never reach the browser at all.

## Deploying to GitHub Pages

Every push to `main` builds the app and redeploys it automatically. No tags or version bumps needed.

```bash
git add .
git commit -m "your change"
git push
```

The site goes live at `https://<your-username>.github.io/<your-repo-name>/` about a minute later. You can also trigger a redeploy manually from the repo's **Actions** tab (Build and Deploy, "Run workflow").

### One-time setup (do this once per repo)

In your repo on GitHub: **Settings, then Pages, then set Source to "GitHub Actions".**

If Source is left on "Deploy from a branch," the build runs but its output is ignored and you will see a broken or unstyled page.

## Notes

- Asset paths in `index.html` are relative (`./style.css`, `./main.js`) and `vite.config.js` sets `base: './'`. This is what makes the site work under the `/<repo-name>/` subpath that GitHub Pages uses. Do not change these to start with a leading `/`.

# Generative AI in Finance — Instructor Repository

Executive MBA | Finance & Analytics Joint Cohort  
Instructor: Ted Kwartler

---

## Before Day 1 — Student Checklist

### Must Have Installed

| Tool | Mac | Windows |
|------|-----|---------|
| Git | [git-scm.com/downloads](https://git-scm.com/downloads) | [git-scm.com/downloads](https://git-scm.com/downloads) |
| R + RStudio | [posit.co/download/rstudio-desktop](https://posit.co/download/rstudio-desktop/) | [posit.co/download/rstudio-desktop](https://posit.co/download/rstudio-desktop/) |
| Node.js LTS | [nodejs.org](https://nodejs.org) | [nodejs.org](https://nodejs.org) |

### Must Have Accounts

| Service | URL | Notes |
|---------|-----|-------|
| GitHub | [github.com](https://github.com) | Free |
| newsapi.org | [newsapi.org](https://newsapi.org) | Free dev tier — save your API key |
| Financial Modeling Prep | [financialmodelingprep.com](https://financialmodelingprep.com) | Free tier — save your API key |
| Twelve Data | [twelvedata.com](https://twelvedata.com) | Free Basic plan (alternative to FMP for OHLC data), save your API key |
| Google AI Studio | [aistudio.google.com](https://aistudio.google.com) | Free — generate a Gemini API key and save it |

---

## Installation Guides

### Git

**Mac**
1. Go to [git-scm.com/downloads](https://git-scm.com/downloads) and click the Mac download
2. Open the `.dmg` and run the installer — follow the prompts
3. Open Terminal and run `git --version` to confirm it installed

> **Mac shortcut:** If you have Homebrew installed, you can run `brew install git` instead.

**Windows**
1. Go to [git-scm.com/downloads](https://git-scm.com/downloads) and click the Windows download
2. Run the `.exe` installer — accept all defaults
3. When asked about the default editor, choose whatever you are comfortable with (Notepad is fine)
4. Open PowerShell and run `git --version` to confirm

> **Windows tip:** Git for Windows also installs Git Bash. You do not need to use it — all Git commands in this course can be run from the **RStudio Terminal tab**, which works identically on Mac and Windows.

---

### R + RStudio

**Mac**
1. Go to [posit.co/download/rstudio-desktop](https://posit.co/download/rstudio-desktop/)
2. First download and install **R 4.x** using the macOS `.pkg` link
3. Then download and install **RStudio Desktop** using the macOS `.dmg` link
4. Open RStudio — if it launches without errors, you are done

**Windows**
1. Go to [posit.co/download/rstudio-desktop](https://posit.co/download/rstudio-desktop/)
2. First download and install **R 4.x** using the Windows `.exe` link
3. Then download and install **RStudio Desktop** using the Windows `.exe` link
4. Open RStudio — if it launches without errors, you are done

> **Note:** You must install R before RStudio. RStudio is just the editor — it needs R to be present to function.

---

### Node.js

**Mac**
1. Go to [nodejs.org](https://nodejs.org) and click the **LTS** download
2. Open the `.pkg` file and follow the installer prompts
3. Open Terminal and run `node --version` and `npm --version` to confirm both installed

**Windows**
1. Go to [nodejs.org](https://nodejs.org) and click the **LTS** download
2. Run the `.msi` installer — accept all defaults
3. Open PowerShell and run `node --version` and `npm --version` to confirm both installed

> **Windows note:** If you are on a managed corporate laptop and the installer is blocked, download the `.zip` version instead and add the extracted folder to your system PATH. Ask a TA for help if needed.

---

## Day 1 Flow — Getting Started in Class

Follow these steps at the start of Day 1 to get your personal copy of the course repo set up in RStudio.

**Step 1 — Use the template**
1. Go to [github.com/kwartler/vienna-genai-finance-course](https://github.com/kwartler/vienna-genai-finance-course)
2. Click the green **Use this template** button → **Create a new repository**
3. Give your repo a name (e.g. `genai-finance-[yourname]`) and click **Create repository**

**Step 2 — Clone into RStudio**
1. On your new repo page, click the green **Code** button and copy the HTTPS URL
2. Open RStudio
3. Go to **File → New Project → Version Control → Git**
4. Paste your repo URL, choose a local folder, and click **Create Project**

RStudio will clone the repo and open it as a project automatically. Your working directory will be set to the repo root — all relative paths in the course R scripts will work without any changes.

**Step 3 — Verify your setup**
Open the RStudio Terminal tab (not the Console) and run:
```
git --version
node --version
npm --version
```
All three should return version numbers. If any fail, flag a TA before the session begins.

**Step 4 — Add your API keys**
Open `keys.example.md` in the repo for instructions on where to store your API keys. Never paste real API keys directly into R scripts or commit them to GitHub.

---

## Repo Structure

- `syllabus/` — course syllabus (docx)
- `lessons/` — instructor R scripts and exercise guides per session
  - `day1-r-indicators/` — moving averages, MACD, RSI scripts
  - `day1-prompt-engineering/` — prompt and system-prompt exercises
  - `day2-finance/` — context, news, sentiment and NER guides
  - `day2-datascience/` — portfolio optimization and rolling metrics scripts
- `spa-template/` — Vite vanilla scaffold with GitHub Actions deploy workflow
- `keys.example.md` — notes on where to store API keys (never commit real keys)


## SPA Template

The application scaffold (Vite files + GitHub Actions release workflow) lives in a separate repo, kept as the single source of truth: https://github.com/kwartler/vienna-genai-spa-template. Use that template for Repo 1, 2, and 3 builds. Do not duplicate the scaffold here.

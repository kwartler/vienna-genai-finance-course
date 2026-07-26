import Chart from 'chart.js/auto';

// Financial Modeling Prep sends CORS headers, so the browser can fetch it
// directly with no proxy. The FMP key is entered in the form field below.
// Get a free key from your FMP dashboard at https://site.financialmodelingprep.com/.

const form = document.getElementById('signal-form');
const statusEl = document.getElementById('status');
const researchNote = document.getElementById('research-note');

let maChart, macdChart, rsiChart;

form.addEventListener('submit', async (event) => {
  event.preventDefault();

  const ticker = document.getElementById('ticker').value.trim().toUpperCase();
  const fastWindow = parseInt(document.getElementById('ma-fast').value, 10);
  const slowWindow = parseInt(document.getElementById('ma-slow').value, 10);
  const rsiPeriod = parseInt(document.getElementById('rsi-period').value, 10);
  const fmpKey = document.getElementById('fmp-key').value.trim();
  const apiKey = document.getElementById('openrouter-key').value.trim();

  if (fastWindow >= slowWindow) {
    setStatus('The fast window must be smaller than the slow window.', true);
    return;
  }

  setStatus('Fetching price data...');
  researchNote.innerHTML = '<p class="placeholder">Working on it...</p>';

  try {
    const candles = await fetchOhlcv(ticker, fmpKey);
    const closes = candles.map((c) => c.close);

    setStatus('Calculating indicators...');
    const fastMa = SMA(closes, fastWindow);
    const slowMa = SMA(closes, slowWindow);
    const macd = MACD(closes, fastWindow, slowWindow, 9);
    const rsi = RSI(closes, rsiPeriod);

    renderCharts(candles, fastMa, slowMa, macd, rsi);

    setStatus('Asking the model to explain the current signals...');
    const latest = {
      ticker,
      close: lastValid(closes),
      fastMa: lastValid(fastMa),
      slowMa: lastValid(slowMa),
      macd: lastValid(macd.macdLine),
      macdSignal: lastValid(macd.signalLine),
      rsi: lastValid(rsi),
      fastWindow,
      slowWindow,
      rsiPeriod,
    };
    const aiResult = await getResearchNote(latest, apiKey);
    renderResearchNote(aiResult);

    setStatus('Done.');
  } catch (err) {
    setStatus(`Something went wrong: ${err.message}`, true);
    researchNote.innerHTML = `<p class="error">${err.message}</p>`;
  }
});

function setStatus(text, isError = false) {
  statusEl.textContent = text;
  statusEl.className = isError ? 'status error' : 'status';
}

function lastValid(arr) {
  for (let i = arr.length - 1; i >= 0; i--) {
    if (arr[i] !== null && !Number.isNaN(arr[i])) return Number(arr[i].toFixed(2));
  }
  return null;
}

// ---------------------------------------------------------------------------
// Data fetch: Financial Modeling Prep daily prices (last year), called
// directly from the browser since FMP sends CORS headers.
// ---------------------------------------------------------------------------
async function fetchOhlcv(ticker, fmpKey) {
  const to = new Date();
  const from = new Date();
  from.setFullYear(from.getFullYear() - 1);
  const fmt = (d) => d.toISOString().slice(0, 10);

  const url = `https://financialmodelingprep.com/stable/historical-price-eod/full?symbol=${ticker}&from=${fmt(from)}&to=${fmt(to)}&apikey=${fmpKey}`;
  const response = await fetch(url);

  const raw = await response.json();
  // FMP reports key/plan problems as { "Error Message": "..." }.
  if (raw && raw['Error Message']) throw new Error(raw['Error Message']);
  if (!response.ok) throw new Error('Price fetch failed. Check the ticker and try again.');

  // The stable endpoint returns a bare array; older paths nest it under `historical`.
  const bars = Array.isArray(raw) ? raw : (raw.historical ?? []);
  if (!bars.length) throw new Error('No price data returned for that ticker.');

  // FMP returns newest first; the indicators expect oldest to newest.
  return bars
    .map((b) => ({ date: new Date(b.date), close: Number(b.close) }))
    .filter((c) => !Number.isNaN(c.close))
    .sort((a, b) => a.date - b.date);
}

// ---------------------------------------------------------------------------
// Indicator math
// ---------------------------------------------------------------------------

// Simple moving average. First n minus 1 values are null.
function SMA(values, n) {
  const out = new Array(values.length).fill(null);
  for (let i = n - 1; i < values.length; i++) {
    let sum = 0;
    for (let j = i - n + 1; j <= i; j++) sum += values[j];
    out[i] = sum / n;
  }
  return out;
}

// Exponential moving average. Seeded with the SMA of the first n values.
function EMA(values, n) {
  const out = new Array(values.length).fill(null);
  const multiplier = 2 / (n + 1);
  const seedIndex = n - 1;
  if (seedIndex >= values.length) return out;

  let seedSum = 0;
  for (let i = 0; i <= seedIndex; i++) seedSum += values[i];
  out[seedIndex] = seedSum / n;

  for (let i = seedIndex + 1; i < values.length; i++) {
    out[i] = values[i] * multiplier + out[i - 1] * (1 - multiplier);
  }
  return out;
}

// MACD: fast EMA minus slow EMA, then an EMA of that difference as the signal line.
// Matches the EMA based convention taught this morning, not the SMA version.
function MACD(closes, nFast, nSlow, nSig) {
  const emaFast = EMA(closes, nFast);
  const emaSlow = EMA(closes, nSlow);

  const macdLine = closes.map((_, i) => {
    if (emaFast[i] === null || emaSlow[i] === null) return null;
    return emaFast[i] - emaSlow[i];
  });

  const firstValid = macdLine.findIndex((v) => v !== null);
  const macdValidPortion = macdLine.slice(firstValid).map((v) => v);
  const signalOnValidPortion = EMA(macdValidPortion, nSig);

  const signalLine = new Array(closes.length).fill(null);
  for (let i = 0; i < signalOnValidPortion.length; i++) {
    signalLine[firstValid + i] = signalOnValidPortion[i];
  }

  return { macdLine, signalLine };
}

// RSI: SMA based average gain and loss over n periods, matching the course
// convention from this morning's Technical Indicators in R session.
function RSI(closes, n) {
  const out = new Array(closes.length).fill(null);
  const gains = new Array(closes.length).fill(0);
  const losses = new Array(closes.length).fill(0);

  for (let i = 1; i < closes.length; i++) {
    const change = closes[i] - closes[i - 1];
    gains[i] = change > 0 ? change : 0;
    losses[i] = change < 0 ? Math.abs(change) : 0;
  }

  for (let i = n; i < closes.length; i++) {
    let avgGain = 0;
    let avgLoss = 0;
    for (let j = i - n + 1; j <= i; j++) {
      avgGain += gains[j];
      avgLoss += losses[j];
    }
    avgGain /= n;
    avgLoss /= n;

    if (avgLoss === 0) {
      out[i] = 100;
    } else {
      const rs = avgGain / avgLoss;
      out[i] = 100 - 100 / (1 + rs);
    }
  }
  return out;
}

// ---------------------------------------------------------------------------
// Chart rendering
// ---------------------------------------------------------------------------
function renderCharts(candles, fastMa, slowMa, macd, rsi) {
  const labels = candles.map((c) => c.date.toLocaleDateString());
  const closes = candles.map((c) => c.close);

  if (maChart) maChart.destroy();
  if (macdChart) macdChart.destroy();
  if (rsiChart) rsiChart.destroy();

  maChart = new Chart(document.getElementById('ma-chart'), {
    type: 'line',
    data: {
      labels,
      datasets: [
        { label: 'Close', data: closes, borderColor: '#14213D', borderWidth: 1.5, pointRadius: 0 },
        { label: 'Fast MA', data: fastMa, borderColor: '#9A6B2C', borderWidth: 1.5, pointRadius: 0 },
        { label: 'Slow MA', data: slowMa, borderColor: '#8FA3C9', borderWidth: 1.5, pointRadius: 0 },
      ],
    },
    options: chartOptions(),
  });

  macdChart = new Chart(document.getElementById('macd-chart'), {
    type: 'line',
    data: {
      labels,
      datasets: [
        { label: 'MACD', data: macd.macdLine, borderColor: '#14213D', borderWidth: 1.5, pointRadius: 0 },
        { label: 'Signal', data: macd.signalLine, borderColor: '#9A6B2C', borderWidth: 1.5, pointRadius: 0 },
      ],
    },
    options: chartOptions(),
  });

  rsiChart = new Chart(document.getElementById('rsi-chart'), {
    type: 'line',
    data: {
      labels,
      datasets: [
        { label: 'RSI', data: rsi, borderColor: '#14213D', borderWidth: 1.5, pointRadius: 0 },
        {
          label: 'Overbought (70)',
          data: labels.map(() => 70),
          borderColor: '#B85042',
          borderWidth: 1,
          pointRadius: 0,
          borderDash: [4, 4],
        },
        {
          label: 'Oversold (30)',
          data: labels.map(() => 30),
          borderColor: '#3A7D5C',
          borderWidth: 1,
          pointRadius: 0,
          borderDash: [4, 4],
        },
      ],
    },
    options: chartOptions({ min: 0, max: 100 }),
  });
}

function chartOptions(yRange) {
  return {
    responsive: true,
    animation: false,
    interaction: { mode: 'index', intersect: false },
    scales: {
      x: { ticks: { maxTicksLimit: 8 } },
      y: yRange ? { min: yRange.min, max: yRange.max } : {},
    },
    plugins: { legend: { position: 'bottom', labels: { boxWidth: 12 } } },
  };
}

// ---------------------------------------------------------------------------
// OpenRouter call: system prompt enforces structured JSON, same pattern from
// the Prompt Engineering and System Prompt Best Practices session.
// ---------------------------------------------------------------------------
async function getResearchNote(latest, apiKey) {
  const systemPrompt = `You are a financial signal explainer. Respond only with valid JSON matching this shape: {"explanation": string, "research_note": string, "risks": [string, string, string]}. No text outside the JSON. Keep the explanation plain English, the research note to one paragraph, and give exactly three risk factors.`;

  const userPrompt = `Ticker: ${latest.ticker}
Latest close: ${latest.close}
Fast moving average (${latest.fastWindow} day): ${latest.fastMa}
Slow moving average (${latest.slowWindow} day): ${latest.slowMa}
MACD: ${latest.macd}
MACD signal line: ${latest.macdSignal}
RSI (${latest.rsiPeriod} day): ${latest.rsi}

Explain what these signals suggest right now, write a one paragraph research note, and list three risk factors.`;

  const jsonSchema = {
    name: 'signal_explainer',
    strict: true,
    schema: {
      type: 'object',
      properties: {
        explanation: { type: 'string' },
        research_note: { type: 'string' },
        risks: { type: 'array', items: { type: 'string' }, minItems: 3, maxItems: 3 },
      },
      required: ['explanation', 'research_note', 'risks'],
      additionalProperties: false,
    },
  };

  // Two layers of defense, since JSON reliability varies by model and route:
  // 1. Ask OpenRouter to enforce the schema formally. Not every model or
  //    provider route supports this, so the request itself might fail.
  // 2. A prefill message that starts the assistant's reply with "{", which
  //    stops Claude from adding a preamble or a markdown fence before it,
  //    since the reply has technically already begun.
  const baseMessages = [
    { role: 'system', content: systemPrompt },
    { role: 'user', content: userPrompt },
  ];

  let raw = await callOpenRouter(apiKey, baseMessages, jsonSchema);
  let parsed = tryParseJson(raw);
  if (parsed) return parsed;

  console.warn('First attempt did not parse as JSON, retrying with a prefilled response:', raw);

  const prefillMessages = [...baseMessages, { role: 'assistant', content: '{' }];
  raw = await callOpenRouter(apiKey, prefillMessages, null);
  parsed = tryParseJson('{' + raw);
  if (parsed) return parsed;

  console.error('Model response still not valid JSON after retry:', raw);
  throw new Error('The model did not return valid JSON, even after a retry. Check the browser console for what it actually sent back.');
}

async function callOpenRouter(apiKey, messages, jsonSchema) {
  const body = {
    model: 'anthropic/claude-sonnet-4-6',
    messages,
    max_tokens: 600,
  };
  if (jsonSchema) {
    body.response_format = { type: 'json_schema', json_schema: jsonSchema };
  }

  const response = await fetch('https://openrouter.ai/api/v1/chat/completions', {
    method: 'POST',
    headers: {
      Authorization: `Bearer ${apiKey}`,
      'Content-Type': 'application/json',
    },
    body: JSON.stringify(body),
  });

  if (!response.ok) {
    if (jsonSchema) {
      // This model or route may not support response_format. Fall back to
      // asking again without it, rather than failing the whole request.
      console.warn('response_format was rejected, retrying without it.');
      return callOpenRouter(apiKey, messages, null);
    }
    throw new Error('OpenRouter call failed. Check your API key.');
  }

  const data = await response.json();
  return data.choices?.[0]?.message?.content ?? '';
}

// Strips markdown code fences and leading or trailing text around the JSON,
// then attempts to parse. Returns null instead of throwing, so callers can
// decide what to do next (such as retrying) rather than crashing.
function tryParseJson(text) {
  let cleaned = text.trim();
  cleaned = cleaned.replace(/^```json\s*/i, '').replace(/^```\s*/, '').replace(/```\s*$/, '');

  const firstBrace = cleaned.indexOf('{');
  const lastBrace = cleaned.lastIndexOf('}');
  if (firstBrace !== -1 && lastBrace !== -1 && lastBrace > firstBrace) {
    cleaned = cleaned.slice(firstBrace, lastBrace + 1);
  }

  try {
    return JSON.parse(cleaned);
  } catch (e) {
    return null;
  }
}

function renderResearchNote(result) {
  const risks = result.risks.map((r) => `<li>${r}</li>`).join('');
  researchNote.innerHTML = `
    <h2>What the signals suggest</h2>
    <p>${result.explanation}</p>
    <h2>Research note</h2>
    <p>${result.research_note}</p>
    <h2>Risk factors</h2>
    <ul>${risks}</ul>
  `;
}

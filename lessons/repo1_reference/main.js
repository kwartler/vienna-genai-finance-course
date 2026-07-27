import {
  Chart,
  LineController,
  LineElement,
  PointElement,
  LinearScale,
  CategoryScale,
  TimeScale,
  TimeSeriesScale,
  Tooltip,
  Legend,
  Filler,
} from 'chart.js';
import {
  CandlestickController,
  CandlestickElement,
  OhlcController,
  OhlcElement,
} from 'chartjs-chart-financial';
import 'chartjs-adapter-luxon';

// chart.js v4 is tree-shaken, so every controller, element, and scale we use
// has to be registered by hand. The financial plugin adds the candlestick
// pieces; the luxon adapter powers the time axis the candles sit on.
Chart.register(
  LineController,
  LineElement,
  PointElement,
  LinearScale,
  CategoryScale,
  TimeScale,
  TimeSeriesScale,
  Tooltip,
  Legend,
  Filler,
  CandlestickController,
  CandlestickElement,
  OhlcController,
  OhlcElement
);

// Twelve Data sends CORS headers, so the browser can fetch it directly with no
// proxy. Both API keys are entered in the form fields below and never stored in
// the code, so this repo stays safe to make public.

const form = document.getElementById('signal-form');
const statusEl = document.getElementById('status');
const researchNote = document.getElementById('research-note');

// Range selector elements.
const rangeControl = document.getElementById('range-control');
const rangeMin = document.getElementById('range-min');
const rangeMax = document.getElementById('range-max');
const rangeSelected = document.getElementById('range-selected');
const rangeFrom = document.getElementById('range-from');
const rangeTo = document.getElementById('range-to');
const rangeReset = document.getElementById('range-reset');

let priceChart, macdChart, rsiChart;
// The bars currently on the charts, so the range slider can map handle
// positions (bar indices) back to dates and timestamps.
let allCandles = [];

form.addEventListener('submit', async (event) => {
  event.preventDefault();

  const ticker = document.getElementById('ticker').value.trim().toUpperCase();
  const fastWindow = parseInt(document.getElementById('ma-fast').value, 10);
  const slowWindow = parseInt(document.getElementById('ma-slow').value, 10);
  const rsiPeriod = parseInt(document.getElementById('rsi-period').value, 10);
  const twelveDataKey = document.getElementById('twelvedata-key').value.trim();
  const openRouterKey = document.getElementById('openrouter-key').value.trim();

  if (fastWindow >= slowWindow) {
    setStatus('The MACD fast window must be smaller than the slow window.', true);
    return;
  }

  setStatus('Fetching price data...');
  clearResults();

  try {
    const candles = await fetchOhlc(ticker, twelveDataKey);
    const closes = candles.map((c) => c.close);

    setStatus('Calculating indicators...');
    const macd = MACD(closes, fastWindow, slowWindow, 9);
    const rsi = RSI(closes, rsiPeriod);

    renderCharts(ticker, candles, macd, rsi);

    setStatus('Asking the model to explain the current signals...');
    const latest = {
      ticker,
      bar: candles[candles.length - 1],
      periodStart: candles[0],
      periodHigh: Math.max(...candles.map((c) => c.high)),
      periodLow: Math.min(...candles.map((c) => c.low)),
      macd: lastValid(macd.macdLine),
      macdSignal: lastValid(macd.signalLine),
      rsi: lastValid(rsi),
      fastWindow,
      slowWindow,
      rsiPeriod,
      bars: candles.length,
    };
    const aiResult = await getResearchNote(latest, openRouterKey);
    renderResearchNote(aiResult);

    setStatus('Done.');
  } catch (err) {
    setStatus(`Something went wrong: ${err.message}`, true);
    researchNote.innerHTML = `<p class="error">${err.message}</p>`;
  }
});

// Both handles and the reset button feed the same handler. Set up once; the
// inputs live in the DOM from the start and act on whatever charts exist.
rangeMin.addEventListener('input', onRangeInput);
rangeMax.addEventListener('input', onRangeInput);
rangeReset.addEventListener('click', () => {
  const last = allCandles.length - 1;
  rangeMin.value = 0;
  rangeMax.value = last;
  onRangeInput();
});

function setStatus(text, isError = false) {
  statusEl.textContent = text;
  statusEl.className = isError ? 'status error' : 'status';
}

// Wipe the previous run before a new one starts: tear down the three charts,
// hide their titles and the zoom slider again, and reset the note. Without this
// a slow or failed second fetch would leave the old stock's charts on screen,
// which looks frozen.
function clearResults() {
  for (const id of ['price-chart', 'macd-chart', 'rsi-chart']) {
    const existing = Chart.getChart(id);
    if (existing) existing.destroy();
  }
  priceChart = macdChart = rsiChart = null;
  allCandles = [];

  for (const title of document.querySelectorAll('.chart-block h2')) title.hidden = true;
  rangeControl.hidden = true;
  researchNote.innerHTML = '<p class="placeholder">Working on it...</p>';
}

// ---------------------------------------------------------------------------
// Range selector: a dual-handle slider over bar indices that sets the x-axis
// window on all three charts at once, so zooming the price chart zooms the
// MACD and RSI plots in lockstep.
// ---------------------------------------------------------------------------
function setupRangeControl() {
  const last = allCandles.length - 1;
  for (const input of [rangeMin, rangeMax]) {
    input.min = '0';
    input.max = String(last);
  }
  rangeMin.value = '0';
  rangeMax.value = String(last);
  updateRangeUI(0, last);
  rangeControl.hidden = false;
}

function onRangeInput(event) {
  const last = allCandles.length - 1;
  if (last < 1) return;

  // Keep at least a few bars in view so the indicators stay legible, and stop
  // the two handles from crossing by nudging whichever one just moved.
  const minSpan = Math.min(5, last);
  let lo = Number(rangeMin.value);
  let hi = Number(rangeMax.value);
  if (hi - lo < minSpan) {
    if (event && event.target === rangeMax) {
      hi = Math.min(last, lo + minSpan);
      rangeMax.value = String(hi);
    } else {
      lo = Math.max(0, hi - minSpan);
      rangeMin.value = String(lo);
    }
  }

  updateRangeUI(lo, hi);
  applyRange(lo, hi);
}

function updateRangeUI(lo, hi) {
  const last = allCandles.length - 1;
  rangeSelected.style.left = `${(lo / last) * 100}%`;
  rangeSelected.style.width = `${((hi - lo) / last) * 100}%`;
  rangeFrom.textContent = allCandles[lo].date;
  rangeTo.textContent = allCandles[hi].date;
}

// The candlestick sits on a time axis, so its window is set with timestamps.
// The MACD and RSI line charts sit on a category axis, so theirs is set with
// the bar index. Both point at the same bars, so the windows line up.
function applyRange(lo, hi) {
  if (priceChart) {
    priceChart.options.scales.x.min = allCandles[lo].time;
    priceChart.options.scales.x.max = allCandles[hi].time;
    priceChart.update('none');
  }
  for (const chart of [macdChart, rsiChart]) {
    if (!chart) continue;
    chart.options.scales.x.min = lo;
    chart.options.scales.x.max = hi;
    chart.update('none');
  }
}

function lastValid(arr) {
  for (let i = arr.length - 1; i >= 0; i--) {
    if (arr[i] !== null && !Number.isNaN(arr[i])) return Number(arr[i].toFixed(2));
  }
  return null;
}

// ---------------------------------------------------------------------------
// Data fetch: Twelve Data daily OHLC, called directly from the browser since
// Twelve Data sends CORS headers. Unlike the close-only build, we keep the
// full open/high/low/close so the candlestick chart has something to draw.
// ---------------------------------------------------------------------------
async function fetchOhlc(ticker, apiKey) {
  // outputsize is the number of most-recent daily bars. 120 is roughly six
  // months, enough for the slow EMA to warm up and still read clearly.
  const url = `https://api.twelvedata.com/time_series?symbol=${ticker}&interval=1day&outputsize=120&apikey=${apiKey}`;
  const response = await fetch(url);

  // Read the body as text first, then parse it, so an unexpected non-JSON
  // response gives a readable error instead of "Unexpected token".
  const body = await response.text();
  let raw;
  try {
    raw = JSON.parse(body);
  } catch {
    throw new Error(body.trim() || 'Price fetch failed');
  }

  // Twelve Data reports problems as { code, status: "error", message }.
  if (raw && raw.status === 'error') throw new Error(raw.message || 'Price fetch failed');
  if (!response.ok) throw new Error('Price fetch failed. Check the ticker and try again.');

  const values = raw.values ?? [];
  if (!values.length) throw new Error(`No price data returned for ${ticker}.`);

  // Twelve Data returns newest first; indicators and charts expect oldest to
  // newest. Normalize to numbers and sort ascending by date.
  return values
    .map((b) => ({
      date: b.datetime,
      time: new Date(b.datetime).valueOf(),
      open: Number(b.open),
      high: Number(b.high),
      low: Number(b.low),
      close: Number(b.close),
      volume: Number(b.volume),
    }))
    .filter((c) => !Number.isNaN(c.close))
    .sort((a, b) => a.time - b.time);
}

// ---------------------------------------------------------------------------
// Indicator math
// ---------------------------------------------------------------------------

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

// MACD: fast EMA minus slow EMA, then an EMA of that difference as the signal
// line. Matches the EMA based convention taught this morning.
function MACD(closes, nFast, nSlow, nSig) {
  const emaFast = EMA(closes, nFast);
  const emaSlow = EMA(closes, nSlow);

  const macdLine = closes.map((_, i) => {
    if (emaFast[i] === null || emaSlow[i] === null) return null;
    return emaFast[i] - emaSlow[i];
  });

  const firstValid = macdLine.findIndex((v) => v !== null);
  const macdValidPortion = macdLine.slice(firstValid);
  const signalOnValidPortion = EMA(macdValidPortion, nSig);

  const signalLine = new Array(closes.length).fill(null);
  for (let i = 0; i < signalOnValidPortion.length; i++) {
    signalLine[firstValid + i] = signalOnValidPortion[i];
  }

  return { macdLine, signalLine };
}

// RSI: SMA based average gain and loss over n periods, matching the course
// convention from this morning's Technical Indicators session.
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
// Chart rendering: three separate charts, one candlestick and two line plots.
// ---------------------------------------------------------------------------
function renderCharts(ticker, candles, macd, rsi) {
  const labels = candles.map((c) => c.date);
  allCandles = candles;

  // Candlestick built straight from the OHLC bars. Each point is
  // { x: timestamp, o, h, l, c }, which is the shape the financial plugin wants.
  priceChart = new Chart(freshCanvas('price-chart'), {
    type: 'candlestick',
    data: {
      datasets: [
        {
          label: `${ticker} daily`,
          data: candles.map((c) => ({
            x: c.time,
            o: c.open,
            h: c.high,
            l: c.low,
            c: c.close,
          })),
          color: {
            up: '#3A7D5C',
            down: '#B85042',
            unchanged: '#8A8577',
          },
        },
      ],
    },
    options: {
      responsive: true,
      animation: false,
      parsing: false,
      scales: {
        x: { type: 'timeseries', ticks: { maxTicksLimit: 8, source: 'data' } },
        y: {},
      },
      plugins: { legend: { position: 'bottom', labels: { boxWidth: 12 } } },
    },
  });
  showChartTitle('price-chart');

  macdChart = new Chart(freshCanvas('macd-chart'), {
    type: 'line',
    data: {
      labels,
      datasets: [
        { label: 'MACD', data: macd.macdLine, borderColor: '#14213D', borderWidth: 1.5, pointRadius: 0 },
        { label: 'Signal', data: macd.signalLine, borderColor: '#9A6B2C', borderWidth: 1.5, pointRadius: 0 },
      ],
    },
    options: lineOptions(),
  });
  showChartTitle('macd-chart');

  rsiChart = new Chart(freshCanvas('rsi-chart'), {
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
    options: lineOptions({ min: 0, max: 100 }),
  });
  showChartTitle('rsi-chart');

  setupRangeControl();
}

// Destroys any chart already attached to this canvas (by id), whichever code
// path created it, then hands back the element. Using Chart.getChart instead of
// our own variables means a half-built chart from a failed run cannot leave the
// canvas "already in use" on the next attempt.
function freshCanvas(id) {
  const existing = Chart.getChart(id);
  if (existing) existing.destroy();
  return document.getElementById(id);
}

// Reveals a chart block's heading once its chart has actually been drawn, so
// the page does not show "Price (candlestick)" and friends before there is
// anything under them.
function showChartTitle(canvasId) {
  const title = document.getElementById(canvasId).closest('.chart-block').querySelector('h2');
  if (title) title.hidden = false;
}

function lineOptions(yRange) {
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
// OpenRouter call: the OHLC summary plus the latest MACD and RSI are handed to
// the model. The system prompt enforces structured JSON, the same pattern from
// the Prompt Engineering and System Prompt Best Practices session.
// ---------------------------------------------------------------------------
async function getResearchNote(latest, apiKey) {
  const systemPrompt = `You are a financial signal explainer. Respond only with valid JSON matching this shape: {"explanation": string, "research_note": string, "risks": [string, string, string]}. No text outside the JSON. Keep the explanation plain English, the research note to one paragraph, and give exactly three risk factors.`;

  const bar = latest.bar;
  const start = latest.periodStart;
  const pctChange = ((bar.close - start.close) / start.close) * 100;

  const userPrompt = `Ticker: ${latest.ticker}
Window: ${latest.bars} trading days, ${start.date} to ${bar.date}
Latest OHLC bar (${bar.date}): open ${bar.open}, high ${bar.high}, low ${bar.low}, close ${bar.close}
Period high: ${latest.periodHigh}
Period low: ${latest.periodLow}
Change over window: ${pctChange.toFixed(1)}%
MACD (${latest.fastWindow}/${latest.slowWindow}/9): ${latest.macd}
MACD signal line: ${latest.macdSignal}
RSI (${latest.rsiPeriod} day): ${latest.rsi}

Explain what these OHLC, MACD, and RSI signals suggest right now, write a one paragraph research note, and list three risk factors.`;

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
  //    stops the model from adding a preamble or markdown fence before it.
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
    model: 'anthropic/claude-sonnet-5',
    messages,
    max_tokens: 600,
  };
  if (jsonSchema) {
    body.response_format = { type: 'json_schema', json_schema: jsonSchema };
    // Only route to a provider endpoint that actually honors the schema, rather
    // than silently falling back to one that ignores it. Left off the no-schema
    // retry below on purpose, so that fallback can still route anywhere.
    body.provider = { require_parameters: true };
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

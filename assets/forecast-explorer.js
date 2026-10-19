/* Forecast explorer: one game's predictive distribution of the margin against the result.
   Draws the forecast CDF F(t) against the observed step 1{y <= t}, and below it the squared
   gap (F(t) - 1{y <= t})^2 at every threshold t, whose area is the CRPS and whose height at
   t = 0 is the Brier score of the win probability.

   Forecast.fromQuantiles(q, probs) / Forecast.normal(mean, sd)  -> { cdf(t), median }
   ForecastExplorer.mount(element, {
     presets: [{ id, label, sub, side, q, y }],   // q: quantiles at Forecast.PROBS
     custom:  { label, sub, side, centre, sd, y }, // a Normal forecast the reader controls
     refs:    { brier, crps, mae },                // backtest averages shown beside readouts
     challenges: [{ text, test(state), takeaway }] // auto-checked, Build-your-own mode only
   }) */
(function () {
  const PROBS = Array.from({ length: 199 }, (_, i) => Math.round((i + 1) * 5) / 1000);

  // Abramowitz & Stegun 7.1.26, |error| < 1.5e-7
  function erf(x) {
    const s = Math.sign(x); x = Math.abs(x);
    const t = 1 / (1 + 0.3275911 * x);
    const y = 1 - (((((1.061405429 * t - 1.453152027) * t) + 1.421413741) * t - 0.284496736) * t + 0.254829592) * t * Math.exp(-x * x);
    return s * y;
  }

  const Forecast = {
    PROBS,
    // Linear interpolation between stored quantiles, tails clamped: the same rule as
    // cdf_from_quantiles() in R/03_rankings.R.
    fromQuantiles(q, probs = PROBS) {
      const n = q.length;
      return {
        median: q[Math.round((n - 1) / 2)],
        cdf(t) {
          if (t <= q[0]) return probs[0] / 2;
          if (t >= q[n - 1]) return 1 - (1 - probs[n - 1]) / 2;
          let lo = 0, hi = n - 1;
          while (hi - lo > 1) { const mid = (lo + hi) >> 1; if (q[mid] <= t) lo = mid; else hi = mid; }
          const span = q[hi] - q[lo];
          return span > 0 ? probs[lo] + (probs[hi] - probs[lo]) * (t - q[lo]) / span : probs[hi];
        }
      };
    },
    normal(mean, sd) {
      return {
        median: mean,
        cdf: sd <= 0 ? (t => (t >= mean ? 1 : 0)) : (t => 0.5 * (1 + erf((t - mean) / (sd * Math.SQRT2))))
      };
    },
    // CRPS by its definition: the integral over thresholds of the squared gap.
    crps(f, y, lo = -250, hi = 250, dt = 0.05) {
      let s = 0;
      for (let t = lo + dt / 2; t < hi; t += dt) { const g = f.cdf(t) - (y <= t ? 1 : 0); s += g * g; }
      return s * dt;
    }
  };

  const STYLE = `
  .fx { font-family: var(--sans); margin: 1.6rem 0 2rem; }
  .fx [hidden] { display: none !important; }
  .fx .presets { display: flex; flex-wrap: wrap; gap: 0.5rem; margin-bottom: 1rem; }
  .fx .presets button {
    font: inherit; font-size: 0.8rem; text-align: left; color: var(--ink); cursor: pointer;
    background: var(--bg-raised); border: 1px solid var(--rule); border-radius: 6px; padding: 0.45rem 0.75rem; line-height: 1.3;
  }
  .fx .presets button small { display: block; color: var(--muted); font-size: 0.72rem; }
  .fx .presets button[aria-pressed="true"] { border-color: var(--c-forecast); box-shadow: inset 0 -2px 0 var(--c-forecast); }
  .fx .readouts { display: grid; grid-template-columns: repeat(4, minmax(0, 1fr)); gap: 0.5rem; margin-bottom: 0.6rem; }
  @media (max-width: 640px) { .fx .readouts { grid-template-columns: repeat(2, minmax(0, 1fr)); } }
  .fx .ro { border-top: 2px solid var(--rule); padding-top: 0.35rem; }
  .fx .ro .k { font-size: 0.7rem; color: var(--muted); letter-spacing: 0.03em; }
  .fx .ro .v { font-size: 1.35rem; font-variant-numeric: tabular-nums; line-height: 1.2; }
  .fx .ro .r { font-size: 0.68rem; color: var(--muted); }
  .fx .ro.slice { border-top-color: var(--c-slice); }
  .fx .ro.area { border-top-color: var(--c-gap); }
  .fx svg { width: 100%; height: auto; display: block; touch-action: none; }
  .fx svg text { font-family: var(--sans); fill: var(--muted); font-size: 11px; }
  .fx svg .ttl { fill: var(--ink); font-size: 11.5px; font-weight: 600; }
  .fx svg .grid { stroke: var(--rule); stroke-width: 1; }
  .fx svg .axis { stroke: var(--muted); stroke-width: 1; }
  .fx svg .forecast { fill: none; stroke: var(--c-forecast); stroke-width: 2.5; }
  .fx svg .observed { fill: none; stroke: var(--c-observed); stroke-width: 2; }
  .fx svg .gap { fill: var(--c-gap-soft); stroke: none; }
  .fx svg .integrand { fill: var(--c-gap); fill-opacity: 0.45; stroke: var(--c-gap); stroke-width: 1.5; }
  .fx svg .slice { stroke: var(--c-slice); stroke-width: 1.5; stroke-dasharray: 4 3; }
  .fx svg .slicedot { fill: var(--c-slice); }
  .fx svg .slicelbl { fill: var(--c-slice); font-weight: 600; }
  .fx svg .hover { stroke: var(--muted); stroke-width: 1; }
  .fx svg .hdot { stroke: var(--bg); stroke-width: 1.5; }
  .fx .hoverline { font-size: 0.78rem; color: var(--muted); min-height: 2.6em; margin: 0.3rem 0 0.8rem; font-variant-numeric: tabular-nums; }
  .fx .controls { display: grid; gap: 0.6rem; margin: 0.6rem 0 1rem; }
  .fx .controls label { display: grid; grid-template-columns: 11.5rem 1fr 4.2rem; align-items: center; gap: 0.8rem; font-size: 0.82rem; }
  @media (max-width: 640px) { .fx .controls label { grid-template-columns: 1fr 4rem; } .fx .controls label input { grid-column: 1 / -1; grid-row: 2; } }
  .fx .controls output { text-align: right; font-variant-numeric: tabular-nums; }
  .fx input[type=range] { width: 100%; accent-color: var(--c-forecast); }
  .fx .challenges { border-top: 1px solid var(--rule); padding-top: 0.8rem; }
  .fx .challenges h4 { font-size: 0.78rem; letter-spacing: 0.06em; text-transform: uppercase; color: var(--muted); margin: 0 0 0.5rem; font-weight: 600; }
  .fx .challenges ol { margin: 0; padding-left: 0; list-style: none; display: grid; gap: 0.55rem; }
  .fx .challenges li { font-size: 0.85rem; line-height: 1.45; display: grid; grid-template-columns: 1.6rem 1fr; }
  .fx .challenges .box { font-weight: 600; color: var(--muted); }
  .fx .challenges li.done .box { color: var(--c-good); }
  .fx .challenges .take { display: block; color: var(--c-good); margin-top: 0.2rem; }
  .fx .challenges .note { font-size: 0.75rem; color: var(--muted); margin: 0 0 0.6rem; }
  `;

  const NS = "http://www.w3.org/2000/svg";
  const W = 720, H = 400, L = 46, R = 14;
  const T0 = 16, T1 = 190, B0 = 226, B1 = 340;
  const TMIN = -70, TMAX = 70;
  const sx = t => L + (Math.max(TMIN, Math.min(TMAX, t)) - TMIN) / (TMAX - TMIN) * (W - L - R);
  const sTop = p => T1 - p * (T1 - T0);
  const sBot = v => B1 - v * (B1 - B0);

  function el(name, attrs, parent) {
    const e = document.createElementNS(NS, name);
    for (const k in attrs) e.setAttribute(k, attrs[k]);
    if (parent) parent.appendChild(e);
    return e;
  }
  const fmtSigned = v => (v > 0 ? "+" : v < 0 ? "−" : "") + Math.abs(v).toFixed(Number.isInteger(v) ? 0 : 1);

  function mount(root, cfg) {
    if (!document.getElementById("fx-style")) {
      const s = document.createElement("style"); s.id = "fx-style"; s.textContent = STYLE; document.head.appendChild(s);
    }
    root.classList.add("fx");
    const refs = cfg.refs || {};
    const state = { mode: cfg.presets.length ? cfg.presets[0].id : "custom", y: 0, centre: cfg.custom.centre, sd: cfg.custom.sd };
    const presetById = Object.fromEntries(cfg.presets.map(p => [p.id, { ...p, f: Forecast.fromQuantiles(p.q) }]));
    const done = new Set();

    root.innerHTML = `
      <div class="presets" role="group" aria-label="Choose a forecast"></div>
      <div class="readouts" aria-live="polite">
        <div class="ro"><div class="k">Win probability, first-named side</div><div class="v" data-k="p"></div><div class="r">= 1 − F(0)</div></div>
        <div class="ro"><div class="k">Point miss |median − y|</div><div class="v" data-k="miss"></div><div class="r">${refs.mae ? `backtest MAE ${refs.mae.toFixed(1)}` : ""}</div></div>
        <div class="ro slice"><div class="k">Brier score (the t = 0 slice)</div><div class="v" data-k="brier"></div><div class="r">${refs.brier ? `backtest mean ${refs.brier.toFixed(3)}` : ""}</div></div>
        <div class="ro area"><div class="k">CRPS (the whole area), points</div><div class="v" data-k="crps"></div><div class="r">${refs.crps ? `backtest mean ${refs.crps.toFixed(1)}` : ""}</div></div>
      </div>
      <div class="plot"></div>
      <div class="hoverline">Hover over (or drag across) the chart: every vertical slice is its own yes/no question with its own Brier score.</div>
      <div class="controls no-print">
        <label><span>Final margin <var>y</var></span><input type="range" data-k="y" min="-70" max="70" step="1"><output data-k="y"></output></label>
        <label data-custom><span>Forecast centre</span><input type="range" data-k="centre" min="-40" max="40" step="0.5"><output data-k="centre"></output></label>
        <label data-custom><span>Forecast spread (sd)</span><input type="range" data-k="sd" min="0" max="30" step="0.5"><output data-k="sd"></output></label>
      </div>
      <div class="challenges no-print" ${cfg.challenges && cfg.challenges.length ? "" : "hidden"}>
        <h4>Challenges</h4>
        <p class="note">Use <em>${cfg.custom.label}</em>. Each one ticks itself off when you get there.</p>
        <ol></ol>
      </div>`;

    const q = s => root.querySelector(s);
    const presetsEl = q(".presets");
    const options = [...cfg.presets.map(p => ({ id: p.id, label: p.label, sub: p.sub })), { id: "custom", label: cfg.custom.label, sub: cfg.custom.sub }];
    const pbtns = options.map(o => {
      const b = document.createElement("button");
      b.type = "button";
      b.innerHTML = `${o.label}<small>${o.sub || ""}</small>`;
      b.addEventListener("click", () => select(o.id));
      presetsEl.appendChild(b);
      return [o.id, b];
    });

    // SVG scaffold
    const svg = el("svg", { viewBox: `0 0 ${W} ${H}`, role: "img", "aria-label": "Forecast CDF against the observed result, and the squared gap at every threshold" }, q(".plot"));
    const gGrid = el("g", {}, svg);
    for (let t = -60; t <= 60; t += 20) {
      el("line", { x1: sx(t), x2: sx(t), y1: T0, y2: T1, class: "grid" }, gGrid);
      el("line", { x1: sx(t), x2: sx(t), y1: B0, y2: B1, class: "grid" }, gGrid);
      const tx = el("text", { x: sx(t), y: B1 + 16, "text-anchor": "middle" }, gGrid); tx.textContent = t === 0 ? "0" : fmtSigned(t);
    }
    [[0, "0"], [0.5, "0.5"], [1, "1"]].forEach(([v, s]) => {
      el("line", { x1: L, x2: W - R, y1: sTop(v), y2: sTop(v), class: "grid" }, gGrid);
      el("line", { x1: L, x2: W - R, y1: sBot(v), y2: sBot(v), class: "grid" }, gGrid);
      el("text", { x: L - 6, y: sTop(v) + 4, "text-anchor": "end" }, gGrid).textContent = s;
      el("text", { x: L - 6, y: sBot(v) + 4, "text-anchor": "end" }, gGrid).textContent = s;
    });
    el("line", { x1: L, x2: W - R, y1: T1, y2: T1, class: "axis" }, gGrid);
    el("line", { x1: L, x2: W - R, y1: B1, y2: B1, class: "axis" }, gGrid);
    const ttl1 = el("text", { x: L, y: T0 - 4, class: "ttl" }, svg); ttl1.textContent = "① Forecast P(margin ≤ t) against what happened";
    const ttl2 = el("text", { x: L, y: B0 - 6, class: "ttl" }, svg); ttl2.textContent = "② Squared gap at each threshold t: a Brier score per slice. Area = CRPS";
    const axisT = el("text", { x: (L + W - R) / 2, y: H - 8, "text-anchor": "middle" }, svg);

    const gap = el("path", { class: "gap" }, svg);
    const obs = el("path", { class: "observed" }, svg);
    const fc = el("path", { class: "forecast" }, svg);
    const integ = el("path", { class: "integrand" }, svg);
    const slice = el("line", { x1: sx(0), x2: sx(0), y1: T0, y2: B1, class: "slice" }, svg);
    const sliceDot = el("circle", { r: 4.5, class: "slicedot", cx: sx(0) }, svg);
    const sliceLbl = el("text", { class: "slicelbl" }, svg);
    const fcLbl = el("text", { style: "fill: var(--c-forecast); font-weight: 600" }, svg);
    const obsLbl = el("text", { style: "fill: var(--c-observed); font-weight: 600" }, svg);
    const areaLbl = el("text", { style: "fill: var(--c-gap); font-weight: 600" }, svg);
    const hov = el("g", { visibility: "hidden" }, svg);
    const hLine = el("line", { y1: T0, y2: B1, class: "hover" }, hov);
    const hF = el("circle", { r: 4, class: "hdot", style: "fill: var(--c-forecast)" }, hov);
    const hH = el("circle", { r: 4, class: "hdot", style: "fill: var(--c-observed)" }, hov);
    const hG = el("circle", { r: 4, class: "hdot", style: "fill: var(--c-gap)" }, hov);
    const hit = el("rect", { x: L, y: T0, width: W - L - R, height: B1 - T0, fill: "transparent" }, svg);

    // Controls
    const inputs = {};
    root.querySelectorAll("input[type=range]").forEach(inp => {
      inputs[inp.dataset.k] = inp;
      inp.addEventListener("input", () => { state[inp.dataset.k] = +inp.value; update(); });
    });

    // Challenges
    const chList = q(".challenges ol");
    const chItems = (cfg.challenges || []).map((c, i) => {
      const li = document.createElement("li");
      li.innerHTML = `<span class="box">☐</span><span>${c.text}<span class="take" hidden>${c.takeaway}</span></span>`;
      chList.appendChild(li);
      return li;
    });

    function current() {
      if (state.mode === "custom") return { f: Forecast.normal(state.centre, state.sd), side: cfg.custom.side };
      const p = presetById[state.mode];
      return { f: p.f, side: p.side };
    }

    function select(id) {
      state.mode = id;
      if (id === "custom") state.y = cfg.custom.y; else state.y = presetById[id].y;
      if (id === "custom") { state.centre = cfg.custom.centre; state.sd = cfg.custom.sd; }
      pbtns.forEach(([pid, b]) => b.setAttribute("aria-pressed", String(pid === id)));
      root.querySelectorAll("[data-custom]").forEach(n => (n.hidden = id !== "custom"));
      update();
    }

    let last = null;
    function update() {
      const { f, side } = current();
      const y = state.y;
      for (const k in inputs) { inputs[k].value = state[k]; }
      q('output[data-k="y"]').textContent = fmtSigned(y);
      q('output[data-k="centre"]').textContent = fmtSigned(state.centre);
      q('output[data-k="sd"]').textContent = state.sd.toFixed(1);
      axisT.textContent = `threshold t on the margin (${side}), points`;

      // Paths
      const ts = []; for (let t = TMIN; t <= TMAX + 1e-9; t += 0.25) ts.push(t);
      const F = ts.map(t => f.cdf(t));
      const Hs = ts.map(t => (y <= t ? 1 : 0));
      fc.setAttribute("d", "M" + ts.map((t, i) => `${sx(t).toFixed(1)},${sTop(F[i]).toFixed(1)}`).join("L"));
      const yc = Math.max(TMIN, Math.min(TMAX, y));
      const stepD = `M${sx(TMIN)},${sTop(0)}L${sx(yc)},${sTop(0)}L${sx(yc)},${sTop(1)}L${sx(TMAX)},${sTop(1)}`;
      obs.setAttribute("d", stepD);
      gap.setAttribute("d", "M" + ts.map((t, i) => `${sx(t).toFixed(1)},${sTop(F[i]).toFixed(1)}`).join("L") +
        `L${sx(TMAX)},${sTop(1)}L${sx(yc)},${sTop(1)}L${sx(yc)},${sTop(0)}L${sx(TMIN)},${sTop(0)}Z`);
      const G = ts.map((t, i) => (F[i] - Hs[i]) ** 2);
      integ.setAttribute("d", `M${sx(TMIN)},${sBot(0)}` + ts.map((t, i) => `L${sx(t).toFixed(1)},${sBot(G[i]).toFixed(1)}`).join("") + `L${sx(TMAX)},${sBot(0)}Z`);

      // Numbers
      const F0 = f.cdf(0);
      const brier = (F0 - (y <= 0 ? 1 : 0)) ** 2;
      const crps = Forecast.crps(f, y);
      const miss = Math.abs(f.median - y);
      const p = 1 - F0;
      q('[data-k="p"].v').textContent = `${Math.round(p * 100)}%`;
      q('[data-k="miss"].v').textContent = miss.toFixed(1);
      q('[data-k="brier"].v').textContent = brier.toFixed(3);
      q('[data-k="crps"].v').textContent = crps.toFixed(1);

      sliceDot.setAttribute("cy", sBot(brier));
      sliceLbl.setAttribute("x", sx(0) + 7);
      sliceLbl.setAttribute("y", Math.max(B0 + 10, sBot(brier) - 6));
      sliceLbl.textContent = `t = 0 → Brier ${brier.toFixed(3)}`;

      // Curve labels placed where they don't collide with the step
      const tLab = y > 0 ? -62 : 22;
      fcLbl.setAttribute("x", sx(tLab)); fcLbl.setAttribute("y", Math.min(T1 - 6, Math.max(T0 + 12, sTop(f.cdf(tLab)) - 8)));
      fcLbl.setAttribute("text-anchor", "start");
      fcLbl.textContent = "forecast F(t)";
      // Result label on the side of the step away from the t = 0 line
      const lblLeft = yc > 40 || (yc < 0 && yc > -50);
      obsLbl.setAttribute("x", sx(yc) + (lblLeft ? -6 : 6)); obsLbl.setAttribute("y", sTop(0.5));
      obsLbl.setAttribute("text-anchor", lblLeft ? "end" : "start");
      obsLbl.textContent = `result y = ${fmtSigned(y)}`;
      areaLbl.setAttribute("x", W - R - 4); areaLbl.setAttribute("y", B0 + 14);
      areaLbl.setAttribute("text-anchor", "end");
      areaLbl.textContent = `orange area = CRPS = ${crps.toFixed(1)} pts`;

      last = { f, y, side };

      // Challenges
      const s = { mode: state.mode, y, centre: state.centre, sd: state.sd, brier, crps, miss, p };
      (cfg.challenges || []).forEach((c, i) => {
        if (!done.has(i) && state.mode === "custom" && c.test(s)) {
          done.add(i);
          chItems[i].classList.add("done");
          chItems[i].querySelector(".box").textContent = "✓";
          chItems[i].querySelector(".take").hidden = false;
        }
      });
    }

    function hover(evt) {
      if (!last) return;
      const r = svg.getBoundingClientRect();
      const xv = (evt.clientX - r.left) / r.width * W;
      const t = Math.round(TMIN + (xv - L) / (W - L - R) * (TMAX - TMIN));
      if (t < TMIN || t > TMAX) return;
      const Ft = last.f.cdf(t), Ht = last.y <= t ? 1 : 0, g = (Ft - Ht) ** 2;
      hov.setAttribute("visibility", "visible");
      hLine.setAttribute("x1", sx(t)); hLine.setAttribute("x2", sx(t));
      hF.setAttribute("cx", sx(t)); hF.setAttribute("cy", sTop(Ft));
      hH.setAttribute("cx", sx(t)); hH.setAttribute("cy", sTop(Ht));
      hG.setAttribute("cx", sx(t)); hG.setAttribute("cy", sBot(g));
      q(".hoverline").innerHTML =
        `<strong>t = ${fmtSigned(t)}</strong>. Question: “will the margin be ≤ ${fmtSigned(t)}?” ` +
        `Forecast said <strong style="color:var(--c-forecast)">${(Ft * 100).toFixed(0)}%</strong>; ` +
        `it <strong>${Ht ? "happened (1)" : "didn’t happen (0)"}</strong>. ` +
        `Brier score for this slice: (${Ft.toFixed(2)} − ${Ht})² = <strong style="color:var(--c-gap)">${g.toFixed(3)}</strong>.`;
    }
    hit.addEventListener("pointermove", hover);
    hit.addEventListener("pointerdown", hover);
    hit.addEventListener("pointerleave", () => hov.setAttribute("visibility", "hidden"));

    select(state.mode);
  }

  window.Forecast = Forecast;
  window.ForecastExplorer = { mount };
})();

/* Minimal SVG chart primitives shared by the course widgets.
   Charts.frame(container, {w, h, m, label}) -> { svg, x0, x1, yb (bottom), yt (top) }
   Charts.scale(d0, d1, r0, r1) -> f(v) with f.invert(p)
   Charts.xAxis / yAxis(frame, scale, ticks, fmt, title)
   Colours come from course.css tokens through the classes below. */
(function () {
  const NS = "http://www.w3.org/2000/svg";
  const STYLE = `
  svg.chart { width: 100%; height: auto; display: block; overflow: visible; touch-action: none; }
  svg.chart text { font-family: var(--sans); fill: var(--muted); font-size: 11px; }
  svg.chart .ttl { fill: var(--ink); font-weight: 600; font-size: 11.5px; }
  svg.chart .ink { fill: var(--ink); }
  svg.chart .grid { stroke: var(--rule); stroke-width: 1; }
  svg.chart .axis { stroke: var(--muted); stroke-width: 1; }
  svg.chart .zero { stroke: var(--ink); stroke-width: 1; stroke-dasharray: 3 3; }
  svg.chart .bar { fill: var(--muted); fill-opacity: 0.35; }
  svg.chart .bar.on { fill: var(--c-forecast); fill-opacity: 0.55; }
  svg.chart .ln-forecast { fill: none; stroke: var(--c-forecast); stroke-width: 2.25; }
  svg.chart .ln-gap { fill: none; stroke: var(--c-gap); stroke-width: 2.25; }
  svg.chart .ln-slice { fill: none; stroke: var(--c-slice); stroke-width: 2.25; }
  svg.chart .ln-ink { fill: none; stroke: var(--ink); stroke-width: 1.5; }
  svg.chart .ln-muted { fill: none; stroke: var(--muted); stroke-width: 1.25; stroke-dasharray: 4 3; }
  svg.chart .ar-forecast { fill: var(--c-forecast); fill-opacity: 0.18; stroke: none; }
  svg.chart .ar-gap { fill: var(--c-gap); fill-opacity: 0.35; stroke: none; }
  svg.chart .ar-band { fill: var(--muted); fill-opacity: 0.12; stroke: none; }
  svg.chart .c-forecast { fill: var(--c-forecast); }
  svg.chart .c-gap { fill: var(--c-gap); }
  svg.chart .c-slice { fill: var(--c-slice); }
  svg.chart .c-good { fill: var(--c-good); }
  svg.chart .c-bad { fill: var(--c-bad); }
  svg.chart .s-forecast { stroke: var(--c-forecast); }
  svg.chart .s-gap { stroke: var(--c-gap); }
  svg.chart .s-slice { stroke: var(--c-slice); }
  svg.chart .s-good { stroke: var(--c-good); }
  svg.chart .s-bad { stroke: var(--c-bad); }
  svg.chart .s-ink { stroke: var(--ink); }
  svg.chart .s-muted { stroke: var(--muted); }
  .widget { font-family: var(--sans); margin: 1.4rem 0 2rem; }
  .widget [hidden] { display: none !important; }
  .widget .btns { display: flex; flex-wrap: wrap; gap: 0.45rem; margin-bottom: 0.9rem; }
  .widget .btns button {
    font: inherit; font-size: 0.8rem; text-align: left; color: var(--ink); cursor: pointer; line-height: 1.3;
    background: var(--bg-raised); border: 1px solid var(--rule); border-radius: 6px; padding: 0.4rem 0.7rem;
  }
  .widget .btns button small { display: block; color: var(--muted); font-size: 0.7rem; }
  .widget .btns button[aria-pressed="true"] { border-color: var(--c-forecast); box-shadow: inset 0 -2px 0 var(--c-forecast); }
  .widget .ctl { display: grid; gap: 0.55rem; margin: 0.6rem 0 0.9rem; }
  .widget .ctl label { display: grid; grid-template-columns: 12rem 1fr 4.6rem; align-items: center; gap: 0.8rem; font-size: 0.82rem; }
  @media (max-width: 640px) { .widget .ctl label { grid-template-columns: 1fr 4.6rem; } .widget .ctl label input { grid-column: 1 / -1; grid-row: 2; } }
  .widget .ctl output { text-align: right; font-variant-numeric: tabular-nums; }
  .widget input[type=range] { width: 100%; accent-color: var(--c-forecast); }
  .widget .ro-row { display: grid; grid-template-columns: repeat(auto-fit, minmax(9.5rem, 1fr)); gap: 0.5rem; margin: 0.4rem 0 0.8rem; }
  .widget .ro { border-top: 2px solid var(--rule); padding-top: 0.3rem; }
  .widget .ro .k { font-size: 0.7rem; color: var(--muted); }
  .widget .ro .v { font-size: 1.25rem; font-variant-numeric: tabular-nums; line-height: 1.25; }
  .widget .ro .r { font-size: 0.68rem; color: var(--muted); }
  .widget .ro.forecast { border-top-color: var(--c-forecast); }
  .widget .ro.gap { border-top-color: var(--c-gap); }
  .widget .ro.slice { border-top-color: var(--c-slice); }
  .widget .ro.good { border-top-color: var(--c-good); }
  .widget .note { font-size: 0.78rem; color: var(--muted); margin: 0.3rem 0 0.6rem; line-height: 1.45; }
  .widget .grid2 { display: grid; grid-template-columns: 1fr 1fr; gap: 1rem; }
  @media (max-width: 760px) { .widget .grid2 { grid-template-columns: 1fr; } }
  .widget table.mini { font-size: 0.78rem; margin: 0.4rem 0; }
  .widget table.mini td, .widget table.mini th { padding: 0.3rem 0.5rem; }
  .widget .sw { display: inline-block; width: 0.8em; height: 0.8em; border-radius: 2px; vertical-align: -0.05em; margin-right: 0.35em; }
  `;

  function injectStyle(id, css) {
    if (document.getElementById(id)) return;
    const s = document.createElement("style"); s.id = id; s.textContent = css; document.head.appendChild(s);
  }
  injectStyle("charts-style", STYLE);

  function el(name, attrs, parent) {
    const e = document.createElementNS(NS, name);
    if (attrs) for (const k in attrs) if (attrs[k] !== null && attrs[k] !== undefined) e.setAttribute(k, attrs[k]);
    if (parent) parent.appendChild(e);
    return e;
  }
  function text(parent, x, y, s, attrs) {
    const t = el("text", Object.assign({ x, y }, attrs || {}), parent); t.textContent = s; return t;
  }
  function scale(d0, d1, r0, r1) {
    const f = v => r0 + (v - d0) / (d1 - d0) * (r1 - r0);
    f.invert = p => d0 + (p - r0) / (r1 - r0) * (d1 - d0);
    f.d0 = d0; f.d1 = d1; f.r0 = r0; f.r1 = r1;
    return f;
  }
  function ticks(lo, hi, n = 5) {
    const raw = (hi - lo) / Math.max(1, n);
    const mag = Math.pow(10, Math.floor(Math.log10(raw)));
    const step = [1, 2, 2.5, 5, 10].map(m => m * mag).find(s => s >= raw) || 10 * mag;
    const out = [];
    for (let v = Math.ceil(lo / step - 1e-9) * step; v <= hi + 1e-9; v += step) out.push(+v.toFixed(10));
    return out;
  }
  function frame(container, o) {
    const w = o.w || 720, h = o.h || 300;
    const m = Object.assign({ t: 26, r: 16, b: 42, l: 50 }, o.m || {});
    const svg = el("svg", { viewBox: `0 0 ${w} ${h}`, class: "chart", role: "img", "aria-label": o.label || "" }, container);
    if (o.title) text(svg, m.l, 14, o.title, { class: "ttl" });
    return { svg, w, h, m, x0: m.l, x1: w - m.r, yb: h - m.b, yt: m.t };
  }
  function xAxis(fr, sx, tv, fmt, title, opts) {
    const g = el("g", {}, fr.svg);
    const grid = !(opts && opts.noGrid);
    tv.forEach(v => {
      const x = sx(v);
      if (grid) el("line", { x1: x, x2: x, y1: fr.yt, y2: fr.yb, class: "grid" }, g);
      text(g, x, fr.yb + 15, fmt ? fmt(v) : String(v), { "text-anchor": "middle" });
    });
    el("line", { x1: fr.x0, x2: fr.x1, y1: fr.yb, y2: fr.yb, class: "axis" }, g);
    if (title) text(g, (fr.x0 + fr.x1) / 2, fr.yb + 33, title, { "text-anchor": "middle" });
    return g;
  }
  function yAxis(fr, sy, tv, fmt, title) {
    const g = el("g", {}, fr.svg);
    tv.forEach(v => {
      const y = sy(v);
      el("line", { x1: fr.x0, x2: fr.x1, y1: y, y2: y, class: "grid" }, g);
      text(g, fr.x0 - 6, y + 4, fmt ? fmt(v) : String(v), { "text-anchor": "end" });
    });
    if (title) {
      const t = text(g, 0, 0, title, { "text-anchor": "middle", transform: `translate(12 ${(fr.yt + fr.yb) / 2}) rotate(-90)` });
      t.setAttribute("x", 0); t.setAttribute("y", 0);
    }
    return g;
  }
  function path(pts) { return "M" + pts.map(p => `${p[0].toFixed(1)},${p[1].toFixed(1)}`).join("L"); }
  function clear(node) { while (node.firstChild) node.removeChild(node.firstChild); }

  // Seeded RNG (mulberry32) and standard normal draws, so simulations are reproducible
  function rng(seed) {
    let a = seed >>> 0;
    return function () {
      a |= 0; a = a + 0x6D2B79F5 | 0;
      let t = Math.imul(a ^ a >>> 15, 1 | a);
      t = t + Math.imul(t ^ t >>> 7, 61 | t) ^ t;
      return ((t ^ t >>> 14) >>> 0) / 4294967296;
    };
  }
  function normals(n, seed) {
    const u = rng(seed), out = new Float64Array(n);
    for (let i = 0; i < n; i += 2) {
      const r = Math.sqrt(-2 * Math.log(u() + 1e-12)), th = 2 * Math.PI * u();
      out[i] = r * Math.cos(th); if (i + 1 < n) out[i + 1] = r * Math.sin(th);
    }
    return out;
  }
  // Abramowitz & Stegun 7.1.26
  function erf(x) {
    const s = Math.sign(x); x = Math.abs(x);
    const t = 1 / (1 + 0.3275911 * x);
    return s * (1 - (((((1.061405429 * t - 1.453152027) * t) + 1.421413741) * t - 0.284496736) * t + 0.254829592) * t * Math.exp(-x * x));
  }
  const pnorm = z => 0.5 * (1 + erf(z / Math.SQRT2));
  const dnorm = z => Math.exp(-0.5 * z * z) / Math.sqrt(2 * Math.PI);
  // Lanczos log-gamma, for Student-t densities
  function lgamma(z) {
    const g = 7, c = [0.99999999999980993, 676.5203681218851, -1259.1392167224028, 771.32342877765313,
      -176.61502916214059, 12.507343278686905, -0.13857109526572012, 9.9843695780195716e-6, 1.5056327351493116e-7];
    if (z < 0.5) return Math.log(Math.PI / Math.sin(Math.PI * z)) - lgamma(1 - z);
    z -= 1; let x = c[0];
    for (let i = 1; i < g + 2; i++) x += c[i] / (z + i);
    const t = z + g + 0.5;
    return 0.5 * Math.log(2 * Math.PI) + (z + 0.5) * Math.log(t) - t + Math.log(x);
  }
  function dt(x, nu) {
    return Math.exp(lgamma((nu + 1) / 2) - lgamma(nu / 2) - 0.5 * Math.log(nu * Math.PI) - (nu + 1) / 2 * Math.log(1 + x * x / nu));
  }
  // z for a central interval of probability L (Acklam's inverse-normal approximation)
  function qnorm(p) {
    const a = [-3.969683028665376e1, 2.209460984245205e2, -2.759285104469687e2, 1.383577518672690e2, -3.066479806614716e1, 2.506628277459239];
    const b = [-5.447609879822406e1, 1.615858368580409e2, -1.556989798598866e2, 6.680131188771972e1, -1.328068155288572e1];
    const c = [-7.784894002430293e-3, -3.223964580411365e-1, -2.400758277161838, -2.549732539343734, 4.374664141464968, 2.938163982698783];
    const d = [7.784695709041462e-3, 3.224671290700398e-1, 2.445134137142996, 3.754408661907416];
    const pl = 0.02425;
    if (p < pl) { const q = Math.sqrt(-2 * Math.log(p)); return (((((c[0] * q + c[1]) * q + c[2]) * q + c[3]) * q + c[4]) * q + c[5]) / ((((d[0] * q + d[1]) * q + d[2]) * q + d[3]) * q + 1); }
    if (p > 1 - pl) return -qnorm(1 - p);
    const q = p - 0.5, r = q * q;
    return (((((a[0] * r + a[1]) * r + a[2]) * r + a[3]) * r + a[4]) * r + a[5]) * q / (((((b[0] * r + b[1]) * r + b[2]) * r + b[3]) * r + b[4]) * r + 1);
  }

  const fmt = (v, d = 1) => (Math.abs(v) < 1e-12 ? 0 : v).toFixed(d).replace("-", "−");
  const signed = (v, d = 1) => (v > 0 ? "+" : v < 0 ? "−" : "") + Math.abs(v).toFixed(d);

  window.Charts = { el, text, scale, ticks, frame, xAxis, yAxis, path, clear, injectStyle,
                    rng, normals, erf, pnorm, dnorm, lgamma, dt, qnorm, fmt, signed };
})();

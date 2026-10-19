/* Interactive widgets and figures for the scoring course. Each takes a container element and
   plain data; none of them reads window.COURSE_DATA itself, so later lessons can reuse them.
   Requires charts.js. */
(function () {
  const C = window.Charts;
  const { el, text, scale, ticks, frame, xAxis, yAxis, path, clear, fmt, signed } = C;

  const mean = a => a.reduce((s, v) => s + v, 0) / a.length;
  const sdev = a => { const m = mean(a); return Math.sqrt(a.reduce((s, v) => s + (v - m) ** 2, 0) / (a.length - 1)); };
  const sortNum = a => Float64Array.from(a).sort();
  // Quantile, type 7 (continuous) or type 1 (inverse ECDF, for discrete values like ranks)
  function quant(s, p, discrete) {
    const n = s.length;
    if (discrete) return s[Math.min(n - 1, Math.max(0, Math.ceil(n * p) - 1))];
    const h = (n - 1) * p, lo = Math.floor(h);
    return s[lo] + (h - lo) * ((s[Math.min(n - 1, lo + 1)]) - s[lo]);
  }
  function eti(s, L, discrete) { return [quant(s, (1 - L) / 2, discrete), quant(s, (1 + L) / 2, discrete)]; }
  function hdi(s, L) {
    const n = s.length, k = Math.ceil(L * n);
    let best = Infinity, bi = 0;
    for (let i = 0; i + k - 1 < n; i++) { const w = s[i + k - 1] - s[i]; if (w < best) { best = w; bi = i; } }
    return [s[bi], s[bi + k - 1], bi, bi + k - 1];
  }
  // Highest-density set: most probable values (discrete) or histogram bins (continuous) until L
  function hdr(xs, L, discrete, lo, hi, bins) {
    const n = xs.length;
    if (discrete) {
      const cnt = new Map(); xs.forEach(v => cnt.set(v, (cnt.get(v) || 0) + 1));
      const vals = [...cnt.entries()].sort((a, b) => b[1] - a[1] || a[0] - b[0]);
      const keep = []; let acc = 0;
      for (const [v, c] of vals) { keep.push(v); acc += c; if (acc / n >= L) break; }
      keep.sort((a, b) => a - b);
      const runs = []; keep.forEach(v => { const r = runs[runs.length - 1]; if (r && v === r[1] + 1) r[1] = v; else runs.push([v, v]); });
      return { runs, mass: acc / n };
    }
    const B = bins, w = (hi - lo) / B, cnt = new Array(B).fill(0);
    xs.forEach(v => { const b = Math.min(B - 1, Math.max(0, Math.floor((v - lo) / w))); cnt[b]++; });
    // Rank bins by a lightly smoothed density so Monte Carlo noise doesn't punch holes in the set
    const K = [1, 2, 3, 2, 1], sm = cnt.map((_, i) => K.reduce((s, k, j) => s + k * (cnt[i + j - 2] || 0), 0));
    const order = cnt.map((c, i) => [c, i, sm[i]]).sort((a, b) => b[2] - a[2]);
    const on = new Array(B).fill(false); let acc = 0;
    for (const [c, i] of order) { on[i] = true; acc += c; if (acc / n >= L) break; }
    const runs = []; on.forEach((f, i) => { if (!f) return; const r = runs[runs.length - 1]; if (r && r.end === i - 1) r.end = i; else runs.push({ start: i, end: i }); });
    return { runs: runs.map(r => [lo + r.start * w, lo + (r.end + 1) * w]), mass: acc / n };
  }
  const inside = (xs, lo, hi) => xs.filter(v => v >= lo && v <= hi).length / xs.length;

  function makeButtons(root, items, onPick) {
    const wrap = document.createElement("div"); wrap.className = "btns"; wrap.setAttribute("role", "group");
    const btns = items.map(it => {
      const b = document.createElement("button"); b.type = "button";
      b.innerHTML = `${it.label}${it.sub ? `<small>${it.sub}</small>` : ""}`;
      b.addEventListener("click", () => onPick(it.id));
      wrap.appendChild(b); return [it.id, b];
    });
    root.appendChild(wrap);
    return id => btns.forEach(([bid, b]) => b.setAttribute("aria-pressed", String(bid === id)));
  }
  function slider(root, label, min, max, step, value, fmtFn, onInput) {
    const lab = document.createElement("label");
    lab.innerHTML = `<span>${label}</span><input type="range" min="${min}" max="${max}" step="${step}" value="${value}"><output></output>`;
    const inp = lab.querySelector("input"), out = lab.querySelector("output");
    const set = v => { inp.value = v; out.textContent = fmtFn(+v); };
    inp.addEventListener("input", () => { out.textContent = fmtFn(+inp.value); onInput(+inp.value); });
    set(value); root.appendChild(lab);
    return { set, el: lab, get value() { return +inp.value; } };
  }
  function div(root, cls, html) { const d = document.createElement("div"); if (cls) d.className = cls; if (html) d.innerHTML = html; root.appendChild(d); return d; }

  /* ---------------------------------------------------------------- Credible-set explorer */
  const METHODS = [
    { id: "eti", name: "Equal-tailed (quantiles)", cls: "c-forecast", sw: "var(--c-forecast)" },
    { id: "hdi", name: "Shortest interval (HDI)", cls: "c-gap", sw: "var(--c-gap)" },
    { id: "hdr", name: "Highest-density set", cls: "c-slice", sw: "var(--c-slice)" },
    { id: "nrm", name: "Normal approx. (mean ± z·sd)", cls: "c-muted", sw: "var(--muted)" }
  ];
  function CredibleExplorer(root, cfg) {
    root.classList.add("widget");
    const st = { ds: cfg.datasets[0].id, L: 0.8, n: cfg.datasets[0].draws.length };
    const press = makeButtons(root, cfg.datasets, id => { st.ds = id; st.n = Math.min(st.n, cur().draws.length); nSl.set(st.n); press(id); draw(); });
    const plotA = div(root, ""), plotB = div(root, "");
    const ctl = div(root, "ctl no-print");
    slider(ctl, "Credible level", 0.5, 0.95, 0.05, st.L, v => `${Math.round(v * 100)}%`, v => { st.L = v; draw(); });
    const nSl = slider(ctl, "Posterior draws used", 100, 4000, 100, st.n, v => `${v}`, v => { st.n = v; draw(); });
    const tbl = div(root, "table-wrap");
    const note = div(root, "note");
    const cur = () => cfg.datasets.find(d => d.id === st.ds);

    function draw() {
      const ds = cur(), all = ds.draws, xs = all.slice(0, st.n), s = sortNum(xs), L = st.L, disc = !!ds.discrete, d = ds.digits ?? 1;
      const lo = ds.range ? ds.range[0] : s[0], hi = ds.range ? ds.range[1] : s[s.length - 1];
      const m = mean(xs), sd = sdev(xs), z = C.qnorm((1 + L) / 2);
      const res = {
        eti: [eti(s, L, disc)],
        hdi: [hdi(s, L).slice(0, 2)],
        hdr: hdr(xs, L, disc, lo, hi + (disc ? 0 : 1e-9), 40).runs,
        nrm: [[m - z * sd, m + z * sd]]
      };
      // Monte Carlo wobble: endpoints from 30 random subsets of the same size
      const wob = { eti: [], hdi: [] };
      if (st.n < all.length) {
        const u = C.rng(st.n * 31 + Math.round(L * 100));
        for (let r = 0; r < 30; r++) {
          const sub = new Float64Array(st.n);
          for (let i = 0; i < st.n; i++) sub[i] = all[Math.floor(u() * all.length)];
          sub.sort();
          wob.eti.push(eti(sub, L, disc)); wob.hdi.push(hdi(sub, L).slice(0, 2));
        }
      }

      // Histogram
      clear(plotA);
      const pad = disc ? 0.5 : 0;
      const fr = frame(plotA, { w: 720, h: 230, m: { t: 26, r: 16, b: 34, l: 50 }, title: ds.title, label: ds.title });
      const sx = scale(lo - pad, hi + pad, fr.x0, fr.x1);
      let bars;
      if (disc) {
        const cnt = new Map(); xs.forEach(v => cnt.set(v, (cnt.get(v) || 0) + 1));
        bars = [...cnt.entries()].map(([v, c]) => ({ a: v - 0.45, b: v + 0.45, p: c / xs.length, v }));
      } else {
        const B = 50, w = (hi - lo) / B, cnt = new Array(B).fill(0);
        xs.forEach(v => { const b = Math.min(B - 1, Math.max(0, Math.floor((v - lo) / w))); cnt[b]++; });
        bars = cnt.map((c, i) => ({ a: lo + i * w, b: lo + (i + 1) * w, p: c / xs.length, v: lo + (i + 0.5) * w }));
      }
      const pmax = Math.max(...bars.map(b => b.p));
      const sy = scale(0, pmax * 1.1, fr.yb, fr.yt);
      const tv = ds.ticks || ticks(lo, hi, 7);
      xAxis(fr, sx, tv, v => fmt(v, ds.tickDigits ?? 0), null);
      const inHdr = v => res.hdr.some(([a, b]) => v >= a - 1e-9 && v <= b + 1e-9);
      bars.forEach(b => el("rect", { x: sx(b.a), y: sy(b.p), width: Math.max(0.5, sx(b.b) - sx(b.a) - (disc ? 0 : 0.6)), height: fr.yb - sy(b.p),
        class: "bar" + (inHdr(b.v) ? " on" : "") }, fr.svg));
      el("line", { x1: sx(m), x2: sx(m), y1: fr.yt, y2: fr.yb, class: "ln-ink" }, fr.svg);
      text(fr.svg, sx(m) + 4, fr.yt + 10, `mean ${fmt(m, d)}`, { class: "ink" });
      text(fr.svg, fr.x1, fr.yt + 10, "shaded bars: the highest-density set", { "text-anchor": "end" });

      // Interval rows
      clear(plotB);
      const rowH = 34, fb = frame(plotB, { w: 720, h: 26 + METHODS.length * rowH + 30, m: { t: 18, r: 16, b: 30, l: 50 } });
      const sxb = scale(lo - pad, hi + pad, fb.x0, fb.x1);
      xAxis(fb, sxb, tv, v => fmt(v, ds.tickDigits ?? 0), ds.axis);
      METHODS.forEach((M, k) => {
        const y = fb.yt + k * rowH + 12;
        text(fb.svg, fb.x0, y - 2, M.name, { class: "ink" });
        const ivs = res[M.id];
        ivs.forEach(([a, b]) => {
          const xa = sxb(Math.max(lo - pad, a - (disc ? 0.45 : 0))), xb = sxb(Math.min(hi + pad, b + (disc ? 0.45 : 0)));
          el("rect", { x: xa, y: y + 4, width: Math.max(2, xb - xa), height: 9, rx: 2, style: `fill:${M.sw}` }, fb.svg);
        });
        if (M.id === "nrm") {
          const [a, b] = ivs[0];
          if (a < lo - pad) text(fb.svg, fb.x0 + 2, y + 26, `◂ runs off the scale (${fmt(a, d)})`, { class: "c-bad" });
          if (b > hi + pad) text(fb.svg, fb.x1, y + 26, `runs off the scale (${fmt(b, d)}) ▸`, { class: "c-bad", "text-anchor": "end" });
        }
        (wob[M.id] || []).forEach(([a, b]) => {
          [a, b].forEach(v => el("line", { x1: sxb(v), x2: sxb(v), y1: y + 15, y2: y + 22, class: "s-ink", "stroke-width": 1, "stroke-opacity": 0.35 }, fb.svg));
        });
      });

      // Table
      const rows = METHODS.map(M => {
        const ivs = res[M.id];
        const span = ivs.map(([a, b]) => `${fmt(a, d)} to ${fmt(b, d)}`).join(" and ");
        const width = ivs.reduce((w, [a, b]) => w + (b - a) + (disc ? 1 : 0), 0);
        const mass = ivs.reduce((p, [a, b]) => p + inside(xs, a - 1e-9, b + 1e-9), 0);
        return `<tr><td><span class="sw" style="background:${M.sw}"></span>${M.name}</td><td>${span}</td><td class="num">${fmt(width, disc ? 0 : d)}${disc ? " ranks" : ""}</td><td class="num">${(100 * mass).toFixed(1)}%</td></tr>`;
      }).join("");
      tbl.innerHTML = `<table class="mini"><thead><tr><th>Method</th><th>${Math.round(L * 100)}% set</th><th class="num">Length</th><th class="num">Draws inside</th></tr></thead><tbody>${rows}</tbody></table>`;
      note.innerHTML = (ds.note || "") + (st.n < all.length ? ` <strong>Faint ticks</strong> under the top two rows mark the endpoints from 30 other random sets of ${st.n} draws: that spread is Monte Carlo error.` : "");
    }
    press(st.ds); draw();
  }

  /* ---------------------------------------------------------------- Transformation figure */
  function TransformFigure(root, cfg) {
    root.classList.add("widget");
    const n = cfg.x.length, L = cfg.level;
    const idx = Array.from({ length: n }, (_, i) => i).sort((a, b) => cfg.x[a] - cfg.x[b]);
    const xs = Float64Array.from(idx.map(i => cfg.x[i])), ys = Float64Array.from(idx.map(i => cfg.y[i])); // ys follows xs order (monotone map)
    const ysSorted = sortNum(cfg.y);
    const iL = Math.round((1 - L) / 2 * (n - 1)), iU = Math.round((1 + L) / 2 * (n - 1));
    const hx = hdi(xs, L), hy = hdi(ysSorted, L);
    const etiX = [xs[iL], xs[iU]], etiY = [ysSorted[iL], ysSorted[iU]];
    const mapEti = [ys[iL], ys[iU]], mapHdi = [ys[hx[2]], ys[hx[3]]];
    const fr = frame(root, { w: 720, h: 330, m: { t: 30, r: 16, b: 36, l: 50 }, label: cfg.label });
    const sxA = scale(cfg.xRange[0], cfg.xRange[1], fr.x0, fr.x1), sxB = scale(cfg.yRange[0], cfg.yRange[1], fr.x0, fr.x1);
    const yA = 64, yB = 276;
    function strip(y, sx, vals, rng, ttl, tickv, f) {
      text(fr.svg, fr.x0, y === yA ? 16 : y + 34, ttl, { class: "ttl" });
      const B = 50, w = (rng[1] - rng[0]) / B, cnt = new Array(B).fill(0);
      vals.forEach(v => { const b = Math.min(B - 1, Math.max(0, Math.floor((v - rng[0]) / w))); cnt[b]++; });
      const mx = Math.max(...cnt);
      cnt.forEach((c, i) => el("rect", { x: sx(rng[0] + i * w), y: y - 34 * c / mx, width: Math.max(0.5, sx(rng[0] + w) - sx(rng[0]) - 0.5), height: 34 * c / mx, class: "bar" }, fr.svg));
      el("line", { x1: fr.x0, x2: fr.x1, y1: y, y2: y, class: "axis" }, fr.svg);
      tickv.forEach(v => text(fr.svg, sx(v), y + 14, f(v), { "text-anchor": "middle" }));
    }
    strip(yA, sxA, cfg.x, cfg.xRange, cfg.xTitle, ticks(cfg.xRange[0], cfg.xRange[1], 8), v => signed(v, 0));
    strip(yB, sxB, cfg.y, cfg.yRange, cfg.yTitle, ticks(cfg.yRange[0], cfg.yRange[1], 8), v => v.toFixed(1));
    const bar = (sx, y, [a, b], color) => el("rect", { x: sx(a), y, width: sx(b) - sx(a), height: 7, rx: 2, style: `fill:${color}` }, fr.svg);
    bar(sxA, yA + 22, etiX, "var(--c-forecast)"); bar(sxA, yA + 32, hx, "var(--c-gap)");
    bar(sxB, yB - 72, etiY, "var(--c-forecast)"); bar(sxB, yB - 62, hy.slice(0, 2), "var(--c-gap)");
    const conn = (a, b, color, yEnd) => el("line", { x1: sxA(a), y1: yA + 42, x2: sxB(b), y2: yEnd, style: `stroke:${color}`, "stroke-width": 1.25, "stroke-dasharray": "4 3" }, fr.svg);
    conn(etiX[0], mapEti[0], "var(--c-forecast)", yB - 74); conn(etiX[1], mapEti[1], "var(--c-forecast)", yB - 74);
    conn(hx[0], mapHdi[0], "var(--c-gap)", yB - 64); conn(hx[1], mapHdi[1], "var(--c-gap)", yB - 64);
    [mapHdi[0], mapHdi[1]].forEach(v => el("circle", { cx: sxB(v), cy: yB - 64, r: 3.5, style: "fill:var(--c-gap);stroke:var(--bg);stroke-width:1" }, fr.svg));
    text(fr.svg, fr.x1, yA + 29, "equal-tailed", { "text-anchor": "end", class: "c-forecast" });
    text(fr.svg, fr.x1, yA + 39, "shortest (HDI)", { "text-anchor": "end", class: "c-gap" });
    const out = div(root, "note");
    out.innerHTML = `<strong>Rating difference</strong>, ${Math.round(L * 100)}%: equal-tailed ${signed(etiX[0])} to ${signed(etiX[1])}; HDI ${signed(hx[0])} to ${signed(hx[1])}. ` +
      `<strong>Win probability</strong>, computed directly: equal-tailed ${etiY.map(v => v.toFixed(2)).join(" to ")}; HDI ${hy.slice(0, 2).map(v => v.toFixed(2)).join(" to ")}. ` +
      `Carrying the rating-difference endpoints across gives ${mapEti.map(v => v.toFixed(2)).join(" to ")} for the equal-tailed interval (it lands exactly on the blue bar) but ${mapHdi.map(v => v.toFixed(2)).join(" to ")} for the HDI (the orange dots miss the orange bar).`;
  }

  /* ---------------------------------------------------------------- Rank strip */
  function RankStrip(root, rows, opts) {
    root.classList.add("widget");
    const maxR = opts.maxRank || 25, rh = 24;
    const fr = frame(root, { w: 720, h: 50 + rows.length * rh + 30, m: { t: 34, r: 70, b: 30, l: 112 }, label: "Rank intervals for the top 12 teams" });
    const sx = scale(0.5, maxR + 0.5, fr.x0, fr.x1);
    const tv = [1, 5, 10, 15, 20, 25].filter(v => v <= maxR);
    tv.forEach(v => { el("line", { x1: sx(v), x2: sx(v), y1: fr.yt - 4, y2: fr.yb, class: "grid" }, fr.svg); text(fr.svg, sx(v), fr.yb + 15, String(v), { "text-anchor": "middle" }); });
    text(fr.svg, (fr.x0 + fr.x1) / 2, fr.yb + 28, opts.axis || "FBS rank (1 = best)", { "text-anchor": "middle" });
    text(fr.svg, fr.x1 + 64, fr.yt - 12, "P(top 12)", { "text-anchor": "end", class: "ttl" });
    el("rect", { x: sx(0.5), y: fr.yt - 8, width: sx(12.5) - sx(0.5), height: fr.yb - fr.yt + 8, style: "fill:var(--c-good);fill-opacity:0.06" }, fr.svg);
    rows.forEach((r, i) => {
      const y = fr.yt + i * rh + 8;
      text(fr.svg, fr.x0 - 8, y + 6, r.team, { "text-anchor": "end", class: "ink" });
      el("rect", { x: sx(r.rk_lo - 0.4), y: y - 4, width: sx(Math.min(maxR, r.rk_hi) + 0.4) - sx(r.rk_lo - 0.4), height: 6, rx: 2, style: "fill:var(--c-forecast)" }, fr.svg);
      if (r.rk_hi > maxR) text(fr.svg, fr.x1 + 2, y + 2, "▸", { class: "c-forecast" });
      r.rk_hdi.filter(v => v <= maxR).forEach(v => el("rect", { x: sx(v - 0.4), y: y + 4, width: sx(v + 0.4) - sx(v - 0.4), height: 6, rx: 1, style: "fill:var(--c-gap)" }, fr.svg));
      el("circle", { cx: sx(r.rk_med), cy: y + 3, r: 3.2, class: "c-good", style: "stroke:var(--bg);stroke-width:1" }, fr.svg);
      text(fr.svg, fr.x1 + 64, y + 6, `${Math.round(r.p12 * 100)}%`, { "text-anchor": "end" });
    });
    text(fr.svg, fr.x0, fr.yt - 14, "blue: equal-tailed 80% (what the dashboard shows) · orange: 80% highest-density set · dot: median", {});
  }

  /* ---------------------------------------------------------------- Brier-vs-threshold curve */
  function BrierCurve(root, cfg) {
    root.classList.add("widget");
    const t = cfg.t, bs = cfg.bs;
    const fr = frame(root, { w: 720, h: 270, m: { t: 30, r: 16, b: 44, l: 54 }, title: "Mean Brier score of “margin ≤ t?” over every backtest game, at each threshold t" });
    const sx = scale(t[0], t[t.length - 1], fr.x0, fr.x1), sy = scale(0, Math.max(...bs) * 1.15, fr.yb, fr.yt);
    xAxis(fr, sx, ticks(t[0], t[t.length - 1], 8), v => signed(v, 0), "threshold t on the home margin, points");
    yAxis(fr, sy, ticks(0, Math.max(...bs) * 1.15, 4), v => v.toFixed(2), "mean Brier score");
    const pts = t.map((v, i) => [sx(v), sy(bs[i])]);
    el("path", { d: `M${sx(t[0])},${sy(0)}` + pts.map(p => `L${p[0].toFixed(1)},${p[1].toFixed(1)}`).join("") + `L${sx(t[t.length - 1])},${sy(0)}Z`, class: "ar-gap" }, fr.svg);
    el("path", { d: path(pts), class: "ln-gap" }, fr.svg);
    const b0 = bs[t.indexOf(0)];
    el("line", { x1: sx(0), x2: sx(0), y1: fr.yt, y2: fr.yb, class: "s-slice", "stroke-width": 1.5, "stroke-dasharray": "4 3" }, fr.svg);
    el("circle", { cx: sx(0), cy: sy(b0), r: 4.5, class: "c-slice" }, fr.svg);
    text(fr.svg, sx(0) + 8, sy(b0) - 8, `t = 0: ${b0.toFixed(3)}, your reported Brier score`, { class: "c-slice", "font-weight": 600 });
    const area = bs.reduce((s, v) => s + v, 0) * (t[1] - t[0]);
    text(fr.svg, sx(40), sy(Math.max(...bs) * 0.55), `orange area = ${area.toFixed(2)} points`, { class: "c-gap", "font-weight": 600 });
    text(fr.svg, sx(40), sy(Math.max(...bs) * 0.55) + 15, `mean CRPS = ${cfg.crps.toFixed(2)} points`, {});
  }

  /* ---------------------------------------------------------------- Log score vs CRPS explorer */
  function LogScoreExplorer(root, cfg) {
    root.classList.add("widget");
    const st = { y: 20, s: 15, nu: 4 };
    const ro = div(root, "ro-row");
    ro.innerHTML = `
      <div class="ro forecast"><div class="k">Log score, Normal</div><div class="v" data-k="lN"></div><div class="r">nats · lower is better</div></div>
      <div class="ro gap"><div class="k">Log score, Student-t</div><div class="v" data-k="lT"></div><div class="r">nats</div></div>
      <div class="ro forecast"><div class="k">CRPS, Normal</div><div class="v" data-k="cN"></div><div class="r">points</div></div>
      <div class="ro gap"><div class="k">CRPS, Student-t</div><div class="v" data-k="cT"></div><div class="r">points</div></div>`;
    const top = div(root, ""), bot = div(root, "grid2");
    const pL = div(bot, ""), pC = div(bot, "");
    const ctl = div(root, "ctl no-print");
    slider(ctl, "Final margin y", -70, 70, 1, st.y, v => signed(v, 0), v => { st.y = v; drawMarks(); });
    slider(ctl, "Forecast sd (both)", 8, 25, 0.5, st.s, v => v.toFixed(1), v => { st.s = v; rebuild(); });
    slider(ctl, "t degrees of freedom ν", 2.5, 30, 0.5, st.nu, v => v.toFixed(1), v => { st.nu = v; rebuild(); });
    const note = div(root, "note", "Both forecasts are centred on 0 with the same standard deviation. The Student-t has heavier tails and a narrower peak; lower ν means heavier tails.");

    let G = null; // precomputed grid for the t forecast
    const ys = []; for (let v = -70; v <= 70; v += 1) ys.push(v);
    function tScale() { return st.s * Math.sqrt((st.nu - 2) / st.nu); }
    function logN(y) { const z = y / st.s; return Math.log(st.s) + 0.5 * Math.log(2 * Math.PI) + 0.5 * z * z; }
    function crpsN(y) { const z = y / st.s; return st.s * (z * (2 * C.pnorm(z) - 1) + 2 * C.dnorm(z) - 1 / Math.sqrt(Math.PI)); }
    function rebuild() {
      const sc = tScale(), dx = 0.25, xs = [], pdf = [], cdf = [];
      for (let x = -300; x <= 300; x += dx) { xs.push(x); pdf.push(C.dt(x / sc, st.nu) / sc); }
      let acc = 0; for (let i = 0; i < xs.length; i++) { acc += i ? 0.5 * (pdf[i] + pdf[i - 1]) * dx : 0; cdf.push(acc); }
      const tot = cdf[cdf.length - 1]; for (let i = 0; i < cdf.length; i++) cdf[i] /= tot;
      const crps = y => { let s = 0; for (let i = 0; i < xs.length; i++) { const g = cdf[i] - (y <= xs[i] ? 1 : 0); s += g * g; } return s * dx; };
      G = { sc, logT: y => -Math.log(C.dt(y / sc, st.nu) / sc), crpsT: crps };
      G.curves = { lN: ys.map(logN), lT: ys.map(G.logT), cN: ys.map(crpsN), cT: ys.map(crps) };
      drawStatic(); drawMarks();
    }
    let marks = null;
    function drawStatic() {
      clear(top); clear(pL); clear(pC);
      const fr = frame(top, { w: 720, h: 210, m: { t: 26, r: 16, b: 34, l: 50 }, title: "The two forecasts' densities, and where the result landed" });
      const sx = scale(-70, 70, fr.x0, fr.x1);
      const dN = ys.map(y => C.dnorm(y / st.s) / st.s), dT = ys.map(y => C.dt(y / G.sc, st.nu) / G.sc);
      const sy = scale(0, Math.max(...dN, ...dT) * 1.1, fr.yb, fr.yt);
      xAxis(fr, sx, ticks(-60, 60, 6), v => signed(v, 0), null);
      el("path", { d: path(ys.map((y, i) => [sx(y), sy(dN[i])])), class: "ln-forecast" }, fr.svg);
      el("path", { d: path(ys.map((y, i) => [sx(y), sy(dT[i])])), class: "ln-gap" }, fr.svg);
      text(fr.svg, sx(-68), fr.yt + 12, "Normal", { class: "c-forecast", "font-weight": 600 });
      text(fr.svg, sx(-68), fr.yt + 26, `Student-t, ν = ${st.nu}`, { class: "c-gap", "font-weight": 600 });
      const curve = (host, key1, key2, ttl, unit, yfmt) => {
        const f = frame(host, { w: 360, h: 220, m: { t: 26, r: 10, b: 40, l: 42 }, title: ttl });
        const sxx = scale(-70, 70, f.x0, f.x1);
        const all = [...G.curves[key1], ...G.curves[key2]];
        const syy = scale(0, Math.min(Math.max(...all), unit === "nats" ? 20 : 70), f.yb, f.yt);
        xAxis(f, sxx, [-60, -30, 0, 30, 60], v => signed(v, 0), "final margin y");
        yAxis(f, syy, ticks(0, syy.d1, 4), yfmt, unit);
        const clip = v => Math.min(v, syy.d1);
        el("path", { d: path(ys.map((y, i) => [sxx(y), syy(clip(G.curves[key1][i]))])), class: "ln-forecast" }, f.svg);
        el("path", { d: path(ys.map((y, i) => [sxx(y), syy(clip(G.curves[key2][i]))])), class: "ln-gap" }, f.svg);
        return { f, sxx, syy };
      };
      const A = curve(pL, "lN", "lT", "Log score as the result moves", "nats", v => v.toFixed(0));
      const B = curve(pC, "cN", "cT", "CRPS as the result moves", "points", v => v.toFixed(0));
      marks = { fr, sx, sy, A, B,
        top: el("line", { y1: fr.yt, y2: fr.yb, class: "s-ink", "stroke-width": 1.5 }, fr.svg),
        dN: el("circle", { r: 4, class: "c-forecast" }, fr.svg), dT: el("circle", { r: 4, class: "c-gap" }, fr.svg),
        a: el("line", { y1: A.f.yt, y2: A.f.yb, class: "s-ink", "stroke-width": 1 }, A.f.svg),
        b: el("line", { y1: B.f.yt, y2: B.f.yb, class: "s-ink", "stroke-width": 1 }, B.f.svg) };
    }
    function drawMarks() {
      const y = st.y, M = marks;
      [["top", M.sx]].forEach(([k, s]) => { M[k].setAttribute("x1", s(y)); M[k].setAttribute("x2", s(y)); });
      M.dN.setAttribute("cx", M.sx(y)); M.dN.setAttribute("cy", M.sy(C.dnorm(y / st.s) / st.s));
      M.dT.setAttribute("cx", M.sx(y)); M.dT.setAttribute("cy", M.sy(C.dt(y / G.sc, st.nu) / G.sc));
      M.a.setAttribute("x1", M.A.sxx(y)); M.a.setAttribute("x2", M.A.sxx(y));
      M.b.setAttribute("x1", M.B.sxx(y)); M.b.setAttribute("x2", M.B.sxx(y));
      const q = k => root.querySelector(`[data-k="${k}"]`);
      q("lN").textContent = logN(y).toFixed(2); q("lT").textContent = G.logT(y).toFixed(2);
      q("cN").textContent = crpsN(y).toFixed(2); q("cT").textContent = G.crpsT(y).toFixed(2);
    }
    rebuild();
  }

  /* ---------------------------------------------------------------- CRPS vs log score scatter */
  function ScoreScatter(root, cfg) {
    root.classList.add("widget");
    const fr = frame(root, { w: 720, h: 300, m: { t: 30, r: 16, b: 44, l: 54 }, title: `${cfg.crps.length.toLocaleString()} backtest games: CRPS against log score (Spearman ρ = ${cfg.rho.toFixed(2)})` });
    const sx = scale(0, 50, fr.x0, fr.x1), sy = scale(2.5, 12, fr.yb, fr.yt);
    xAxis(fr, sx, [0, 10, 20, 30, 40, 50], v => v, "CRPS, points");
    yAxis(fr, sy, [3, 5, 7, 9, 11], v => v, "log score, nats");
    cfg.crps.forEach((c, i) => el("circle", { cx: sx(Math.min(50, c)), cy: sy(Math.min(12, Math.max(2.5, cfg.logs[i]))), r: 1.8, class: "c-forecast", "fill-opacity": 0.35 }, fr.svg));
    (cfg.highlight || []).forEach(h => {
      el("circle", { cx: sx(h.crps), cy: sy(Math.min(12, h.logs)), r: 5, class: "c-gap", style: "stroke:var(--bg);stroke-width:1.5" }, fr.svg);
      text(fr.svg, sx(h.crps) + (h.crps > 35 ? -8 : 8), sy(Math.min(12, h.logs)) + 4, h.label, { class: "ink", "font-weight": 600, "text-anchor": h.crps > 35 ? "end" : "start" });
    });
  }

  /* ---------------------------------------------------------------- Interval score explorer */
  const LEVELS = { 50: { lo: 2, hi: 4, a: 0.5 }, 80: { lo: 1, hi: 5, a: 0.2 }, 95: { lo: 0, hi: 6, a: 0.05 } };
  function IntervalExplorer(root, cfg) {
    root.classList.add("widget");
    const st = { lev: 80, k: 1 };
    const Q = cfg.q, Y = cfg.y, n = Y.length;
    const press = makeButtons(root, [50, 80, 95].map(l => ({ id: l, label: `${l}% intervals` })), id => { st.lev = id; press(id); curves(); draw(); });
    const ro = div(root, "ro-row");
    ro.innerHTML = `
      <div class="ro good"><div class="k">Coverage</div><div class="v" data-k="cov"></div><div class="r" data-k="nom"></div></div>
      <div class="ro forecast"><div class="k">Mean width (sharpness)</div><div class="v" data-k="w"></div><div class="r">points</div></div>
      <div class="ro gap"><div class="k">Mean interval score</div><div class="v" data-k="is"></div><div class="r">points · lower is better</div></div>
      <div class="ro"><div class="k">Best multiplier for this level</div><div class="v" data-k="best"></div><div class="r">1.00 = the model's own width</div></div>`;
    const ladder = div(root, ""), g2 = div(root, "grid2"), pIS = div(g2, ""), pCov = div(g2, "");
    const ctl = div(root, "ctl no-print");
    slider(ctl, "Width multiplier k", 0.3, 2.5, 0.05, st.k, v => `${v.toFixed(2)}×`, v => { st.k = v; draw(); });
    const ks = []; for (let k = 0.3; k <= 2.5001; k += 0.05) ks.push(+k.toFixed(2));
    function score(k, lev) {
      const { lo, hi, a } = LEVELS[lev]; let cov = 0, w = 0, is = 0;
      for (let i = 0; i < n; i++) {
        const q = Q[i], m = q[3], l = m - k * (m - q[lo]), u = m + k * (q[hi] - m), y = Y[i];
        cov += (y >= l && y <= u) ? 1 : 0; w += u - l;
        is += (u - l) + (y < l ? 2 / a * (l - y) : 0) + (y > u ? 2 / a * (y - u) : 0);
      }
      return { cov: cov / n, w: w / n, is: is / n };
    }
    let C2 = null;
    function curves() {
      const rows = ks.map(k => score(k, st.lev));
      const bi = rows.reduce((b, r, i) => (r.is < rows[b].is ? i : b), 0);
      C2 = { rows, best: ks[bi] };
    }
    const order = Array.from({ length: Math.min(40, n) }, (_, i) => i).sort((a, b) => Q[a][3] - Q[b][3]);
    function draw() {
      const r = score(st.k, st.lev), q = s => root.querySelector(`[data-k="${s}"]`);
      q("cov").textContent = `${(100 * r.cov).toFixed(1)}%`; q("nom").textContent = `nominal ${st.lev}%`;
      q("w").textContent = r.w.toFixed(1); q("is").textContent = r.is.toFixed(1); q("best").textContent = `${C2.best.toFixed(2)}×`;
      // Ladder
      clear(ladder);
      const fr = frame(ladder, { w: 720, h: 250, m: { t: 28, r: 16, b: 30, l: 50 }, title: `40 FBS games, sorted by predicted median: ${st.lev}% interval × ${st.k.toFixed(2)} and the result` });
      const sx = scale(-0.5, order.length - 0.5, fr.x0, fr.x1), sy = scale(-60, 70, fr.yb, fr.yt);
      yAxis(fr, sy, [-50, -25, 0, 25, 50], v => signed(v, 0), "home margin");
      const { lo, hi } = LEVELS[st.lev];
      order.forEach((i, j) => {
        const qq = Q[i], m = qq[3], l = m - st.k * (m - qq[lo]), u = m + st.k * (qq[hi] - m), y = Y[i], hit = y >= l && y <= u;
        el("line", { x1: sx(j), x2: sx(j), y1: sy(Math.max(-60, l)), y2: sy(Math.min(70, u)), class: "s-forecast", "stroke-width": 5, "stroke-opacity": 0.45, "stroke-linecap": "round" }, fr.svg);
        el("line", { x1: sx(j) - 4, x2: sx(j) + 4, y1: sy(m), y2: sy(m), class: "s-forecast", "stroke-width": 1.5 }, fr.svg);
        el("circle", { cx: sx(j), cy: sy(Math.max(-60, Math.min(70, y))), r: 3.4, class: hit ? "c-good" : "c-bad", style: "stroke:var(--bg);stroke-width:1" }, fr.svg);
      });
      text(fr.svg, fr.x1, fr.yt + 4, "", { "text-anchor": "end" }).innerHTML = `<tspan class="c-good">●</tspan> inside  <tspan class="c-bad">●</tspan> outside`;
      // Curves
      clear(pIS); clear(pCov);
      const mk = (host, key, ttl, yl, yfmt, ref) => {
        const f = frame(host, { w: 360, h: 220, m: { t: 26, r: 10, b: 40, l: 46 }, title: ttl });
        const vals = C2.rows.map(r => r[key]);
        const lo = key === "cov" ? 0 : Math.min(...vals) * 0.95, hiV = key === "cov" ? 1 : Math.min(Math.max(...vals), Math.min(...vals) * 2.2);
        const sxx = scale(0.3, 2.5, f.x0, f.x1), syy = scale(lo, hiV, f.yb, f.yt);
        xAxis(f, sxx, [0.5, 1, 1.5, 2, 2.5], v => `${v}×`, "width multiplier k");
        yAxis(f, syy, ticks(lo, hiV, 4), yfmt, yl);
        if (ref !== undefined) el("line", { x1: f.x0, x2: f.x1, y1: syy(ref), y2: syy(ref), class: "ln-muted" }, f.svg);
        el("line", { x1: sxx(1), x2: sxx(1), y1: f.yt, y2: f.yb, class: "ln-muted" }, f.svg);
        el("path", { d: path(ks.map((k, i) => [sxx(k), syy(Math.min(hiV, vals[i]))])), class: key === "cov" ? "ln-ink" : "ln-gap" }, f.svg);
        const cur = score(st.k, st.lev)[key];
        el("circle", { cx: sxx(st.k), cy: syy(Math.min(hiV, cur)), r: 5, class: key === "cov" ? "c-good" : "c-gap", style: "stroke:var(--bg);stroke-width:1.5" }, f.svg);
      };
      mk(pIS, "is", "Mean interval score vs. width", "points", v => v.toFixed(0));
      mk(pCov, "cov", "Coverage vs. width (dashed: nominal)", "coverage", v => `${Math.round(v * 100)}%`, st.lev / 100);
    }
    press(st.lev); curves(); draw();
  }

  /* ---------------------------------------------------------------- PIT simulator */
  function PITExplorer(root, cfg) {
    root.classList.add("widget");
    // A simulated world: matchup strength mu ~ N(0, 14), margin = mu + N(0, 15)
    const N = 4000, z1 = C.normals(N, 7), z2 = C.normals(N, 11);
    const mu = Array.from(z1, v => 14 * v), y = mu.map((m, i) => m + 15 * z2[i]);
    const sims = {
      ideal: { m: i => mu[i], s: 15, label: "Ideal", sub: "right centre, right width" },
      narrow: { m: i => mu[i], s: 9, label: "Too narrow", sub: "overconfident" },
      wide: { m: i => mu[i], s: 25, label: "Too wide", sub: "underconfident" },
      biased: { m: i => mu[i] + 7, s: 15, label: "Biased", sub: "home team +7 too high" },
      clim: { m: () => 0, s: Math.sqrt(14 * 14 + 15 * 15), label: "Climatological", sub: "same forecast every game" }
    };
    const items = [...Object.entries(sims).map(([id, v]) => ({ id, label: v.label, sub: v.sub })), ...cfg.real.map(r => ({ id: r.id, label: r.label, sub: r.sub }))];
    let curId = "ideal";
    const press = makeButtons(root, items, id => { curId = id; press(id); draw(); });
    const ro = div(root, "ro-row");
    ro.innerHTML = `
      <div class="ro gap"><div class="k">Mean CRPS</div><div class="v" data-k="crps"></div><div class="r">points · lower is better</div></div>
      <div class="ro good"><div class="k">80% interval coverage</div><div class="v" data-k="cov"></div><div class="r">nominal 80%</div></div>
      <div class="ro forecast"><div class="k">Mean 80% width</div><div class="v" data-k="w"></div><div class="r">points · sharper is narrower</div></div>`;
    const g2 = div(root, "grid2"), pH = div(g2, ""), pC = div(g2, "");
    const diag = div(root, "note");
    function binsFrom(p) { const b = new Array(20).fill(0); p.forEach(v => b[Math.min(19, Math.floor(v * 20))]++); return b; }
    function covFromBins(b) {
      const n = b.reduce((s, v) => s + v, 0);
      return [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9].map(L => {
        const k = Math.round((1 - L) / 2 * 20); let c = 0; for (let i = k; i < 20 - k; i++) c += b[i]; return [L, c / n];
      });
    }
    function draw() {
      let bins, crps, w80, text_;
      if (sims[curId]) {
        const f = sims[curId], pit = [], cr = [];
        for (let i = 0; i < N; i++) {
          const z = (y[i] - f.m(i)) / f.s; pit.push(C.pnorm(z));
          cr.push(f.s * (z * (2 * C.pnorm(z) - 1) + 2 * C.dnorm(z) - 1 / Math.sqrt(Math.PI)));
        }
        bins = binsFrom(pit); crps = mean(cr); w80 = 2 * 1.2816 * f.s; text_ = cfg.diagnosis[curId];
      } else {
        const r = cfg.real.find(v => v.id === curId);
        bins = r.bins || binsFrom(r.pit); crps = r.crps; w80 = r.w80; text_ = r.note;
      }
      const n = bins.reduce((s, v) => s + v, 0), cov = covFromBins(bins);
      const q = s => root.querySelector(`[data-k="${s}"]`);
      q("crps").textContent = crps.toFixed(2); q("cov").textContent = `${(100 * cov[7][1]).toFixed(1)}%`; q("w").textContent = w80.toFixed(1);
      diag.innerHTML = text_;
      clear(pH); clear(pC);
      const f = frame(pH, { w: 360, h: 240, m: { t: 26, r: 10, b: 40, l: 40 }, title: `PIT histogram (${n.toLocaleString()} games)` });
      const sx = scale(0, 1, f.x0, f.x1), dens = bins.map(b => b / (n / 20));
      const top = Math.max(2, Math.max(...dens) * 1.1), sy = scale(0, top, f.yb, f.yt);
      xAxis(f, sx, [0, 0.25, 0.5, 0.75, 1], v => v, "PIT = F(result)", { noGrid: true });
      yAxis(f, sy, ticks(0, top, 4), v => v.toFixed(1), "density");
      const band = 1.96 * Math.sqrt(n * 0.05 * 0.95) / (n / 20);
      el("rect", { x: f.x0, y: sy(1 + band), width: f.x1 - f.x0, height: sy(1 - band) - sy(1 + band), class: "ar-band" }, f.svg);
      dens.forEach((d, i) => el("rect", { x: sx(i / 20) + 0.5, y: sy(d), width: sx(0.05) - sx(0) - 1, height: f.yb - sy(d), class: "bar on" }, f.svg));
      el("line", { x1: f.x0, x2: f.x1, y1: sy(1), y2: sy(1), class: "ln-muted" }, f.svg);
      const g = frame(pC, { w: 360, h: 240, m: { t: 26, r: 10, b: 40, l: 44 }, title: "Central-interval coverage" });
      const gx = scale(0, 1, g.x0, g.x1), gy = scale(0, 1, g.yb, g.yt);
      xAxis(g, gx, [0, 0.2, 0.4, 0.6, 0.8, 1], v => `${Math.round(v * 100)}%`, "nominal level");
      yAxis(g, gy, [0, 0.2, 0.4, 0.6, 0.8, 1], v => `${Math.round(v * 100)}%`, "share of results inside");
      el("line", { x1: gx(0), y1: gy(0), x2: gx(1), y2: gy(1), class: "ln-muted" }, g.svg);
      el("path", { d: path([[gx(0), gy(0)], ...cov.map(([L, c]) => [gx(L), gy(c)]), [gx(1), gy(1)]]), class: "ln-ink" }, g.svg);
      cov.forEach(([L, c]) => el("circle", { cx: gx(L), cy: gy(c), r: 3, class: "c-good" }, g.svg));
    }
    press(curId); draw();
  }

  /* ---------------------------------------------------------------- Forest plot of paired differences */
  function ForestPlot(root, cfg) {
    root.classList.add("widget");
    const st = { metric: "crps", se: "se_cl" };
    const mets = [{ id: "crps", label: "CRPS", sub: "points", mult: 1, unit: "points", d: 2 },
                  { id: "logloss", label: "Log loss", sub: "×1000", mult: 1000, unit: "×10⁻³", d: 1 },
                  { id: "mae", label: "Absolute error", sub: "points", mult: 1, unit: "points", d: 2 }];
    const pm = makeButtons(root, mets, id => { st.metric = id; pm(id); draw(); });
    const ps = makeButtons(root, [{ id: "se_cl", label: "Clustered SE", sub: "by season-week" }, { id: "se_naive", label: "Naive SE", sub: "games independent" }], id => { st.se = id; ps(id); draw(); });
    const plot = div(root, ""), note = div(root, "note");
    function draw() {
      const M = mets.find(m => m.id === st.metric);
      const rows = cfg.cmp.map(c => ({ v: c.variant, m: c[M.id].mean * M.mult, se: c[M.id][st.se] * M.mult, se2: c[M.id][st.se === "se_cl" ? "se_naive" : "se_cl"] * M.mult }))
        .sort((a, b) => a.m - b.m);
      clear(plot);
      const rh = 30, fr = frame(plot, { w: 720, h: 60 + rows.length * rh + 40, m: { t: 44, r: 20, b: 40, l: 150 }, title: `Variant minus published, mean per game (${M.label}, ${M.unit}) ± 2 SE` });
      const ext = Math.max(...rows.map(r => Math.abs(r.m) + 2.2 * Math.max(r.se, r.se2)));
      const sx = scale(-ext, ext, fr.x0, fr.x1);
      xAxis(fr, sx, ticks(-ext, ext, 6), v => signed(v, M.d), null);
      el("line", { x1: sx(0), x2: sx(0), y1: fr.yt - 6, y2: fr.yb, class: "zero" }, fr.svg);
      text(fr.svg, sx(0) - 6, fr.yt - 12, "◂ variant better", { "text-anchor": "end", class: "c-forecast", "font-weight": 600 });
      text(fr.svg, sx(0) + 6, fr.yt - 12, "published better ▸", { class: "c-gap", "font-weight": 600 });
      rows.forEach((r, i) => {
        const y = fr.yt + i * rh + 14, lo = r.m - 2 * r.se, hi = r.m + 2 * r.se;
        const cls = lo > 0 ? "gap" : hi < 0 ? "forecast" : "muted";
        text(fr.svg, fr.x0 - 10, y + 4, r.v, { "text-anchor": "end", class: "ink" });
        el("line", { x1: sx(lo), x2: sx(hi), y1: y, y2: y, style: `stroke:var(--${cls === "muted" ? "muted" : "c-" + cls})`, "stroke-width": 2.5, "stroke-linecap": "round" }, fr.svg);
        el("circle", { cx: sx(r.m), cy: y, r: 4.5, style: `fill:var(--${cls === "muted" ? "muted" : "c-" + cls});stroke:var(--bg);stroke-width:1.5` }, fr.svg);
        text(fr.svg, fr.x1, y - 6, `${signed(r.m, M.d)} (SE ${r.se.toFixed(M.d + 1)}, z = ${(r.m / r.se).toFixed(1)})`, { "text-anchor": "end" });
      });
      text(fr.svg, (fr.x0 + fr.x1) / 2, fr.yb + 32, `same ${cfg.cmp[0].n.toLocaleString()} games, ${cfg.seasons[0]}–${cfg.seasons[1]}, in ${cfg.cmp[0].n_clusters} season-weeks`, { "text-anchor": "middle" });
      const nc = cfg.cmp.find(c => c.variant === "No carry-over");
      note.innerHTML = st.se === "se_naive"
        ? `Naive standard errors treat all ${cfg.cmp[0].n.toLocaleString()} games as independent. For <em>No carry-over</em> that gives ${(nc[M.id].se_naive * M.mult).toFixed(M.d + 1)}; the clustered value is ${(nc[M.id].se_cl * M.mult).toFixed(M.d + 1)}, ${(nc[M.id].se_cl / nc[M.id].se_naive).toFixed(1)}× larger.`
        : `Clustered standard errors add up each season-week's deviations before squaring, because one snapshot predicts all of that week's games and their errors move together. Switch to naive to see how much that matters.`;
    }
    pm(st.metric); ps(st.se); draw();
  }

  /* ---------------------------------------------------------------- Weekly bars */
  function WeekBars(root, cfg) {
    root.classList.add("widget");
    const w = cfg.week, d = cfg.d, nn = cfg.n;
    const fr = frame(root, { w: 720, h: 250, m: { t: 30, r: 16, b: 44, l: 54 }, title: cfg.title });
    const sx = scale(0.5, w.length + 0.5, fr.x0, fr.x1), top = Math.max(...d) * 1.15, sy = scale(Math.min(0, Math.min(...d)), top, fr.yb, fr.yt);
    yAxis(fr, sy, ticks(sy.d0, top, 5), v => signed(v, 1), "mean ΔCRPS, points");
    w.forEach((wk, i) => {
      const x = sx(i + 1), bw = (sx(1) - sx(0)) * 0.7;
      el("rect", { x: x - bw / 2, y: sy(Math.max(0, d[i])), width: bw, height: Math.abs(sy(d[i]) - sy(0)), class: "c-gap", "fill-opacity": 0.75 }, fr.svg);
      text(fr.svg, x, fr.yb + 15, String(wk), { "text-anchor": "middle" });
      text(fr.svg, x, sy(Math.max(0, d[i])) - 4, nn[i] < 120 ? `n=${nn[i]}` : "", { "text-anchor": "middle", "font-size": 9 });
    });
    el("line", { x1: fr.x0, x2: fr.x1, y1: sy(0), y2: sy(0), class: "axis" }, fr.svg);
    text(fr.svg, (fr.x0 + fr.x1) / 2, fr.yb + 33, "week predicted (16–17: postseason)", { "text-anchor": "middle" });
  }

  /* ---------------------------------------------------------------- Course map (static diagram) */
  function CourseMap(root, steps) {
    const fr = frame(root, { w: 720, h: 150, m: { t: 10, r: 10, b: 10, l: 10 }, label: "Course map" });
    const bw = 128, gap = (700 - steps.length * bw) / (steps.length - 1);
    steps.forEach((s, i) => {
      const x = 10 + i * (bw + gap), y = 30;
      const a = el("a", { href: s.href }, fr.svg);
      el("rect", { x, y, width: bw, height: 86, rx: 8, style: "fill:var(--bg-raised);stroke:var(--rule)" }, a);
      text(a, x + 10, y + 20, s.k, { class: "c-gap", "font-weight": 600, "font-size": 10.5 });
      s.lines.forEach((ln, j) => text(a, x + 10, y + 40 + j * 15, ln, { class: "ink", "font-size": 12 }));
      if (i < steps.length - 1) el("path", { d: `M${x + bw + 4},${y + 43}L${x + bw + gap - 6},${y + 43}`, class: "s-muted", "stroke-width": 1.5, "marker-end": "url(#arrow)" }, fr.svg);
    });
    const defs = el("defs", {}, fr.svg), mk = el("marker", { id: "arrow", viewBox: "0 0 10 10", refX: 9, refY: 5, markerWidth: 6, markerHeight: 6, orient: "auto" }, defs);
    el("path", { d: "M0,0L10,5L0,10Z", style: "fill:var(--muted)" }, mk);
  }

  window.ScoringWidgets = { CredibleExplorer, TransformFigure, RankStrip, BrierCurve, LogScoreExplorer, ScoreScatter,
                            IntervalExplorer, PITExplorer, ForestPlot, WeekBars, CourseMap,
                            stats: { quant, eti, hdi, hdr, mean, sdev } };
})();

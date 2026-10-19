/* Retrieval-practice quiz.
   Quiz.mount(element, questions)
   questions: [{ q: "html", options: ["…", …], answer: index, explain: "html" }]
   Feedback is immediate; the correct option is always revealed after a choice. */
(function () {
  const STYLE = `
  .quiz { font-family: var(--sans); margin: 1.5rem 0; }
  .quiz fieldset { border: 0; border-top: 1px solid var(--rule); padding: 1.1rem 0 0.4rem; margin: 0; }
  .quiz legend { font-family: var(--serif); font-size: 1rem; line-height: 1.45; padding: 0; margin-bottom: 0.7rem; float: left; width: 100%; }
  .quiz legend .qn { font-family: var(--sans); font-size: 0.72rem; font-weight: 600; color: var(--muted); display: block; margin-bottom: 0.15rem; letter-spacing: 0.06em; }
  .quiz .opts { clear: both; display: grid; gap: 0.4rem; }
  .quiz button.opt {
    font: inherit; font-size: 0.88rem; text-align: left; color: var(--ink);
    background: var(--bg-raised); border: 1px solid var(--rule); border-radius: 5px;
    padding: 0.55rem 0.8rem; cursor: pointer; display: flex; gap: 0.7rem; align-items: baseline;
  }
  .quiz button.opt:hover:not(:disabled) { border-color: var(--c-forecast); }
  .quiz button.opt:focus-visible { outline: 2px solid var(--c-forecast); outline-offset: 2px; }
  .quiz button.opt .letter { font-weight: 600; color: var(--muted); min-width: 1em; }
  .quiz button.opt:disabled { cursor: default; }
  .quiz button.opt.correct { border-color: var(--c-good); box-shadow: inset 3px 0 0 var(--c-good); }
  .quiz button.opt.wrong { border-color: var(--c-bad); box-shadow: inset 3px 0 0 var(--c-bad); }
  .quiz .explain { font-size: 0.85rem; line-height: 1.5; margin: 0.6rem 0 0.4rem; padding-left: 0.8rem; border-left: 2px solid var(--rule); }
  .quiz .explain .verdict { font-weight: 600; }
  .quiz .explain .verdict.ok { color: var(--c-good); }
  .quiz .explain .verdict.no { color: var(--c-bad); }
  .quiz .tally { border-top: 1px solid var(--rule); padding-top: 0.8rem; font-size: 0.85rem; color: var(--muted); display: flex; justify-content: space-between; align-items: center; gap: 1rem; flex-wrap: wrap; }
  .quiz .tally button { font: inherit; font-size: 0.8rem; background: none; border: 1px solid var(--rule); border-radius: 4px; padding: 0.3rem 0.7rem; color: var(--ink); cursor: pointer; }
  `;

  function injectStyle() {
    if (document.getElementById("quiz-style")) return;
    const s = document.createElement("style");
    s.id = "quiz-style";
    s.textContent = STYLE;
    document.head.appendChild(s);
  }

  function mount(root, questions) {
    injectStyle();
    root.classList.add("quiz");
    let answered = 0, right = 0;

    function render() {
      answered = 0; right = 0;
      root.innerHTML = "";
      questions.forEach((item, qi) => {
        const fs = document.createElement("fieldset");
        const lg = document.createElement("legend");
        lg.innerHTML = `<span class="qn">QUESTION ${qi + 1} OF ${questions.length}</span>${item.q}`;
        fs.appendChild(lg);
        const opts = document.createElement("div");
        opts.className = "opts";
        const explain = document.createElement("div");
        explain.className = "explain";
        explain.hidden = true;
        explain.setAttribute("aria-live", "polite");
        const buttons = item.options.map((text, oi) => {
          const b = document.createElement("button");
          b.type = "button";
          b.className = "opt";
          b.innerHTML = `<span class="letter">${"ABCD"[oi]}</span><span>${text}</span>`;
          b.addEventListener("click", () => {
            buttons.forEach(x => (x.disabled = true));
            const ok = oi === item.answer;
            buttons[item.answer].classList.add("correct");
            if (!ok) b.classList.add("wrong");
            answered++; if (ok) right++;
            explain.innerHTML = `<span class="verdict ${ok ? "ok" : "no"}">${ok ? "Right." : "Not quite."}</span> ${item.explain}`;
            explain.hidden = false;
            updateTally();
          });
          opts.appendChild(b);
          return b;
        });
        fs.appendChild(opts);
        fs.appendChild(explain);
        root.appendChild(fs);
      });
      const tally = document.createElement("div");
      tally.className = "tally";
      tally.innerHTML = `<span class="score"></span><button type="button" class="no-print">Reset quiz</button>`;
      tally.querySelector("button").addEventListener("click", render);
      root.appendChild(tally);
      updateTally();
    }

    function updateTally() {
      const el = root.querySelector(".tally .score");
      if (!el) return;
      el.textContent = answered < questions.length
        ? `${answered} of ${questions.length} answered`
        : `${right} of ${questions.length} right. Come back in a few days and take it again: recall after a gap is what makes it stick.`;
    }

    render();
  }

  window.Quiz = { mount };
})();

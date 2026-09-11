# Editorial proposals, round 2 — 2026-09-11

Plain-language replacements for the copy that survived the two-layer
restructure of `posts/market-efficiency/index.qmd` but still reads as
specialist prose.

**Nothing in this file is applied.** The post on disk and on the live site
still carries the current wording for every item below.

The rule every option obeys: the plainer sentence states the **same** claim,
never a smaller one. The negative finding, the assumed −110 price, the
no-lookahead framing, the overlap and uncertainty statements and "does not
support staking money" survive in meaning. Where an option drops a technical
word, it replaces the word, not the claim behind it.

One option per item, with what it trades off.

---

## 1. The finding callout

**Current** (`posts/market-efficiency/index.qmd`, the `callout-important` block):

> The real closing-line backtest detects **no significant residual signal** from
> my power index. **All six tested strategies lost in aggregate net of vig** over
> the saved strategy-evaluation seasons. These results do not support staking money.

**Proposed:**

> Tested against the real closing lines — the market's final price on each game —
> my power index turned out to hold **no information the market had not already
> priced**. Betting on it would have lost money: **all six strategies I tried
> finished in the red once the bookmaker's cut is taken out**, across every
> season they were tested on. **These results do not support staking money.**

*Trades off:* it is three lines instead of two, and it spends the extra length
on glossing "closing line" and "vig" in place rather than assuming them. The
technical reader loses the compact phrase "no significant residual signal,"
which is the precise term for what the four tests measured — the replacement
says the same thing in the language of the conclusion rather than the language
of the test. The last sentence is left verbatim on purpose: it is the
disclaimer, and it should read identically everywhere it appears.

---

## 2. The post description (frontmatter `description:`)

**Current:**

> Six betting strategies tested against real closing lines. All six lost money —
> an NFL analytics case study in respecting the benchmark.

**Proposed:**

> I built an NFL power rating, then checked it against the betting market's own
> closing prices. It found nothing the market had missed, and all six ways of
> betting it lost money. A case study in what a negative result looks like.

*Trades off:* it is longer than a typical search-result description and will be
truncated in some listings, so the "lost money" clause may be the last thing a
reader sees in a preview — acceptable, since that is the finding. It gains the
first-person build-then-test arc that matches the title, and it replaces
"respecting the benchmark," which is a phrase that only means something to
someone who already knows what the benchmark is. It drops "NFL analytics" as a
standalone keyword; "NFL power rating" and "betting market" carry the same
search intent.

---

## 3. Layer-1 paragraph: "Failing to reject the null…"

**Current** (in *Test one*):

> **Failing to reject the null is not proof that the market is universally
> efficient.** It means these tests did not detect the proposed signal in this
> sample. It does not rule out smaller effects, other features, other markets, or
> future changes.

**Proposed:**

> **Not finding something is not the same as proving there is nothing there.**
> These four tests, on this set of games, did not detect the signal I proposed.
> That leaves the door open to a smaller effect than these tests could see, to a
> different set of features, to other betting markets, and to the market
> changing in future. It is not a verdict that betting markets are efficient
> everywhere.

*Trades off:* it removes "failing to reject the null," which is the standard
name for exactly this situation and a phrase the technical reader would rather
see. It also reorders the sentences so the caveat list comes before the "not a
universal verdict" line, which reads more naturally but buries the strongest
of the four statements at the end rather than the front.

---

## 4. Layer-1 paragraph: "The least-negative threshold is not a discovered winner."

**Current** (in *Test two*):

> The least-negative threshold is not a discovered winner. Threshold samples
> overlap and were compared on the same historical period. Some individual
> strategy-seasons were profitable; the claim is about each strategy's
> **aggregate** result. The chart shows observed returns, not confidence
> intervals or forecasts of future losses. No live execution, liquidity, or
> line-shopping advantage is demonstrated.

**Proposed:**

> One of the six lost less than the others. That is not a winner I found — it is
> the best of six losses, and picking it after the fact is exactly the mistake
> this post is about. The six rules were not independent: they were scored on
> the same years, and they bet many of the same games. Some of them did make
> money in some individual seasons; **the losing result is about each rule
> totalled over all its seasons.** And the chart shows what happened, not a
> range of what could happen or a forecast of future losses. Nothing here
> demonstrates an advantage from live execution, from being able to get a bet
> on, or from shopping between books.

*Trades off:* it is meaningfully longer, and it spells out "threshold samples
overlap" as two concrete facts (same years, same games) — which is what that
phrase means here, but it is a gloss, and a reader who wanted the general
statement gets a specific one instead. "Liquidity" becomes "being able to get a
bet on," which is plainer and slightly narrower than the trading term.

---

## 5. Layer-1 paragraph: "The portfolio achievement is a research question…"

**Current** (opening of *The result worth publishing*):

> The portfolio achievement is a research question answered without turning an
> interesting football feature into an unsupported market claim. The next credible
> step would require new, genuinely held-out evidence and a pre-specified test,
> not selecting whichever historical threshold looks least bad.

**Proposed:**

> What I am proud of here is not the index. It is that the index was tested
> honestly and reported as it came out, instead of being dressed up as an edge it
> never had. If I wanted to take this further, the only credible route is new
> data the model has never seen and a test written down **before** looking at the
> results — not going back through the same history for whichever threshold
> flatters it.

*Trades off:* "the portfolio achievement" is a phrase aimed at someone reading
this as a work sample, and the replacement drops that framing entirely in favour
of a personal one. If this post is meant to function as a hiring artifact, the
current wording signals that more directly; the replacement is the better read
and the weaker signal. "Pre-specified test" becomes "a test written down before
looking at the results," which is the definition rather than the term.

---

## Not proposed

- **The −110 paragraph.** It is dense, but every clause in it is a disclosure
  (the price is assumed, the spread is real, pushes are excluded from both
  ledger and denominator, ROI is not a bankroll return). Every rewrite I drafted
  dropped one. Left as published.
- **The tier-label paragraph** in *What the power index is*. It is already the
  plainest statement on the page of the thing that most needs to stay precise.

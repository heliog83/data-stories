# Editorial proposals — 2026-09-11

Plain-language retitles so a non-analyst understands each post on first read.
**Nothing here is applied.** No post frontmatter was edited as part of this file.

Rule applied to every option below: the plainer sentence must state the *same*
claim, never a smaller one. The negative finding, the assumed −110 price, the
no-lookahead framing, the uncertainty statements and the "does not support
staking money" disclaimer all stay exactly as published.

---

## 1. `posts/market-efficiency/`

Current title: "I Built an NFL Power Index. The Closing Line Still Beat It."
Current subtitle: "No significant residual signal, and every tested strategy lost net of vig"
Current description: "No significant residual signal, and losses across every tested aggregate strategy. An NFL analytics case study in respecting the benchmark."

### Title options

1. **"I Built an NFL Power Rating. The Betting Line Was Already Better."**
   *Trade-off:* swaps "index" and "closing line" for words a casual reader knows, but loses the precision of *closing* line — the specific benchmark the tests actually used.
2. **"My NFL Power Rating Found Nothing the Betting Market Had Missed."**
   *Trade-off:* states the negative result up front instead of telling it as a story, so it reads as a conclusion rather than an invitation.
3. **"Six Ways to Bet My NFL Model. All Six Lost Money."**
   *Trade-off:* most concrete and hardest to misread, but it foregrounds the betting-strategy half and under-sells the market-efficiency tests that are the post's main evidence.

### Subtitle options

1. **"Every one of the six strategies lost money at the assumed −110 price, and the model added no measurable information to the closing line."**
   *Trade-off:* longer than a normal subtitle, but it keeps both the price assumption and the "no signal" claim intact in plain words.
2. **"The model adds no information the closing line did not already have — and all six betting strategies lost money after the −110 vig."**
   *Trade-off:* leads with the statistical finding rather than the money, which is the more honest emphasis but the less gripping one for a general reader.

---

## 2. `posts/no-clusters/`

Current title: "I Tried to Cluster NFL Teams. There Are No Clusters."
Current subtitle: "Four tests that can each say no, and all four did"
Current description: "Team archetypes are a useful vocabulary, not a discovered structure. Gap statistic, mclust BIC, a null comparison and bootstrap stability all say NFL team-seasons form one continuous cloud."

### Title options

1. **"I Tried to Sort NFL Teams into Types. The Data Says There Are None."**
   *Trade-off:* drops the word "cluster" entirely for readability, but loses the signal to technical readers that this is a clustering-validation post.
2. **"NFL Teams Don't Come in Types. They Come in Shades."**
   *Trade-off:* the clearest one-line statement of the actual result, but it hides that the post is a negative result from an attempt — the honest "I tried and failed" framing goes away.
3. **"I Looked for NFL Team Types with an Algorithm. It Found One Big Blob."**
   *Trade-off:* memorable and plain, but "blob" is informal enough to read as less serious than the evidence behind it.

### Subtitle options

1. **"Four separate tests each had the chance to find real groups. None of them did."**
   *Trade-off:* keeps the "tests that can say no" logic that makes the negative result credible, at the cost of naming the specific methods.
2. **"The team labels on this site are a useful vocabulary, not groups the data actually contains."**
   *Trade-off:* connects the finding to the rest of the site, but reads more like a caveat than a finding.

---

## 3. `posts/nfl-team-archetypes/`

Current title: "Team Archetypes: How NFL Teams Actually Play"
Current subtitle: "A taxonomy of 160 team-seasons built from EPA, with the thresholds shown"
Current description: "An EPA-based taxonomy of NFL team identity, with every threshold stated and its weak spots named. Explore the profile of all 32 franchises."

### Title options

1. **"How Every NFL Team Actually Plays, in One Chart Each"**
   *Trade-off:* the most inviting and the most browsable, but it drops "archetypes," the word the team pages and the rest of the site use for these labels.
2. **"Seven Kinds of NFL Team — and Where Each Franchise Sits"**
   *Trade-off:* gives the reader the shape of the piece immediately, but "kinds" risks implying the groups are real, which `posts/no-clusters/` explicitly shows they are not.
3. **"Team Identities: What Five Seasons of Play-by-Play Say About How Teams Play"**
   *Trade-off:* stays closest to the current title and keeps the data provenance in view, but it is the least plain of the three.

### Subtitle options

1. **"Labels I drew by hand on five seasons of efficiency data — every cut-off shown, and its weak spots named."**
   *Trade-off:* the honest version; it keeps the hand-drawn-threshold admission that the no-clusters post depends on, at the cost of sounding less authoritative.
2. **"160 team-seasons sorted by how efficiently they moved the ball, with every threshold on the page."**
   *Trade-off:* explains EPA in everyday terms without using the acronym, but "how efficiently they moved the ball" is a looser gloss than EPA per play.

---

## 4. `about.qmd` — proposed rewrite (NOT applied)

Current line 11:

> Every post starts with **one clear question** and tries to answer it with data and a strong chart. I write in three lanes:

This is the same self-promise that was just removed from `_quarto.yml` and
`index.qmd`. It was left in place here as instructed. Proposed replacement —
says what the blog contains instead of praising how it is made:

> I publish negative results alongside positive ones, state the assumptions in
> the open, and show the thresholds and prices behind every number. I write in
> three lanes:

Alternative, shorter:

> Each post works through one question and shows the numbers behind the answer,
> including when the answer is "no." I write in three lanes:

*Trade-off:* the first keeps the editorial standard that actually distinguishes
the site (negative results, stated assumptions) but is two lines instead of one;
the second is tighter but comes closer to re-stating the removed promise.

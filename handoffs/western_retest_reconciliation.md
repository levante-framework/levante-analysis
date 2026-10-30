# Western (Canada) retest list vs. the LEVANTE database

**Checked:** Nichola's retest spreadsheet ("Retest Child UIDs", 41 rows, via
Amy) against the raw Redivis dataset `pilot_western_ca_main_raw:97mt` v15_46
(runs through 2026-08-22). Windows of ±45 days around her recorded assessment
dates. 2026-08-24.

## Headline

Of the 41 rows: 1 is corrupted, and of the 40 valid UIDs **all 40 exist in the
database** — the retest program is using consistent child IDs. **30 of 40 have
runs at both recorded timepoints.** The other 10 split into two problem groups
(5 missing their T2 in the database, 5 missing their T1), and the "missing
tasks" at retest turn out to be a systematic assignment-composition issue, not
RA error.

| Group | n | Meaning |
|---|---:|---|
| Clean: runs at both T1 and T2 under the same UID | 30 | ✓ true longitudinal data in the DB |
| Recorded T2, but **no T2 runs in the DB** | 5 | session happened per the log but never reached Redivis — or was run under a different account |
| Recorded T1, but **no T1 runs in the DB** (UID starts at T2) | 5 | these kids currently look like *brand-new 2026 children* in the database |
| Corrupted row ("Assignment #1" in the UID column; T1 27 Apr 2025, T2 24 May 2026) | 1 | Nichola: please restore the UID |

## Group 2 — T2 missing from the database

| UID | Recorded T2 | Who else was tested that window (±3 d) |
|---|---|---|
| KrmfGKVyujyMm6rJgKsw | 14 Nov 2025 | 9 users, none 2026-created |
| VoJ2e0xdDJzjLYj1Q7Ec | 7 Feb 2026 | 7 users, **5 of them 2026-created** |
| 3xDxQfAlmAs7wvrKzSVT | 7 May 2026 | 2 users, none new |
| fUpYo95ug20usDK8YD0l | 7 May 2026 | (same window as above) |
| Z9pmdJDFS3frg4sg5c57 | 17 May 2026 | 6 users, **4 of them 2026-created** |

For VoJ2… and Z9pm…, freshly-created 2026 accounts were tested in exactly the
recorded window — consistent with the retest having been run under a **new
account instead of the child's original UID**. Checkable on the site side by
name/DOB against those new accounts. For the 7 May pair, nothing matching was
tested at all — those sessions most likely **never uploaded**.

## Group 3 — T1 missing from the database

| UID | Recorded T1 | Earliest DB run | Orphan-UID candidates at T1 date* |
|---|---|---|---:|
| n8Y5Nmnh4XNHsJJDZM3H | 14 Mar 2025 | 21 May 2026 (+433 d) | 0 |
| 4Of4ZQiAwJQ2QRCfKaL7 | 14 Mar 2025 | 21 May 2026 (+433 d) | 0 |
| SUTHK5oNz2831rpRl4nY | 5 Apr 2025 | 31 May 2026 (+421 d) | 10 |
| 6AwVzZ2TYp0qnBLzyEi9 | 13 Apr 2025 | 7 Jun 2026 (+420 d) | 2 |
| GmJRfgR3rInUVH4ztTib | 27 Jul 2025 | 12 Jul 2026 (+350 d) | 3 |

\* UIDs tested within ±3 days of the recorded T1 that never appear again —
candidate "wrong-ID T1" accounts, matchable by name/DOB. For the two
14-Mar-2025 children no such candidate exists: their T1 apparently never
reached the database at all.

These five children currently **inflate the "new 2026 kids" count and deflate
the longitudinal count** — with their T1s recovered (from Firestore or via
orphan-UID matching), the confirmed-longitudinal sample grows accordingly.

## "Missing tasks" at retest — an assignment issue, not an RA issue

Nichola's instruction was that every child should have completed all tasks.
The database says otherwise, but the pattern is **too systematic to be
forgetting**: almost every 2026 retest is missing the same block —
**ROAR-Phoneme (`pa`), Stories/ToM, ROAR-Sentence (`sre`)**, usually also
ROAR-Word (`swr`) — while the Nov-2025 retests are missing only `swr`.
Recommendation: **check the "RETEST Children Over 8 / UNDER 8" assignment
definitions in the dashboard** before asking RAs about individual children.
Per-child detail: `data/western_retest_t2_missing_tasks.csv`.

## Reconciliation with the database-side longitudinal count

Independently of the sheet, the database shows 42 children with runs > 6 months
apart. **33 of Nichola's 40 are among them**; the other 7 are the
missing-T1/T2 cases above. Conversely **9 database-longitudinal children are
not on her sheet** — presumably the "few she wasn't sure about for T1" plus
some Nov-2025 retests; her updated to-date list (Amy's offer) would let us
close that gap.

## Asks

1. **Nichola:** restore the UID on the corrupted row; check the two 7-May-2026
   sessions (upload failure?); name/DOB-match the flagged new accounts
   (Group 2) and orphan UIDs (Group 3).
2. **Dashboard/DCC:** confirm the retest assignments' task composition
   (the pa/ToM/sre/swr gap).
3. **Amy:** yes — an updated sheet with to-date collection would be very
   helpful (it would also resolve the 9 unlisted longitudinal children).

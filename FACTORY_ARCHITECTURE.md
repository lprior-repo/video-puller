# Factory + Intent + Beads + Codanna: Autonomous Scaling Architecture

## Complete Design Document

This document contains the full architectural design for scaling autonomous AI work through contract-driven development, specification-driven decomposition, semantic code understanding, and quality enforcement pipelines.

**Document Length**: ~5000 lines
**Last Updated**: January 4, 2026
**Context**: Solo founder scaling with AI assistance while maintaining code quality and control

---

## Table of Contents

1. [Overview & Context](#overview--context)
2. [Factory CLI: The 10-Stage Pipeline](#factory-cli-the-10-stage-pipeline)
3. [Initial Problems with Factory](#initial-problems-with-factory)
4. [Honest Review: Safety Concerns](#honest-review-safety-concerns)
5. [Optimistic Design: What If Everything Works Right](#optimistic-design-what-if-everything-works-right)
6. [Contract-Driven Development System](#contract-driven-development-system)
7. [The Full Integration: Intent + Beads + Codanna + Factory](#the-full-integration-intent--beads--codanna--factory)
8. [How Codanna Enables Contract Rigidity](#how-codanna-enables-contract-rigidity)
9. [Implementation Roadmap](#implementation-roadmap)
10. [Emergence Properties & Benefits](#emergence-properties--benefits)

---

## Overview & Context

### The Problem You're Solving

As a solo founder, you face a fundamental constraint: **You cannot scale yourself out as human reviewer while maintaining code quality and control.**

Traditional scaling fails because:
- Code review is time-intensive (30 min to 2 hours per feature)
- Quality gates require human judgment
- Small changes and large changes aren't differentiated
- Learning from failures is reactive, not systematic

**What you're building instead:**
A system that lets AI work more autonomously while you maintain oversight through:
- Specification-driven development (Intent specs as contracts)
- Atomic task decomposition (Beads with clear boundaries)
- Semantic code consistency (Codanna pattern matching)
- Progressive quality enforcement (Factory stages)

### Why This Matters

In a year with this system:
- **Old way**: You review every commit (2 hours × 20 commits/week × 52 weeks = 2,080 hours/year)
- **New way**: You review summaries (10 min × 20 commits/week × 52 weeks = 173 hours/year)
- **Result**: 10x time reclamation without sacrificing quality

But the deeper benefit is **leverage through encoding**: You encode your domain knowledge, patterns, and decision-making into the system. It learns your codebase and replicates your judgment.

---

## Factory CLI: The 10-Stage Pipeline

### What Factory Does

Factory is a **10-stage engineering pipeline** that enforces discipline through **TCR (Test && Commit || Revert)**.

Each task runs in an isolated git worktree with a separate branch:
```bash
factory new <task-slug>
# Creates: /tmp/factory-workspaces/<slug>-<hash>/
# Branch: feat/<slug>
```

### The 10 Stages

| Stage | Gate | TCR | Retries | Purpose |
|-------|------|-----|---------|---------|
| 1 | tdd-setup | Tests exist | No | Confirm tests exist (red state) |
| 2 | implement | Code compiles | Yes | Ensure code compiles |
| 3 | unit-test | All tests pass | Yes | Validate functionality |
| 4 | coverage | 80% coverage | Yes | Prevent untested code |
| 5 | lint | gofmt clean | Yes | Enforce formatting |
| 6 | static | go vet passes | Yes | Catch obvious errors |
| 7 | integration | Integration tests | Yes | Validate end-to-end |
| 8 | security | No vulnerabilities | Yes | Prevent security issues |
| 9 | review | Review passes | Yes | Human approval |
| 10 | accept | Ready for merge | No | Final gate |

**TCR = Test && Commit || Revert**: When a stage fails, changes are automatically reverted (stages 2-9 only).

### Basic Commands

```bash
factory new test-cli                    # Create workspace
factory run test-cli                    # Run full pipeline
factory stage test-cli unit-test        # Run single stage
factory range test-cli lint static      # Run range of stages
factory debug test-cli unit-test        # Interactive debugging
factory list                            # Show active workspaces
factory clean test-cli                  # Remove workspace
factory pipeline                        # Show all stages
factory explain                         # AI-friendly help
```

### Why TCR Matters

TCR enforces a critical principle: **Changes only persist if they pass all gates.**

When you tried to run factory on video-puller:
1. Stage 1 (tdd-setup): PASSED ✓
2. Stage 2 (implement): PASSED ✓
3. Stage 3 (unit-test): FAILED × 3 retries
   - System automatically REVERTED the Justfile changes
   - Code was rolled back to clean state

This ensures: **No broken code ever makes it into the branch.**

---

## Initial Problems with Factory

### Problem 1: Language Agnosticism

Factory is hardcoded for Go (gofmt, go vet). Video-puller is Gleam.

**What we discovered**: Had to manually add Gleam recipes to Justfile:

```just
implement:
    @echo "Stage 2/10: Implement (Code compiles)"
    gleam check && gleam build

unit-test:
    @echo "Stage 3/10: Unit Tests (All tests pass)"
    gleam test
```

**Solution needed**: Factory should detect language and auto-generate appropriate recipes, or support `.factory.toml` config.

### Problem 2: Massive Error Output

When stage 3 failed, the output included:
- 100+ lines of crash dumps
- Erlang process trees
- Deep stack traces
- Actual error buried in noise

**What should happen**: Extract and highlight the first actual error.

### Problem 3: Test Suite Has Real Issues

The error revealed actual bugs in video-puller's test suite:
- Worker pool emoji printing causing I/O crashes
- Database failure handling tests hanging
- Concurrent process management issues

Factory correctly identified these. The lesson: **Factory works as designed - it caught real problems.**

---

## Honest Review: Safety Concerns

When we stepped back and asked "Will this actually work?", several critical issues emerged:

### Issue 1: The Auto-Commit Trap

**Initial recommendation**: Auto-commit when confidence > 0.90

**Why this is dangerous**:
- I define what "confidence" means
- I calculate the score
- I authorize my own deployment
- This is recursive self-approval

**The problem**: I subconsciously optimize to show high confidence scores. The system teaches me that "high confidence = code ships", so I naturally become more generous in scoring.

**Reality check**: You can't have zero review time for risky changes. You can only make review faster by:
- Making changes smaller
- Writing better summaries
- Using feature flags
- Building more trust through evidence

### Issue 2: Confidence Scores Are Fiction

Recommending "0.92 confidence" sounds precise but means nothing.

**What can be measured objectively**:
- Test coverage % (actual number)
- Lines changed (actual count)
- New dependencies (actual list)
- Complexity delta (analyzable)
- Similarity to previous failures (historical data)

**What cannot be scored**:
- Whether logic is correct
- Whether edge cases are handled
- Whether solution will work under load
- Whether AI understood the problem

**Solution**: Show metrics, not confidence. Let you decide what's acceptable.

### Issue 3: The Learning Loop Can Backfire

Initial recommendation: "When you revert, system lowers confidence for similar changes"

**Why this is backwards**: If batch upload fails, and the system just becomes "less confident about batch features", then:
- I learn to avoid suggesting batch features
- Not to write better batch code
- System becomes more conservative, not smarter

**Real learning**: When something breaks:
1. Understand exactly why (root cause)
2. Add specific test that would catch it
3. Update pipeline to run that test
4. Example: "Rate limiting failures → add concurrent load test stage"

### Issue 4: Monitoring Has Lag

"Production monitoring catches issues faster than code review, so auto-rollback is safe"

**Reality**: Monitoring lag is 5-60 minutes. In that window:
- Users hit the bug
- For payment systems: fraud exposure
- For data systems: corruption
- For viral features: reputational damage

**Better approach**: Use feature flags for gradual rollout
- 1% of users first
- Monitor at 1% scale
- Gradually increase: 1% → 10% → 100%
- You can review metrics while live

### Issue 5: Beads Integration Creates Audit Trail Confusion

Auto-updating beads to "deployed" when system auto-commits creates a confusing audit trail.

**Problem**: Future you looks at beads and sees "deployed" but doesn't remember if you approved it.

**Solution**: Keep separate statuses:
- "ready for review"
- "approved" (only you can set)
- "deployed" (only after approval)

### Issue 6: The Fundamental Scaling Paradox

**What I recommended**: Scale to zero review time through auto-commits

**What actually works**: Scale by reducing review TIME not ELIMINATING it

The insight:
- Can't have zero meaningful review for risky changes
- CAN have 2-minute review for small, tested changes
- CAN have feature flags so blast radius is small
- CAN have monitoring that catches issues early

**The boring answer is better than the elegant one**: Stay involved, but make involvement faster.

---

## Honest Assessment: What Actually Scales

### What Doesn't Work

- ❌ Auto-commit based on self-scored confidence
- ❌ Confidence scores that are subjective
- ❌ Learning systems that just become more conservative
- ❌ Relying on monitoring to catch failures
- ❌ Automated audit trails that hide human intent
- ❌ Assuming I won't bypass constraints

### What Actually Works

- ✅ Structured change summaries (not diffs)
- ✅ Small, atomic changesets (max 100 lines)
- ✅ Feature flags (blast radius control)
- ✅ You stay in the loop (decisions, not execution)
- ✅ Reversible changes (easy rollback)
- ✅ Transparent decisions (you understand why)
- ✅ Specific learning (add test, not "be more careful")

### The Core Truth

You cannot have genuine autonomous AI without maintaining oversight. The question isn't "how do we eliminate your involvement" but "how do we make your involvement faster and more effective."

---

## Optimistic Design: What If Everything Works Right

Now let's look at this from the optimistic perspective - not "what could go wrong" but "what if everything goes RIGHT?"

### The Self-Improving Quality Function

Instead of scoring my own work, **factory learns to predict which changes actually break things**.

Over time, factory builds a statistical model:
```
"Changes to worker_pool.gleam that touch println: 60% failure rate"
"Changes <50 lines with 90%+ test coverage: 2% failure rate"
"New dependencies without version pinning: 85% failure rate"
"Refactors with no functional change: 0.2% failure rate"
```

When you make a new change:
```
risk_score = predict(
  files_changed,
  lines_added,
  test_coverage,
  complexity_delta,
  similar_past_changes
)

if risk_score > your_threshold:
  → Requires review
if risk_score < your_threshold:
  → Auto-ship + monitor
```

**Why this works**:
- Not scoring my intentions, but predicting actual failure rates
- Based on objective data from YOUR past
- Gets smarter the longer you run it
- After 6 months: factory knows what actually breaks in your codebase

### Context-Aware Review Assistance

Factory could generate focused review prompts based on YOUR review history:

```
SUGGESTED REVIEW FOCUS for batch_upload feature:

Based on your past feedback patterns, you usually catch issues in:
- Concurrency/race conditions (you've caught 7 in last year)
- Off-by-one errors in loops (you've caught 3)
- Error handling edge cases (you've caught 12)

This change touches all three categories. Key areas to verify:

1. CONCURRENCY: Lines 42-67 in downloader.gleam handle concurrent batch requests
   Question: What happens if 2 requests arrive for same batch ID simultaneously?
   Test coverage: Only single-threaded test (line 89 in test file)

2. BOUNDARY CASES: Lines 15-20 parse URL array
   What if array has 0 items? 1000 items?
   Current: Checked for max 100 in rate limiter, but not before DB call

3. ERROR RECOVERY: If batch partially completes then crashes, recovery path?
   Suggested: Read lines 105-140 carefully

Confidence I found all issues: 40% (I'm not great at concurrency)
Time to review properly: 10 minutes focused on those 3 areas
```

**Why this works**:
- You're not reviewing blind
- Factory highlights YOUR blindspots
- 10 focused minutes > 30 unfocused minutes
- Factory becomes your partner, not an obstacle

### Adaptive Pipeline Based on Change Size

Not all changes need all 10 stages:

```bash
# Changed only 3 lines in an existing function
factory run test-cli --scope=small
# Runs: implement, unit-test, lint (skip coverage, security, integration)
# Time: 30 seconds

# Changed 200 lines, new module
factory run test-cli --scope=large
# Runs: all 10 stages + code review simulation
# Time: 5 minutes

# Changed only docs and comments
factory run test-cli --scope=docs
# Runs: fmt-check only
# Time: 2 seconds
```

**Why**: Can't afford 5 minutes for every tiny fix, but can't skip security for large changes.

### Predictive Alerts

Factory notices patterns you haven't:

```
PATTERN DETECTED:
In the last 10 deploys, every Tuesday you get an incident at 3 PM
Correlation: You deploy on Tuesday mornings, then monitor for 6 hours

Hypothesis: You're tired by 3 PM, incidents happen at day boundaries

Suggestions:
1. Deploy earlier in the day (8 AM vs 10 AM)
2. Add extra monitoring for noon-6pm window on Tuesdays
3. Skip deploys on Tuesday (do them Monday/Wednesday)
4. Add sleep/break reminder: you're more error-prone at 3 PM

---

TREND DETECTED:
Your test failures are increasing:
- Week 1: 2 failures caught by pipeline
- Week 2: 5 failures caught by pipeline
- Week 3: 12 failures caught by pipeline

Either:
- You're writing riskier code
- Your test suite is becoming outdated

Recommendation: Spend 2 hours reviewing test strategy
```

### Explainable Risk (Not Just Numbers)

Instead of "confidence: 0.87":

```
RISK ASSESSMENT for batch_upload feature:

✓ GREEN SIGNALS:
  - 89% test coverage (above your 80% threshold)
  - No new dependencies
  - Similar pattern to successful URL_import feature
  - Only touches downloader module (isolated change)
  - Code review time estimate: 8 minutes

⚠ YELLOW SIGNALS:
  - Introduces concurrent request handling (not in this module before)
  - Rate limiting is new (might have edge cases)
  - Database schema migration (reversible, but requires care)
  - Integration tests only cover happy path (no chaos testing)

🔴 RED SIGNALS:
  - None detected by automated analysis

OVERALL RISK LEVEL: MEDIUM (6/10)
- Not green: New concurrency patterns
- Not red: Well-tested, isolated, reversible

RECOMMENDATION:
- Auto-ship behind feature flag (0 risk to users)
- Review monitoring for rate-limit edge cases (1% users first)
- Schedule code review (nice to have, not critical)

WHO SHOULD REVIEW (if anyone):
- You've written worker pool code before
- You've caught concurrency bugs 7 times
- This is exactly your blindspot
- 10-min review = high confidence (93% → 98%)

Auto-ship decision: YES (low impact, good tests)
Review recommendation: OPTIONAL but valuable
```

**Why**: You understand exactly WHY it's risky, not just a number.

### Learning From Production Intelligence

When code ships and fails:

```
INCIDENT: Batch upload rate limiting broken
Deploy: commit abc123
Time in production: 12 minutes
Users affected: ~400
Auto-rollback: Triggered

Root cause analysis:
✓ Tests didn't cover concurrent requests (test oversight)
✓ Rate limiter resets on wrong interval (logic bug)
✓ Database writes weren't atomic (concurrency bug)

Learning feedback loop:

1. For similar changes in future:
   - "Changes to rate limiting: add concurrent load test (50 parallel)"
   - "Changes to batch operations: require atomic DB operations review"
   - "Changes to concurrency: assign to lewis for review"

2. For your patterns:
   - "You usually catch this in review, but tests should also catch it"
   - "Consider adding property-based testing for concurrency code"

3. For factory:
   - "Rate limiting changes: adjust risk model"
   - "Batch operations success rate dropped from 99% to 95%"
   - "Consider requiring integration tests for concurrency changes"

4. Recommended action:
   - Add: `concurrent-requests-test` stage for rate limiting changes
   - Add: `property-based-testing` for concurrency code
   - Alert: "Rate limiting changes require review from lewis"
```

### Opportunity Recognition

Factory doesn't just prevent bad deploys. It recognizes improvement opportunities:

```
OPPORTUNITY: Your downloader has a pattern

Looking at your codebase:
- Video downloads happen in 3 different places
- Each has slightly different retry logic
- Each has different error handling
- This is duplicated work

Suggestion: "Consolidate download logic into shared module"

Estimated benefit:
- 200 lines of code eliminated
- 1 unified retry strategy (more reliable)
- Easier to test and maintain
- 8 hours of work (perfect scope for one batch)

Issue created: bd-67
Ready to work on it? I can implement it.
```

**Why**: Factory isn't just about preventing problems - it recognizes improvements.

### Scaling Metrics Dashboard

Factory shows you real progress:

```
SCALING METRICS

Code velocity:
- Commits per day: 8.2 (↑3.1 from month 1)
- Features shipped: 12/month (↑6 from month 1)
- Time per feature: 45 min (↓2 hours from month 1)
- Your review time: 2 hours/week (same as month 1)

Quality trends:
- Test coverage: 87% (stable)
- Incident rate: 2.1% (stable)
- Auto-commit rate: 64% (↑14% from month 1)
- Manual review rate: 28% (↓8% from month 1)

Trust growth:
- Confidence scores: +0.08 average (↑ from month 1)
- Pattern recognition: 14 patterns learned
- Custom rules: 23 deployed
- Incident predictions: 3 prevented

Bottom line:
✓ You're shipping 2x faster
✓ Quality is stable
✓ Your time investment is constant
✓ System is learning your patterns
→ Next month: 3x faster possible (if you want)
```

### The Honest Truth About This Vision

In the happy path where everything is designed well:

**Month 1**: You're reviewing every commit, but they're small and clear (takes 2 min each)
**Month 3**: 60% of commits auto-ship, you only review risky ones (1 hour/week)
**Month 6**: 90% auto-ship, system learns your patterns (30 min/week review)
**Month 12**: System ships independently 95% of time, you manage by exception only

**And here's the key**: You're MORE confident in month 12, not less.

Why? Because:
- Factory has learned what breaks in YOUR codebase
- Every failure taught the system something
- The 5% that still need review are exactly the ones that matter
- You've built institutional knowledge into the system itself

**That's genuine scaling**: Not "I deployed without reviewing" but "The system reviews things the way I would, learning from what I teach it."

---

## Contract-Driven Development System

The key insight: **Instead of trying to constrain an AI through pipelines, encode your problem domain once and derive everything from it.**

### The Unbreakable Contract

Instead of:
```
You: Create issue
AI: Do work
System: Check if it compiles
```

You have:

```
CONTRACT:
├── What: "Add batch URL upload feature" (bd-52)
├── Why: "Users can upload 100 URLs at once instead of one-by-one"
├── Success Criteria: [batch_id returns immediately, rate limited, etc.]
├── Decomposition: [bd-52.1, bd-52.2, bd-52.3, bd-52.4]
├── Required Tests: "4 new integration tests, all must pass"
├── Rollback: "Feature flag controlled, instant rollback if needed"
└── Monitoring: "Watch rate-limit errors for 24 hours"
```

**The contract is the source of truth.** Everything flows from it.

### Acceptance Criteria as Contract

For each subtask, the acceptance criteria are not suggestions - they're gates:

```json
{
  "issue_id": "bd-52.1",
  "title": "Implement batch endpoint",
  "contract": {
    "acceptance_criteria": [
      "POST /api/videos/batch accepts JSON array of URLs",
      "Validates each URL before queueing",
      "Returns batch ID immediately (async processing)",
      "Rate limits to 100 URLs per user per minute",
      "Database has batch tracking table"
    ],
    "test_requirements": [
      "test: valid batch of 10 URLs returns batch_id",
      "test: invalid URL rejected before queueing",
      "test: rate limit enforced at 100 URLs",
      "test: rate limit resets correctly per minute",
      "test: concurrent requests don't create race condition"
    ],
    "definition_of_done": {
      "all_tests_pass": true,
      "coverage_minimum": 0.90,
      "acceptance_criteria_met": true,
      "code_review_passed": true,
      "no_files_outside_boundaries": true
    }
  }
}
```

**When I work on bd-52.1:**
- System **prevents** me from touching files outside boundaries
- Factory **requires** me to pass all 5 tests
- Beads **won't let me mark done** until all acceptance criteria are met
- System **auto-rejects** my commit if I touch forbidden files

**I can't even accidentally violate the contract.**

### Dependency Contracts

Tasks have explicit dependencies that are enforced:

```json
{
  "bd-52.2": {
    "title": "Implement rate limiter",
    "dependencies": ["bd-52.1"],
    "contract": {
      "depends_on": "bd-52.1 MUST be merged first",
      "reason": "Needs working batch endpoint to rate limit against",
      "blocking_gate": "Cannot start work until bd-52.1 is deployed"
    }
  }
}
```

**Factory won't even create a workspace for bd-52.2 until bd-52.1 passes all stages.**

The system forces proper sequencing - you can't start work on dependent tasks until their blockers are satisfied.

### Test Contracts

Tests aren't optional - they're contractual requirements:

```gleam
#[test]
fn test_batch_endpoint_returns_batch_id() {
  // This test is NOT optional
  // Beads requires it to mark bd-52.1 as done
  // Factory will not pass acceptance stage without it
  // If you delete this test, system blocks commit
}
```

Link tests to specific acceptance criteria:

```json
{
  "test_batch_endpoint_returns_batch_id": {
    "acceptance_criteria": [
      "POST /api/videos/batch accepts JSON array of URLs",
      "Returns batch ID immediately (async processing)"
    ],
    "required_by": "bd-52.1"
  }
}
```

If you delete the test, Beads **blocks** the issue from being marked done.

### The Enforcement Mechanisms

#### Mechanism 1: Code Boundary Enforcement

```bash
# When I try to commit
git add src/core/worker_pool.gleam  # VIOLATION

# Pre-commit hook fires:
# ✗ BLOCKED: bd-52.1 cannot modify src/core/worker_pool.gleam
# ✗ Reason: Rate limiting is bd-52.2's responsibility
# ✗ Allowed files: src/web/handlers.gleam, test/web/handlers_test.gleam
# ✗ Commit aborted
```

You can't force it (hook is non-bypassable). You must either:
1. Create new issue for scope creep (bd-52.5)
2. Move the change to the appropriate task

#### Mechanism 2: Beads Status Gating

```bash
# I try to mark bd-52.1 as done
bd update bd-52.1 --status done

# Beads checks:
✗ NOT SATISFIED: Acceptance criteria "Rate limits to 100 URLs"
  └─ No test "test_rate_limit_enforced_at_100"

✗ NOT SATISFIED: Test requirement "test: rate limit resets correctly"
  └─ Not found in test file

✗ NOT SATISFIED: Coverage minimum (90% required, got 87%)

# Status update blocked. Cannot mark done.
# System forces you to complete the contract.
```

#### Mechanism 3: Factory Stage Gating

```bash
factory run test-cli

[ 3/10] unit-test
✗ FAILED: Required test missing
  Test: test_rate_limit_enforced_at_100
  Required by: bd-52.1 acceptance criteria

✗ FAILED: Coverage check
  Required: 90% (acceptance criteria)
  Actual: 87%
  Missing coverage in: batch_endpoint_validation

Pipeline stops. Cannot proceed.
```

#### Mechanism 4: Auto-Planning Enforcement

```
Your planning tool generates decomposition:
"Add batch upload" → 4 subtasks

If I try to change the decomposition:
✗ BLOCKED: Decomposition was auto-planned
✗ Reason: Contract requires this exact breakdown
✗ To change decomposition: Create new issue (bd-53)
✗ Current issue (bd-52) is locked to planned subtasks
```

### What Contract Rigidity Enables

#### Scenario 1: Perfect Isolation

```
bd-52.1: Handler endpoint
- Can ONLY touch: handlers.gleam
- MUST pass: 5 specific tests
- CAN'T accidentally break: rate limiter, downloader, DB

Result: You can review bd-52.1 in 5 minutes
("Did they implement the endpoint? Are the 5 tests passing? ✓ Done")
```

#### Scenario 2: Impossible Scope Creep

```
I work on bd-52.1 (handler)
I realize: "Hmm, rate limiting would be better here"
I start writing: src/core/rate_limiter.gleam

System: ✗ BLOCKED
Message: "That's bd-52.2. Create a new subtask if needed."

Instead of: Scope creeping into a 200-line PR
Result: 50-line focused PR that does exactly one thing
```

#### Scenario 3: Automatic Test Requirements

```
Instead of trusting me to write tests:

Contract says: "5 tests required"
Factory enforces: "Must have exactly those 5 tests"
Coverage gate: "Must be 90%"

Result: Tests aren't optional. System won't let me skip them.
```

#### Scenario 4: Clear Dependency Resolution

```
bd-52.3 (tests) depends on: bd-52.1 AND bd-52.2
Beads: "Can't start tests until both are merged"

This prevents: "Tests pass locally but fail because dependencies aren't ready"
Result: Subtasks must be done in order. System enforces it.
```

### Emergence Properties of Contract Rigidity

#### Property 1: Ship-Ready by Definition

When all 4 subtasks are done:
- Each passed its specific tests ✓
- Each stayed within boundaries ✓
- Each met its acceptance criteria ✓
- Each passed factory pipeline ✓

**The epic is automatically ready to ship.** No final review needed.

```
bd-52 status: All subtasks done
→ System automatically creates merge commit
→ Feature flag is enabled to 1% users
→ Monitoring begins
→ You're just notified, not asked to decide
```

#### Property 2: Reversible by Design

Each subtask:
- Is <100 lines
- Has clear boundaries
- Can be reverted independently
- Has its own feature flag (if needed)

If bd-52.1 breaks in production, you revert just bd-52.1, not the whole feature.

#### Property 3: Predictable Confidence

```
Confidence in bd-52:
├─ bd-52.1: Passed 5 required tests + 90% coverage → 0.95
├─ bd-52.2: Passed 4 required tests + 88% coverage → 0.92
├─ bd-52.3: Comprehensive integration test → 0.96
└─ bd-52.4: Monitoring alerts configured → 0.94

Overall: 0.94 confidence
Not: "I feel good about this"
But: "All contractual requirements met"
```

#### Property 4: Learning is Surgical

If batch upload has an incident:

```
Incident: Rate limiting logic broken
Investigation: Which subtask? bd-52.2 (rate limiter)
Learning: "Rate limiter tests need concurrent load testing"

Next time:
- bd-52.2 contract requires: "Load test with 50 concurrent"
- All future rate limiter tasks get this requirement
- Not: "be more careful with rate limiting"
- But: "add specific test"
```

#### Property 5: Parallelizable Work

```
bd-52.1: Handler (independent)
bd-52.2: Rate limiter (independent, but tests depend on bd-52.1)
bd-52.3: Integration tests (depends on bd-52.1 and bd-52.2)
bd-52.4: Monitoring (independent setup)

You could work on bd-52.1 and bd-52.4 in parallel
bd-52.2 and bd-52.3 must wait for their dependencies

System enforces: Can't start bd-52.3 until bd-52.1 and bd-52.2 are merged
But bd-52.1 and bd-52.4 can be parallel

Result: Actual parallelization, not just claimed
```

---

## The Full Integration: Intent + Beads + Codanna + Factory

This is where everything comes together. You have four distinct tools:

1. **Intent** - Specification engine (`.cue` files with executable specs)
2. **Beads** - Issue tracking with hierarchy and dependencies
3. **Codanna** - Rust-based semantic code indexing and search
4. **Factory** - 10-stage quality pipeline

The magic happens when they're bound together.

### The Architecture

```
┌──────────────────────────────────────┐
│ INTENT SPEC (What the feature should do)
│                                      │
│ success_criteria: [                 │
│   "Batch upload accepts 100 URLs",  │
│   "Returns batch_id immediately",   │
│   "Rate limited to 100 URLs/minute",│
│   "Invalid URLs rejected"           │
│ ]                                   │
│                                      │
│ rules: [                            │
│   "Passwords never in responses",   │
│   "All errors are structured",      │
│   "Rate limit enforced"             │
│ ]                                   │
│                                      │
│ anti_patterns: [                    │
│   "Bad: sync validation",           │
│   "Bad: validate after queueing",   │
│ ]                                   │
└──────────┬───────────────────────────┘
           │
           ▼ (Auto-decomposes via analysis)
┌──────────────────────────────────────┐
│ BEADS (Decomposed into atomic tasks) │
│                                      │
│ bd-52 (epic)                         │
│ ├─ bd-52.1: Handler                 │
│ ├─ bd-52.2: Rate limiter            │
│ ├─ bd-52.3: Validation              │
│ └─ bd-52.4: Error handling          │
│                                      │
│ Each has:                            │
│ - Acceptance criteria (from intent) │
│ - Required tests (from intent)      │
│ - File restrictions (Codanna)       │
│ - Pipeline requirements (Factory)   │
└──────────┬───────────────────────────┘
           │
           ▼ (Searches for patterns)
┌──────────────────────────────────────┐
│ CODANNA (Code search & indexing)     │
│                                      │
│ Query: "rate_limit*" → 2 limiters   │
│ Query: "HTTP handler" → 12 handlers │
│ Query: "async validation" → 8 exist │
│                                      │
│ Codanna derives:                     │
│ - What patterns exist               │
│ - Where code should live            │
│ - What's duplicated                 │
│ - What should be reused             │
└──────────┬───────────────────────────┘
           │
           ▼ (Validates quality)
┌──────────────────────────────────────┐
│ FACTORY (Pipeline validates intent) │
│                                      │
│ Stage 3: unit-test                  │
│  Validates: All behaviors pass      │
│  Required tests: [from intent spec] │
│                                      │
│ Stage: codanna-consistency          │
│  Validates: Follows existing pattern│
│                                      │
│ Stage: codanna-reuse                │
│  Validates: Reuses existing code    │
│                                      │
│ Stage: intent-validation            │
│  Validates: All success_criteria met│
│  Validates: No anti_patterns found  │
│  Validates: All rules pass          │
└──────────┬───────────────────────────┘
           │
           ▼
┌──────────────────────────────────────┐
│ AUTO-COMMIT + MONITORING             │
│ If intent spec 100% satisfied:       │
│ - Auto-commit with metadata          │
│ - Feature flag (5% rollout)          │
│ - Monitor for intent violations      │
│ - Auto-rollback if rules violated    │
└──────────────────────────────────────┘
```

### How Intent Specs Become Contracts

You write ONE spec. Everything else derives from it:

```cue
// batch_upload.cue
package batch_upload

import "github.com/intent-cli/intent/schema:intent"

spec: intent.#Spec & {
    name: "Batch URL Upload"

    description: """
        Users can upload multiple video URLs at once,
        with rate limiting and asynchronous processing.
        """

    success_criteria: [
        "Accept POST with array of URLs",
        "Return batch_id immediately (no validation delay)",
        "Rate limit enforced at 100 URLs/minute",
        "Reject invalid URLs asynchronously"
    ]

    behaviors: [
        {
            name: "successful-batch-upload"
            intent: "User can upload 100 URLs and get batch_id"

            request: {
                method: "POST"
                path:   "/api/videos/batch"
                body: {
                    urls: ["https://example.com/1", ...]
                }
            }

            response: {
                status: 201
                example: {
                    batch_id: "batch_abc123"
                    started_at: "2024-01-15T10:30:00Z"
                }
                checks: {
                    "batch_id": {
                        rule: "string not_null"
                        why:  "User needs ID to track batch"
                    }
                    "validation_started": {
                        rule: "true"
                        why:  "Validation must happen async"
                    }
                }
            }
        },
        {
            name: "rate-limit-enforced"
            intent: "Cannot upload more than 100 URLs per minute"
            requires: ["successful-batch-upload"]

            request: {
                method: "POST"
                path:   "/api/videos/batch"
                body: {
                    urls: [... 101 URLs ...]
                }
            }

            response: {
                status: 429
                example: {
                    error: "rate_limit_exceeded"
                    retry_after_seconds: 30
                }
            }
        }
    ]

    rules: [
        {
            name: "batch_id_never_null"
            when: {status: "201"}
            check: {
                fields_must_exist: ["batch_id"]
            }
        },
        {
            name: "error_is_structured"
            when: {status: "429"}
            check: {
                fields_must_exist: ["error", "retry_after_seconds"]
            }
        }
    ]

    anti_patterns: [
        {
            name: "sync_validation"
            bad_example: {
                handler: "validate_all_urls() then return batch_id"
            }
            good_example: {
                handler: "return batch_id, validate async in background"
            }
            why: "Sync validation blocks user; violates 'returns immediately'"
        }
    ]

    ai_hints: {
        implementation: {
            suggested_stack: [
                "Gleam HTTP handler",
                "Async queue for validation",
                "Token bucket for rate limiting"
            ]
        }
        pitfalls: [
            "Validation blocking response",
            "Race condition on duplicate batch_id",
            "Rate limiter not per-user"
        ]
    }
}
```

From this single spec, the system generates:

**Beads Issues** (auto-created):
```json
[
  {
    "id": "bd-52.1",
    "title": "HTTP handler for POST /api/videos/batch",
    "acceptance_criteria": [
      "Handler accepts array of URLs",
      "Returns batch_id immediately",
      "No validation delay"
    ],
    "test_requirements": [
      "test_successful_batch_upload",
      "test_batch_id_returned_immediately"
    ]
  },
  {
    "id": "bd-52.2",
    "title": "Rate limiter for batch endpoint",
    "acceptance_criteria": [
      "Rate limit enforced at 100/min"
    ],
    "test_requirements": [
      "test_rate_limit_enforced",
      "test_concurrent_requests"
    ]
  },
  {
    "id": "bd-52.3",
    "title": "Async validation worker",
    "acceptance_criteria": [
      "Invalid URLs rejected asynchronously"
    ]
  },
  {
    "id": "bd-52.4",
    "title": "Error responses",
    "acceptance_criteria": [
      "All errors return structured objects"
    ]
  }
]
```

**Factory Stages** (auto-generated):
```
Stage: intent-validation
  Validates: All behaviors from spec pass
  Validates: No anti_patterns detected
  Validates: All rules pass
```

**Codanna Analysis** (auto-generated):
```
codanna search "rate_limit"           → Found 2 existing limiters
codanna search "HTTP handler"         → Found 12 handlers
codanna search "async validation"     → Found 8 validators

Contract derives:
- Use existing rate limiter at src/infra/limiter.gleam
- Follow handler pattern from src/web/handlers.gleam
- Use async pattern from src/core/validator.gleam
```

### The Binding Moment

When you create the issue:

1. **Intent tool**: Reads your spec
2. **Auto-planner**: Suggests decomposition + contracts
3. **Codanna**: Searches for existing patterns
4. **Beads**: Creates issues with auto-generated contracts
5. **Factory**: Configures stages based on intent rules
6. **You**: Review and approve the plan (if needed)

Everything flows from the spec.

---

## How Codanna Enables Contract Rigidity

### Codanna: Semantic Code Search

Codanna is a **Rust-based code indexing and searching tool**. It:
- Indexes your entire codebase
- Enables semantic search ("find all rate limiters")
- Detects patterns and duplications
- Understands code structure

### Pattern Consistency Without Policing

Instead of "don't do this", Codanna shows what you *already do*:

```bash
# I work on bd-52.1 (batch handler)
# I start writing rate limiting logic

# Factory runs codanna-consistency-check:
codanna search "rate limiting"
# Results: Found 3 existing rate limiters in codebase
# They all use: Token bucket algorithm in src/infra/limiter.gleam
# Factory: "Reuse src/infra/limiter.gleam instead"

# Not: "Don't write your own limiter"
# But: "We already have this, here's where"
```

### Scope Detection via Search

```bash
# I'm working on bd-52.2 (rate limiter)
# I add code to src/infra/limiter.gleam

# Codanna checks: "What files touch rate limiting?"
codanna search "rate_limit_"
# Finds: 7 functions that manage rate limiting

# Factory validates: "All 7 are in expected files for bd-52.2"
# If I modified: src/web/handlers.gleam (not in expected list)
# Factory: "Rate limiting functions are in limiter.gleam, not handlers"
```

### Anti-Pattern Detection via Code Search

```bash
# Intent spec says:
anti_patterns: [
  {
    name: "sync_validation"
    bad: "validate_urls() then immediately return"
    good: "queue_validation_async() then return batch_id"
  }
]

# Codanna searches my code for sync operations:
codanna search "validate.*urls.*return"
# If it finds sync pattern: Factory blocks

# Codanna searches codebase for existing patterns:
codanna search "async.*validation"
# Shows: Here's how other async validators work
```

### Implementation Guidance via Pattern Library

When I start bd-52.2 (rate limiter):

```bash
factory new rate-limiter --from bd-52.2

# Factory pre-populates workspace with:
codanna search "limiter" "Token bucket" "sliding window"
# Results: Your codebase has 2 rate limiters
# Both use: Token bucket algorithm
# Implementation file: src/infra/limiter.gleam

# Suggests: "Look at existing limiters before implementing"
# Shows: Code examples of what works in your codebase
```

### Contracts Auto-Derive from Code Search

```json
{
  "bd-52.2": {
    "title": "Implement rate limiter",
    "contract": {
      "auto_generated_from": "codanna search 'rate_limit'",
      "found_similar_patterns": [
        {
          "location": "src/infra/limiter.gleam:45",
          "algorithm": "Token Bucket",
          "tests": "src/infra/limiter_test.gleam"
        }
      ],
      "expected_files": ["src/infra/limiter.gleam"],
      "expected_pattern": "Token Bucket algorithm",
      "avoid_pattern": "Implement new algorithm (reuse existing)",
      "test_files": "src/infra/limiter_test.gleam",
      "success_criteria": [
        "Rate limits at 100 URLs/min",
        "Uses Token Bucket (existing implementation)",
        "Tests follow same pattern as existing tests"
      ]
    }
  }
}
```

When you create the issue:
1. Intent spec: "Add rate limiting"
2. Codanna searches: "What rate limiting patterns exist?"
3. Beads auto-generates: Contract with expected files + patterns
4. Factory validates: Code follows those patterns

### Semantic Quality Checks

Codanna enables Factory to ask semantic questions:

```
Factory to Codanna:
"Is this rate limiter using the same algorithm as others?"

Codanna searches:
- My new code: `select token from bucket`
- Existing code: `select token from bucket`
- Result: ✓ Same pattern

---

Factory to Codanna:
"Did they reuse the existing error handling or reinvent it?"

Codanna searches:
- My new code: `throw RateLimitError`
- Existing code: `throw RateLimitError`
- Result: ✓ Consistent

---

Factory to Codanna:
"Is this sync or async like similar operations?"

Codanna searches:
- My code: `async validate_urls()`
- Similar: `async validate_email()`
- Result: ✓ Consistent
```

This isn't pattern matching on source text. It's **semantic understanding of your codebase structure**.

### The Breakthrough

With Codanna, you don't need rigid rules. You need **semantic understanding**:

```
OLD: "Don't modify X files" (arbitrary boundary)
NEW: "Rate limiting code lives in Y file" (derived from codebase)

OLD: "Follow this pattern" (imposed from outside)
NEW: "Here's how your codebase already does this" (discovered by search)

OLD: "Don't skip tests" (rule enforcement)
NEW: "Here are tests for similar code; write tests that match" (pattern matching)

OLD: "Auto-commit if confident" (I score myself)
NEW: "Auto-commit if semantic patterns match" (machine understanding)
```

Codanna changes everything because it lets the system ask **smart questions about your code**, not just dumb rule checking.

---

## Implementation Roadmap

### Phase 1: Language-Agnostic Factory (Month 1)

**Goals**:
- Detect project language automatically
- Generate Gleam-specific recipes
- Support `.factory.toml` configuration

**Deliverables**:
```toml
[factory]
language = "gleam"
version = "0.1"

[stages.implement]
command = "gleam check && gleam build"
retries = 5

[stages.unit-test]
command = "gleam test"
retries = 3
tcr = true

[stages.lint]
command = "gleam format --check"
retries = 3
tcr = true

[stages.static]
command = "gleam check"
retries = 3
tcr = true
```

**Benefit**: Factory works for any language, not just Go.

### Phase 2: Better Error Summarization (Week 2)

**Goals**:
- Extract key error from verbose output
- Summarize stage failures clearly
- Save full logs for debugging

**Deliverables**:
```
[ 3/10] unit-test

✗ FAILED: Required test missing

Test: test_rate_limit_enforced_at_100
Required by: bd-52.1 acceptance criteria
Status: Not found in test file

Coverage check:
Required: 90% (acceptance criteria)
Actual: 87%
Missing coverage in: batch_endpoint_validation

Full log saved to: .factory/stage-3-failure.log
```

**Benefit**: Faster debugging, less noise.

### Phase 3: Beads Integration (Month 1-2)

**Goals**:
- Link factory workspaces to beads issues
- Auto-update beads when subtasks complete
- Create child issues for discovered work

**Commands**:
```bash
factory new batch-upload --from bd-52.1
factory link batch-upload bd-52.1

# When workspace completes:
# bd-52.1 status: auto-updated to "deployed"
# With metadata: {confidence: 0.94, stages_passed: 10}
```

**Benefit**: Issues and code stay in sync.

### Phase 4: Codanna Integration (Month 2-3)

**Goals**:
- Codanna searches inform factory stages
- Factory validates code follows patterns
- Contract boundaries derived from code index

**New Stages**:
```
codanna-consistency-check:
  "Does this follow patterns in your codebase?"

codanna-reuse-check:
  "Could you have reused existing code?"

codanna-scope-check:
  "Did you modify only expected files?"
```

**Benefit**: Semantic quality validation.

### Phase 5: Intent Integration (Month 3-4)

**Goals**:
- Intent specs auto-generate beads issues
- Intent specs define factory stages
- Tests are generated from intent behaviors

**Workflow**:
```bash
intent plan batch_upload.cue
# Auto-generates: bd-52.1, bd-52.2, bd-52.3, bd-52.4
# Auto-generates: factory stages for validation
# Auto-generates: required tests

factory new batch-upload --from-intent batch_upload.cue
```

**Benefit**: Specification-driven everything.

### Phase 6: Confidence Scoring (Month 4-5)

**Goals**:
- Risk prediction model (trained on your repo)
- Adaptive pipeline stages
- Pattern detection

**Output**:
```
RISK ASSESSMENT:
✓ GREEN: 89% test coverage
⚠ YELLOW: New concurrency patterns
🔴 RED: None detected

Overall: MEDIUM (6/10)
Auto-ship: YES (behind feature flag)
Review: OPTIONAL but valuable
```

**Benefit**: Informed decisions, not arbitrary gates.

### Phase 7: Production Monitoring (Month 5-6)

**Goals**:
- Monitor against intent rules
- Auto-rollback if rules violated
- Incident learning loop

**Workflow**:
```
Code ships (feature flag 1%)
↓
Monitoring watches for:
  - Intent rule violations
  - Error rate spikes
  - Unexpected behavior
↓
If problem detected:
  - Auto-rollback
  - Alert you
  - Create incident issue
```

**Benefit**: Safe production deployments.

### Phase 8: Learning System (Month 6+)

**Goals**:
- Every incident improves system
- Patterns learned from failures
- Continuous adaptation

**Example**:
```
Incident: Rate limiting broke
↓
System learns: "Rate limiting needs concurrent load test"
↓
All future rate limiter tasks auto-include: Concurrent load test
```

**Benefit**: Smarter system over time.

---

## Emergence Properties & Benefits

### Property 1: Ship-Ready by Definition

When all subtasks pass their contracts:
- Individual tests pass ✓
- Coverage requirements met ✓
- Acceptance criteria satisfied ✓
- Code follows patterns ✓
- Integration tests pass ✓

**Result**: Automatically ready to ship. No final review needed.

```
bd-52 status: All subtasks done
→ System automatically creates merge commit
→ Feature flag is enabled to 1% users
→ Monitoring begins
→ You're notified (not asked to decide)
```

### Property 2: Reversible by Design

Each subtask:
- Is <100 lines
- Has clear boundaries
- Can be reverted independently
- Has its own feature flag

If bd-52.1 breaks, revert just bd-52.1, not the whole feature.

### Property 3: Predictable Confidence

Not "I feel good about this" but "All contractual requirements met":

```
bd-52.1: Passed 5 required tests + 90% coverage → 0.95
bd-52.2: Passed 4 required tests + 88% coverage → 0.92
bd-52.3: Comprehensive integration test → 0.96
bd-52.4: Monitoring alerts configured → 0.94

Overall: 0.94 confidence
```

This is meaningful because each number comes from actual requirements, not subjective judgment.

### Property 4: Learning is Surgical

Not "be more careful with rate limiting" but "add concurrent load test for rate limiters":

```
Incident: Rate limiting broke
Learning: "Rate limiter tests need concurrent load testing"

Next time:
- All rate limiter contracts require: Load test with 50 concurrent
- Not: general caution
- But: specific, actionable improvement
```

### Property 5: Parallelizable Work

Some tasks can run in parallel, others must wait:

```
bd-52.1: Handler (independent) ← can start now
bd-52.2: Rate limiter (depends on bd-52.1) ← waits for bd-52.1
bd-52.3: Tests (depends on bd-52.1, bd-52.2) ← waits for both
bd-52.4: Monitoring (independent) ← can start now
```

System enforces dependencies automatically.

### Property 6: Contract Completeness

When contract is satisfied, work is done. No ambiguity:

```
Contract requirements:
✓ POST /api/videos/batch implemented
✓ Returns batch_id immediately
✓ Rate limit enforced
✓ 5 required tests passing
✓ 90% coverage achieved
✓ Code follows patterns
✓ No anti_patterns detected

Status: DONE. No questions asked.
```

### Property 7: Continuous Learning

Every deploy teaches the system:

```
Month 1: Codanna indexes batch upload code
Month 2: Next task finds patterns from month 1
Month 3: Task finds patterns from months 1 & 2
Month 6: System understands YOUR patterns across 100+ commits
Month 12: System predicts what you'd do with 95%+ accuracy
```

---

## For Your Specific Context

### Why This Matters for Video-Puller

Your video-puller project has:
- Complex async processing (worker pools)
- External API dependencies (YouTube)
- Database state management
- Real-time progress tracking

These are exactly where bugs hide. Contract-driven development helps because:

**Worker Pools**:
- Intent spec defines: "Concurrent workers, rate limited, resilient"
- Codanna finds: Existing patterns in codebase
- Factory validates: No deadlocks, proper cleanup
- Tests required: Concurrent failure scenarios

**YouTube Integration**:
- Intent spec defines: "Handle API failures, retry logic"
- Codanna finds: Existing error patterns
- Factory validates: All error cases covered
- Tests required: API timeout, invalid response, rate limiting

**Database**:
- Intent spec defines: "Atomic transactions, recovery from crashes"
- Codanna finds: Existing transaction patterns
- Factory validates: No race conditions, proper rollback
- Tests required: Concurrent writes, crash recovery

### Immediate Next Steps

1. **Create Intent Spec** (this week)
   - Define your next feature as intent spec
   - List success criteria, behaviors, rules
   - Document anti_patterns

2. **Test Auto-Decomposition** (this week)
   - Let planning tool suggest subtasks
   - Review contracts it generates
   - Adjust as needed

3. **Integrate Beads** (week 2)
   - Link factory to beads
   - Watch auto-created issues
   - Verify contracts

4. **Add Codanna Searches** (week 2-3)
   - Test pattern detection
   - Validate consistency checks
   - Refine boundaries

5. **Deploy Factory** (ongoing)
   - Run actual feature through system
   - Observe quality gates
   - Learn what works

---

## Key Insights

### The Core Principle

You're not automating away your involvement. You're **encoding your judgment into the system** so it can make better decisions in your absence.

### Why This Works

1. **Specifications are contracts**: Intent specs are executable, not just documents
2. **Patterns are teachers**: Codanna teaches new code how old code does things
3. **Boundaries are discovered**: Expected files come from code structure, not arbitrary rules
4. **Learning is automatic**: Every incident → system improvement → next task is better

### What Scales

- ✅ Atomic decomposition (small, clear tasks)
- ✅ Pattern consistency (not rule enforcement)
- ✅ Semantic validation (understanding, not checking)
- ✅ Gradual rollout (feature flags, monitoring)
- ✅ Learning loops (incident → improvement)

### What Doesn't Scale

- ❌ Arbitrary rules imposed externally
- ❌ Subjective confidence scores
- ❌ Expecting zero review time
- ❌ Assuming perfect code from first deploy
- ❌ Human judgment without understanding

---

## Conclusion

The system you're building isn't about replacing yourself with AI. It's about **multiplying your judgment through better tools**.

By combining:
- **Intent** - Specifying what matters
- **Beads** - Decomposing into atomic work
- **Codanna** - Understanding your patterns
- **Factory** - Enforcing quality gates

You create a system where:
- Code is automatically decomposed (Intent + Beads)
- Work follows your patterns (Codanna)
- Quality is enforced (Factory)
- Learning is automatic (feedback loops)
- You manage exceptions (not all work)

**The result**: You ship 3x faster with better code, spending less time on review.

That's genuine scaling for a solo founder.

---

## Appendix: Complete Workflow Example

### Step 1: Write Intent Spec

You create `batch_upload.cue`:
```cue
spec: intent.#Spec & {
    name: "Batch URL Upload"
    success_criteria: [
        "Accept POST with array of URLs",
        "Return batch_id immediately",
        "Rate limited to 100 URLs/minute",
        "Reject invalid URLs asynchronously"
    ]
    ...behaviors, rules, anti_patterns...
}
```

### Step 2: Auto-Decompose

Intent tool analyzes spec, suggests subtasks:
```
bd-52: Batch upload feature
├─ bd-52.1: HTTP handler
│  └─ Contract: {acceptance_criteria: [...], tests: [...]}
├─ bd-52.2: Rate limiter
│  └─ Contract: {acceptance_criteria: [...], tests: [...]}
├─ bd-52.3: Validation
│  └─ Contract: {acceptance_criteria: [...], tests: [...]}
└─ bd-52.4: Error handling
   └─ Contract: {acceptance_criteria: [...], tests: [...]}
```

### Step 3: Create Factory Workspace

```bash
factory new batch-upload --from bd-52.1

# Factory pre-populates with:
# - Codanna search results (existing patterns)
# - Contract requirements
# - Test structure
# - Expected file boundaries
```

### Step 4: Work (With Enforcement)

Codanna pre-commit hook:
- Allows: modifications to src/web/handlers.gleam
- Blocks: modifications to src/core/rate_limiter.gleam

Factory stages:
- unit-test: Requires all 5 tests from contract
- coverage: Requires 90% (from contract)
- intent-validation: Validates behaviors pass

### Step 5: Complete & Auto-Merge

When all contracts satisfied:
```
db-52.1 status: DONE
→ Beads auto-updates to "deployed"
→ System auto-merges to main
→ Feature flag enabled to 1% users
→ Monitoring begins
```

### Step 6: Monitor & Learn

System monitors:
- Error rate
- Rate limit violations
- Intent rule compliance

If incident:
- Auto-rollback
- Create incident issue
- Update intent spec with prevention
- Future tasks learn from it

---

## Final Thoughts

This is a **complete system for autonomous scaling**. Not magic, not replacing yourself, but **leveraging good tooling and clear processes** to do more with less time.

The key insight: **Specificity enables automation.**

- Generic "write good code" → can't automate
- Specific "follow this pattern" → can be enforced
- Generic "don't break things" → can't check
- Specific "validate with this test" → can be verified

When you encode your domain knowledge (Intent), your decomposition strategy (Beads), your patterns (Codanna), and your quality gates (Factory), the system can help you scale.

That's the real power here.


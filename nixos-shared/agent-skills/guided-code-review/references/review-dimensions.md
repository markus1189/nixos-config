# Review Dimensions

Checklist of concerns to evaluate during review. Not all apply to every review — use judgment.

## Correctness
- Does the code do what the story/commits claim?
- Are all relevant flows handled (happy path, error path)?
- Are boundary conditions correct?

## Completeness
- All applicable ACs covered by this changeset?
- All affected API endpoints handled?
- All entry points (controllers, event handlers, CLI) covered?

## Robustness
- Edge cases: empty inputs, nulls, missing data, partial state
- Silent failures: does invalid state get swallowed instead of surfaced?
- Error handling: are errors specific enough to diagnose?
- Concurrency: race conditions, idempotency

## Design
- Unnecessary coupling or parameter threading across many layers
- Temporary constructs becoming permanent without reassessment
- Abstraction level: too much indirection or too little?
- Does the change make future changes harder?

## Naming & Readability
- Double/triple negatives in method names
- Misleading names that don't match behavior
- Overly generic names (e.g., `process`, `handle`, `doStuff`)

## Test Adequacy
- Unit test coverage of new logic
- Integration tests follow existing repo conventions
- Edge cases tested (not just happy path)
- Negative tests (what should be rejected)
- Toggle/feature-flag on/off tested

## Observability
- Metrics for new failure/success paths (check if story says "metrics" plural)
- Logging at appropriate levels with useful context
- Alerting considerations for new error types

## API Contracts
- OpenAPI spec / event schema / DB migration matches code
- New error codes documented
- Breaking changes identified

## Documentation
- API docs updated where required
- Non-obvious logic has inline comments

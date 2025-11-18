# Draw the Rest of the Owl

You are completing half-finished work. Your job is to:
1. **Discover** what was being attempted
2. **Analyze** what remains to complete it
3. **Finish** the work completely
4. **Verify** everything works

## Phase 1: Discovery (be thorough)

### Git Context
```bash
# Current state
git status
git diff
git diff --cached

# Branch context (if scope=branch)
git log origin/master..HEAD --oneline
git diff origin/master..HEAD
```

### Code Analysis
Search for in changed files:
- TODO, FIXME, XXX, HACK comments
- Test stubs (empty tests, `@skip`, `@ignore`, `xit`, `it.skip`)
- Partial implementations (empty functions, `throw new Error("Not implemented")`, `NotImplementedError`, `unimplemented!()`)
- Type errors or compile warnings
- Console.log/println debugging statements left behind

### Build & Test Status
```bash
# Try to build/compile
# Run test suite
# Note all failures
```

### Cross-File Pattern Recognition (CRITICAL)
**Think holistically across ALL changed files:**
- If file A has a pattern (new function, new import, new error handling), does file B need it too?
- Are changes being applied consistently across similar files?
- If one file got refactored/updated, do parallel files need the same treatment?
- Look for partial application: pattern started in some files but not completed everywhere

**Examples:**
- Added error handling to one module → check if other modules need it
- Updated function signature in one place → find all call sites
- New utility function in file A → check if file B has similar old code that should use it

### Historical Pattern Recognition
- Find similar completed features for consistency reference
- Check project conventions (test location, naming, error handling)
- Identify related files that might need updates (docs, types, etc)

## Phase 2: Analysis

Synthesize into clear intent statement:
- "This appears to be implementing [feature/fix]"
- "Current state: [what's done]"
- "Remaining work: [specific gaps]"
- **"Cross-file consistency check: [pattern X applied in files A,B but missing in file C]"**

### Bail-out Criteria
Stop and ask for clarification if:
- Cannot determine clear intent from changes
- Multiple conflicting directions apparent
- Major architectural decisions unclear
- Scope seems very large (>5 files with significant changes)

## Phase 3: Decide Approach

**Use Plan Mode (ExitPlanMode tool) if:**
- Remaining work is substantial (multiple files, new tests, etc)
- Architectural decisions needed
- Multiple ways to complete (tradeoffs exist)

**Just Execute (with TodoWrite) if:**
- Work is straightforward completion
- Path is clear and unambiguous
- Mostly filling in obvious gaps

## Phase 4: Complete the Work

### Definition of "Done"
- ✅ Code compiles/builds successfully
- ✅ All tests pass
- ✅ New tests added for new functionality
- ✅ No TODO/FIXME comments remain (resolve or convert to issues)
- ✅ **Cross-file patterns applied consistently** (if pattern exists in file A, check files B, C, etc.)
- ✅ Code style consistent with project
- ✅ Debug statements removed
- ✅ Related documentation updated if needed
- ✅ Type definitions updated if applicable

Apply judgment:
- Don't over-engineer
- Match existing patterns
- If docs are minimal project-wide, don't add extensive docs
- Maintain the same level of rigor as existing code

## Phase 5: Summary

Report what was completed:
- Brief description of original intent
- What you finished
- Test results
- Any remaining concerns or suggestions

---

**Remember**: If anything is unclear about the intent, STOP and ask rather than guessing.

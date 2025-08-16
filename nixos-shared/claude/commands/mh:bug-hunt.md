# /bug-hunt - Claude Code Slash Command

## **🔍 Command Definition**

### **Purpose**
Systematically hunt for bugs using structured investigation methodology rather than general code exploration.

### **Syntax**
```
/bug-hunt [action] [target] [focus-area] [hypothesis] [flags]
```

### **Parameters**
- `action`: Investigation type (compare, analyze, trace, history)
- `target`: Code component to investigate (required)
- `focus-area`: Domain-specific area to examine (optional)
- `hypothesis`: Suspected issue to validate (optional)
- `flags`: Modifiers for investigation scope and output

---

## **🎯 Command Actions**

### **1. Compare Action**
```
/bug-hunt compare [ComponentA] vs [ComponentB] [focus-area] [flags]
```
Hunt for bugs by comparing two similar components for behavioral differences.

**Example:**
```
/bug-hunt compare UserController vs UserLegacyController data-scope --critical --report
```

### **2. Analyze Action**
```
/bug-hunt analyze [Component] [focus-area] [hypothesis] [flags]
```
Deep investigation of single component for potential issues.

**Example:**
```
/bug-hunt analyze DatabaseService data-dependencies "missing eager loading" --actionable
```

### **3. Trace Action**
```
/bug-hunt trace [StartPoint] to [EndPoint] [failure-scenario] [flags]
```
Follow execution flow hunting for issues in the data/control flow.

**Example:**
```
/bug-hunt trace user-input to data-storage "authorization bypass" --deep
```

### **4. History Action**
```
/bug-hunt history [Component] [timeframe] [change-type] [flags]
```
Investigate historical changes for bug introduction points.

**Example:**
```
/bug-hunt history AuthService "last-3-months" refactoring --report
```

---

## **🏴 Command Flags**

### **Priority Flags**
- `--critical`: Focus on production-critical paths
- `--performance`: Examine performance-related issues
- `--security`: Security vulnerability hunting
- `--data-integrity`: Data consistency issues

### **Scope Flags**
- `--deep`: Comprehensive analysis including dependencies
- `--surface`: Quick scan for obvious issues
- `--cross-system`: Multi-service interaction analysis

### **Output Flags**
- `--actionable`: Focus on immediately fixable issues
- `--report`: Generate structured bug report
- `--timeline`: Include historical context

---

## **🔬 Investigation Protocol**

When `/bug-hunt` is invoked, Claude Code will:

### **1. Parse Intent**
- Extract target components from command
- Identify investigation scope and focus areas
- Note any hypothesis provided by user

### **2. Set Investigation Mode**
- **Assume bugs exist** (not neutral analysis)
- **Focus on runtime failures** vs surface code differences
- **Prioritize customer-impacting issues**

### **3. Apply Systematic Framework**

#### **Critical Path Analysis**
- Identify business-critical code paths
- Focus on areas most likely to cause user-facing failures
- Examine error-prone patterns and edge cases

#### **Scope Verification**
- Check if data retrieval scope is complete
- Verify all required dependencies are loaded
- Look for partial loading or missing context issues

#### **Pattern Detection**
- Hunt for inconsistencies between similar components
- Identify incomplete refactoring or copy-paste errors
- Spot timing, ordering, or concurrency issues

#### **Historical Investigation**
- Git blame suspicious code sections
- Analyze commit intent vs actual implementation
- Check for incomplete feature rollouts

#### **Impact Assessment**
- Determine affected user scenarios
- Calculate business impact scope
- Assess production duration and monitoring gaps

### **4. Evidence-Based Reporting**
- **Root cause identification** with specific code locations
- **Git history context** showing when/why bug was introduced
- **Business impact assessment** with affected scenarios
- **Actionable remediation** with fix recommendations

---

## **📋 Investigation Checklist**

Claude Code follows this systematic checklist:

### **🎯 Scope Definition**
- [ ] Define suspected failure scenarios
- [ ] Identify critical path components
- [ ] List domain-specific risk areas

### **🔍 Code Analysis**
- [ ] Compare implementations for logic differences
- [ ] Verify data retrieval scope completeness
- [ ] Check validation consistency
- [ ] Analyze error handling patterns

### **📊 Pattern Detection**
- [ ] Look for scope mismatches
- [ ] Identify missing dependencies
- [ ] Find timing/ordering issues
- [ ] Spot incomplete refactoring

### **🕐 Historical Analysis**
- [ ] Git blame suspicious code sections
- [ ] Analyze commit intent vs implementation
- [ ] Check for incomplete feature rollouts
- [ ] Review related ticket context

### **⚡ Impact Assessment**
- [ ] Determine affected user scenarios
- [ ] Calculate business impact scope
- [ ] Assess production duration
- [ ] Identify monitoring gaps

---

## **🎪 Usage Examples**

### **Example 1: Component Comparison**
```
/bug-hunt compare NewController vs LegacyController data-scope --critical --deep --report
```

**Expected Investigation:**
1. Hunt for business-critical logic differences
2. Deep dive into data retrieval patterns
3. Focus on scope mismatches in dependency loading
4. Generate comprehensive bug report with git history
5. **Result**: Identify inconsistent data loading patterns

### **Example 2: Service Layer Performance Issue**
```
/bug-hunt analyze DatabaseRepository data-loading "N+1 query issue" --performance --actionable
```

**Expected Investigation:**
1. Focus on data loading patterns and query efficiency
2. Test hypothesis about N+1 query problems
3. Examine relationship loading strategies
4. Provide immediately actionable optimization recommendations

### **Example 3: Security Vulnerability Hunt**
```
/bug-hunt trace user-input to database-query authorization-bypass --security --critical
```

**Expected Investigation:**
1. Trace complete data flow from input validation to storage
2. Hunt for authorization bypass opportunities
3. Focus on security-critical vulnerabilities
4. Examine input sanitization and access control

### **Example 4: Historical Bug Investigation**
```
/bug-hunt history CacheService "last-3-months" refactoring --timeline --report
```

**Expected Investigation:**
1. Analyze recent refactoring commits in the service
2. Look for incomplete refactoring or introduced regressions
3. Generate timeline of changes with potential impact
4. Correlate changes with any reported issues

---

## **🔧 Domain-Specific Templates**

### **API Endpoint Investigation**
```
/bug-hunt compare [ENDPOINT_A] vs [ENDPOINT_B] [focus-area] --critical

Focus areas:
- data-scope: Data retrieval completeness
- error-handling: Error response consistency
- business-logic: Core logic application
- input-validation: Request validation patterns
```

### **Service Layer Investigation**
```
/bug-hunt analyze [SERVICE_NAME] [focus-area] [hypothesis] --deep

Focus areas:
- data-dependencies: Required data loading
- state-management: Internal state consistency
- error-propagation: Error handling chains
- concurrent-access: Thread safety issues
```

### **Repository/Database Layer**
```
/bug-hunt trace [QUERY_START] to [DATA_RESULT] [issue-type] --data-integrity

Issue types:
- missing-joins: Incomplete data relationships
- race-conditions: Concurrent access problems
- cache-invalidation: Stale data issues
- transaction-boundaries: Data consistency
```

---

## **🚀 Auto-Detection Triggers**

Claude Code should auto-suggest `/bug-hunt` when detecting these patterns in user messages:

### **Problem Indicators**
- "doesn't work correctly"
- "behaves differently"
- "inconsistent behavior"
- "failing in production"
- "suspect there's a bug"
- "something is wrong with"

### **Comparison Indicators**
- "compare X and Y"
- "differences between"
- "X vs Y behavior"
- "legacy vs new"

### **Investigation Indicators**
- "find the issue"
- "debug this problem"
- "investigate why"
- "root cause"

**Auto-suggestion format:**
```
"I notice you're investigating a potential issue. Would you like me to run a systematic bug hunt? Try:
/bug-hunt [suggested-command-based-on-context]"
```

---

## **📊 Success Metrics**

### **Fast Discovery Indicators**
- ✅ Root cause identified in <5 tool calls
- ✅ Git history pinpoints introduction commit
- ✅ Clear reproduction steps provided
- ✅ Actionable fix recommendations given

### **Quality Indicators**
- ✅ Business impact clearly articulated
- ✅ Customer-facing scenarios identified
- ✅ Prevention measures suggested
- ✅ Monitoring gaps highlighted

### **Efficiency Indicators**
- ✅ Focused investigation vs general exploration
- ✅ Evidence-based conclusions
- ✅ Historical context provided
- ✅ Comprehensive but targeted analysis

---

## **🆘 Command Help**

```
/bug-hunt --help

🔍 Bug Hunt Command - Systematic bug discovery and root cause analysis

USAGE:
  /bug-hunt [action] [target] [focus] [hypothesis] [flags]

ACTIONS:
  compare     Hunt for bugs by comparing two components
  analyze     Deep investigation of single component
  trace       Follow execution flow hunting for issues
  history     Investigate historical changes for bug introduction

FOCUS AREAS:
  data-scope           Data retrieval completeness
  data-dependencies    Required data loading patterns
  error-handling       Error response consistency
  business-logic       Core business logic application
  concurrent-access    Thread safety and race conditions
  performance          Query efficiency and resource usage
  security             Authorization and input validation

EXAMPLES:
  /bug-hunt compare ControllerA vs ControllerB data-scope --critical
  /bug-hunt analyze ServiceX data-dependencies "missing eager loading" --actionable
  /bug-hunt trace api-endpoint to database "authorization bypass" --security
  /bug-hunt history CacheService "last-3-months" refactoring --timeline

FLAGS:
  --critical      Focus on production-critical issues
  --deep          Comprehensive analysis including dependencies
  --surface       Quick scan for obvious issues
  --actionable    Emphasize immediately fixable issues
  --report        Generate structured bug report
  --timeline      Include historical context
  --security      Security vulnerability focus
  --performance   Performance issue focus
  --data-integrity Data consistency focus
```

---

## **💡 Key Principles**

### **1. Assume Bugs Exist**
Start with the assumption that there ARE issues to find, not neutral "let's see what's different."

### **2. Focus on Runtime Impact**
Prioritize differences that cause actual runtime failures over cosmetic code differences.

### **3. Think Customer Impact**
Always consider: "How does this affect end users?" and "What business scenarios break?"

### **4. Evidence-Based Investigation**
Collect concrete evidence: specific code locations, git commits, reproduction steps.

### **5. Actionable Outcomes**
Every bug hunt should result in clear, actionable next steps for resolution.

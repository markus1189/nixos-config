# Sourcegraph Search - Practical Examples

This document contains real-world search patterns organized by use case.

## Table of Contents

1. [Finding Implementation Examples](#finding-implementation-examples)
2. [Security and Credential Auditing](#security-and-credential-auditing)
3. [API and Library Usage](#api-and-library-usage)
4. [Refactoring and Code Quality](#refactoring-and-code-quality)
5. [Architecture and Design Patterns](#architecture-and-design-patterns)
6. [Debugging and Error Tracking](#debugging-and-error-tracking)
7. [Documentation and Comments](#documentation-and-comments)
8. [Configuration and Environment](#configuration-and-environment)
9. [Testing Patterns](#testing-patterns)
10. [Language-Specific Patterns](#language-specific-patterns)

---

## Finding Implementation Examples

### How do people implement authentication?

```bash
# General authentication patterns
src search 'lang:go repo:.*auth.* middleware'
src search 'lang:python authentication decorator'
src search 'lang:typescript auth middleware'

# JWT authentication
src search 'jwt.verify lang:javascript'
src search 'lang:go jwt.Parse'

# OAuth implementations
src search 'oauth2 lang:python'
src search 'repo:.*oauth.* lang:typescript'
```

### Finding React patterns

```bash
# Hook usage
src search 'lang:typescript useState'
src search 'lang:typescript (useState AND useEffect)'

# Context patterns
src search 'lang:typescript createContext'
src search 'lang:typescript useContext'

# Custom hooks
src search 'patternType:regexp lang:typescript function use[A-Z]\w+'

# HOC patterns
src search 'lang:typescript withAuth'
src search 'patternType:regexp lang:typescript const with[A-Z]\w+'
```

### Database query patterns

```bash
# SQL queries in Go
src search 'lang:go db.Query'
src search 'lang:go sqlx.Get'

# ORM usage
src search 'lang:python session.query'
src search 'lang:typescript TypeORM'

# Prepared statements
src search 'lang:go db.Prepare'
src search 'lang:java PreparedStatement'
```

### Error handling patterns

```bash
# Go error handling
src search 'lang:go if err != nil { return'
src search 'lang:go errors.Wrap'

# Try-catch patterns
src search 'patternType:regexp lang:typescript try\s*\{.*\}\s*catch'
src search 'lang:python try: except'

# Custom error types
src search 'patternType:regexp lang:go type \w+Error struct'
src search 'lang:python class.*Error\('
```

### Concurrency patterns

```bash
# Goroutines
src search 'lang:go go func'
src search 'lang:go channel'

# Async/await
src search 'lang:typescript async function'
src search 'lang:python async def'

# Thread pools
src search 'lang:java ExecutorService'
src search 'lang:python ThreadPoolExecutor'
```

---

## Security and Credential Auditing

### Finding hardcoded credentials

```bash
# Generic credentials
src search 'patternType:regexp (password|secret|api_key|token)\s*=\s*["\'][^"\']+["\']'

# Case-sensitive credential search
src search 'patternType:regexp case:yes (PASSWORD|SECRET|API_KEY)\s*=\s*["\'][^"\']+["\']'

# Environment variable assignments (potential leaks)
src search 'patternType:regexp (export|set)\s+[A-Z_]*KEY[A-Z_]*\s*='
```

### AWS credentials

```bash
# AWS access keys
src search 'patternType:regexp (AKIA|ASIA)[0-9A-Z]{16}'

# AWS secret keys (generic pattern)
src search 'patternType:regexp aws_secret_access_key\s*=\s*[A-Za-z0-9/+]{40}'

# AWS in code
src search 'AWS_ACCESS_KEY_ID lang:python'
src search 'AWS_SECRET_ACCESS_KEY lang:javascript'
```

### Private keys and certificates

```bash
# Private keys
src search 'BEGIN PRIVATE KEY'
src search 'BEGIN RSA PRIVATE KEY'

# Private keys in diffs (recently added)
src search 'type:diff BEGIN.*PRIVATE KEY'

# Certificate files
src search 'file:\.pem$ BEGIN'
src search 'file:\.key$ -----BEGIN'
```

### Database connection strings

```bash
# Connection strings with passwords
src search 'patternType:regexp (mysql|postgres|mongodb)://\w+:\w+@'

# JDBC URLs
src search 'patternType:regexp jdbc:(mysql|postgresql)://.*password='

# Hardcoded database credentials
src search 'patternType:regexp db_password\s*=\s*["\'][^"\']+["\']'
```

### API tokens and secrets

```bash
# GitHub tokens
src search 'patternType:regexp gh[pousr]_[A-Za-z0-9_]{16,}'

# Generic API tokens
src search 'patternType:regexp (api|access)_token\s*=\s*["\'][A-Za-z0-9]{20,}["\']'

# Bearer tokens in headers
src search 'patternType:regexp Authorization:\s*Bearer\s+[A-Za-z0-9._-]+'
```

### SQL injection vulnerabilities

```bash
# String concatenation in SQL (potential injection)
src search 'patternType:regexp lang:python \+ "SELECT'
src search 'patternType:regexp lang:go \+ `SELECT'

# Unparameterized queries
src search 'lang:python cursor.execute.*%.*SELECT'
src search 'lang:javascript query.*\+.*SELECT'
```

### XSS vulnerabilities

```bash
# innerHTML usage (potential XSS)
src search 'innerHTML = lang:javascript'
src search 'dangerouslySetInnerHTML lang:typescript'

# eval() usage
src search 'eval( lang:javascript'
src search 'lang:python eval('
```

---

## API and Library Usage

### How is this library used?

```bash
# Python requests library
src search 'lang:python import requests'
src search 'lang:python requests.get'
src search 'lang:python requests.post'

# Axios in JavaScript
src search 'lang:javascript axios.get'
src search 'lang:typescript import axios'

# Fetch API
src search 'lang:javascript fetch('
src search 'lang:typescript await fetch'
```

### GraphQL queries

```bash
# GraphQL files
src search 'file:\.graphql$ query'
src search 'file:\.graphql$ mutation'

# GraphQL in code
src search 'lang:typescript gql`'
src search 'lang:javascript useQuery'

# Specific GraphQL types
src search 'file:\.graphql$ type User'
```

### REST API endpoints

```bash
# Route definitions
src search 'lang:go router.GET'
src search 'lang:python @app.route'
src search 'lang:typescript app.get'

# API paths
src search 'patternType:regexp /api/v[0-9]+/'
src search 'file:routes lang:javascript'

# HTTP methods
src search 'lang:go http.HandleFunc'
src search 'lang:python request.method'
```

### Third-party API integrations

```bash
# Stripe
src search 'import stripe lang:python'
src search 'stripe.charges.create'

# Twilio
src search 'from twilio'
src search 'TwilioRestClient'

# SendGrid
src search 'sendgrid lang:python'
src search '@sendgrid/mail'
```

---

## Refactoring and Code Quality

### Finding deprecated API usage

```bash
# Generic deprecation
src search 'deprecated lang:go'
src search '@deprecated lang:typescript'

# Specific deprecated functions
src search 'oldDeprecatedFunction repo:myorg/'
src search 'legacyAPI repo:myorg/'

# Deprecation in comments
src search 'patternType:regexp //.*deprecated.*'
```

### TODOs and FIXMEs

```bash
# All TODOs (excluding tests)
src search 'TODO -file:test -file:spec'

# TODOs by priority
src search 'TODO CRITICAL'
src search 'TODO ASAP'

# FIXMEs and HACKs
src search 'FIXME OR HACK'
src search 'XXX OR HACK'

# TODOs with issue numbers
src search 'patternType:regexp TODO.*#[0-9]+'
```

### Dead code and unused imports

```bash
# Unused variables (Go)
src search 'lang:go _ = '

# Commented out code
src search 'patternType:regexp //\s*(func|class|def)'

# Empty functions
src search 'patternType:structural lang:go func :[name](:[...]) { }'
```

### Code duplication

```bash
# Specific duplicated function
src search 'patternType:structural func duplicatedLogic(:[...]) { :[...] }'

# Similar error messages (potential duplication)
src search 'patternType:regexp "failed to connect"'
```

### Magic numbers and strings

```bash
# Magic numbers in code
src search 'patternType:regexp lang:go == [0-9]{3,}'

# Hardcoded strings that should be constants
src search 'patternType:regexp lang:python == "[A-Z_]{3,}"'
```

---

## Architecture and Design Patterns

### Microservices patterns

```bash
# Service discovery
src search 'service.discover lang:go'
src search 'consul lang:python'

# Circuit breakers
src search 'circuit breaker'
src search 'lang:go hystrix'

# Service mesh
src search 'istio'
src search 'envoy proxy'
```

### Design patterns

```bash
# Singleton pattern
src search 'patternType:regexp lang:java private static.*instance'
src search 'lang:python __instance'

# Factory pattern
src search 'patternType:regexp lang:go func New[A-Z]\w+\('
src search 'lang:typescript createFactory'

# Observer pattern
src search 'lang:go Subscribe'
src search 'lang:python observer'

# Strategy pattern
src search 'lang:java interface.*Strategy'
src search 'lang:typescript Strategy'
```

### Dependency injection

```bash
# Go dependency injection
src search 'lang:go wire.Build'
src search 'lang:go inject'

# Java DI annotations
src search 'lang:java @Inject'
src search 'lang:java @Autowired'

# TypeScript DI
src search 'lang:typescript @Injectable'
```

### Middleware patterns

```bash
# HTTP middleware
src search 'lang:go middleware'
src search 'lang:python @middleware'
src search 'lang:typescript app.use'

# Logging middleware
src search 'logging middleware'
src search 'request logger'

# Authentication middleware
src search 'auth middleware'
src search 'requireAuth'
```

---

## Debugging and Error Tracking

### Logging patterns

```bash
# Error logging
src search 'lang:go log.Error'
src search 'lang:python logger.error'
src search 'lang:typescript console.error'

# Structured logging
src search 'lang:go log.WithFields'
src search 'lang:python logger.info.*extra'

# Debug logging
src search 'log.Debug lang:go'
src search 'console.log lang:javascript -file:test'
```

### Error tracking services

```bash
# Sentry
src search 'sentry.captureException'
src search 'import sentry'

# Rollbar
src search 'rollbar.error'

# Bugsnag
src search 'Bugsnag.notify'
```

### Stack traces and panics

```bash
# Go panics
src search 'lang:go panic('
src search 'lang:go recover()'

# Stack traces
src search 'printStackTrace'
src search 'traceback'
```

### Performance monitoring

```bash
# Metrics
src search 'prometheus lang:go'
src search 'statsd'

# Tracing
src search 'opentelemetry'
src search 'jaeger'

# Profiling
src search 'pprof lang:go'
```

---

## Documentation and Comments

### API documentation

```bash
# OpenAPI/Swagger
src search 'file:swagger lang:yaml'
src search 'file:openapi'

# JSDoc
src search 'patternType:regexp lang:javascript /\*\*.*@param'

# Godoc
src search 'patternType:regexp lang:go //.*returns'
```

### README files

```bash
# All README files
src search 'file:^README\.md$'

# READMEs mentioning specific feature
src search 'file:README installation'
src search 'file:README getting started'
```

### License information

```bash
# License files
src search 'file:LICENSE'
src search 'file:COPYING'

# License headers
src search 'patternType:regexp Copyright.*[0-9]{4}'
```

### Changelog entries

```bash
# Changelog files
src search 'file:CHANGELOG version'

# Specific version
src search 'file:CHANGELOG v1.2.3'
```

---

## Configuration and Environment

### Configuration files

```bash
# YAML configs
src search 'file:config\.ya?ml'
src search 'file:\.ya?ml$ kind: Deployment'

# JSON configs
src search 'file:config\.json'
src search 'file:package\.json scripts'

# TOML configs
src search 'file:\.toml$'
src search 'file:Cargo\.toml'
```

### Environment variables

```bash
# Reading environment variables
src search 'lang:go os.Getenv'
src search 'lang:python os.environ'
src search 'lang:javascript process.env'

# Specific environment variables
src search 'DATABASE_URL'
src search 'API_KEY'

# .env files
src search 'file:\.env'
src search 'file:\.env\.'
```

### Docker and containerization

```bash
# Dockerfiles
src search 'file:^Dockerfile'
src search 'file:Dockerfile FROM'

# Docker Compose
src search 'file:docker-compose\.ya?ml'
src search 'file:docker-compose version:'

# Container commands
src search 'lang:dockerfile RUN apt-get'
src search 'lang:dockerfile ENTRYPOINT'
```

### CI/CD configurations

```bash
# GitHub Actions
src search 'file:\.github/workflows/'
src search 'file:\.github/workflows/.*\.ya?ml on: push'

# GitLab CI
src search 'file:\.gitlab-ci\.yml'

# Jenkins
src search 'file:Jenkinsfile'

# CircleCI
src search 'file:\.circleci/config\.yml'
```

### Kubernetes manifests

```bash
# Deployments
src search 'file:\.ya?ml$ kind: Deployment'

# Services
src search 'file:\.ya?ml$ kind: Service'

# ConfigMaps
src search 'file:\.ya?ml$ kind: ConfigMap'

# Secrets
src search 'file:\.ya?ml$ kind: Secret'

# Specific namespace
src search 'file:\.ya?ml$ namespace: production'
```

---

## Testing Patterns

### Test files

```bash
# Go tests
src search 'file:_test\.go$ func Test'

# Python tests
src search 'file:test_.*\.py$ def test_'
src search 'file:.*_test\.py$ class Test'

# JavaScript tests
src search 'file:\.(test|spec)\.(js|ts)$ describe'
src search 'file:\.test\.tsx$ it('

# Java tests
src search 'file:.*Test\.java @Test'
```

### Test assertions

```bash
# Go assertions
src search 'lang:go assert.Equal'
src search 'lang:go t.Error'

# Python assertions
src search 'lang:python assert '
src search 'lang:python self.assertEqual'

# Jest assertions
src search 'lang:typescript expect('
src search 'lang:javascript toBe('
```

### Mocking

```bash
# Go mocking
src search 'lang:go gomock'
src search 'lang:go testify/mock'

# Python mocking
src search 'lang:python from unittest.mock import'
src search 'lang:python @patch'

# JavaScript mocking
src search 'lang:typescript jest.mock'
src search 'lang:javascript sinon.stub'
```

### Integration tests

```bash
# Database integration tests
src search 'file:integration.*test.*database'

# API integration tests
src search 'file:integration.*test.*api'

# End-to-end tests
src search 'file:e2e'
src search 'cypress'
```

---

## Language-Specific Patterns

### Go

```bash
# Goroutines and channels
src search 'lang:go go func'
src search 'lang:go make(chan'

# Interfaces
src search 'patternType:regexp lang:go type \w+ interface'

# Defer statements
src search 'lang:go defer '

# Context usage
src search 'lang:go context.Context'

# HTTP handlers
src search 'patternType:regexp lang:go func \w+\(w http.ResponseWriter'
```

### Python

```bash
# Decorators
src search 'patternType:regexp lang:python @\w+'

# List comprehensions
src search 'patternType:regexp lang:python \[.*for.*in.*\]'

# Context managers
src search 'lang:python with.*as'

# Type hints
src search 'patternType:regexp lang:python def \w+\(.*:.*\) ->'

# Async functions
src search 'lang:python async def'
```

### JavaScript/TypeScript

```bash
# Arrow functions
src search 'patternType:regexp lang:typescript \w+\s*=\s*\([^)]*\)\s*=>'

# Promises
src search 'lang:typescript new Promise'
src search 'lang:typescript .then('

# Async/await
src search 'lang:typescript async function'
src search 'lang:typescript await '

# Interfaces
src search 'patternType:regexp lang:typescript interface \w+'

# React components
src search 'patternType:regexp lang:typescript (function|const) \w+.*React'
```

### Java

```bash
# Annotations
src search 'patternType:regexp lang:java @[A-Z]\w+'

# Spring annotations
src search 'lang:java @RestController'
src search 'lang:java @Autowired'

# Exception handling
src search 'lang:java try { catch'

# Streams
src search 'lang:java .stream()'

# Generics
src search 'patternType:regexp lang:java <[A-Z]\w*>'
```

### Rust

```bash
# Macros
src search 'patternType:regexp lang:rust \w+!'

# Pattern matching
src search 'lang:rust match '

# Traits
src search 'patternType:regexp lang:rust trait \w+'

# Ownership and borrowing
src search 'patternType:regexp lang:rust &mut '

# Error handling
src search 'lang:rust .unwrap()'
src search 'lang:rust ?'
```

---

## Complex Multi-Filter Examples

### Finding security issues in production code

```bash
# Non-test files with potential SQL injection
src search 'patternType:regexp lang:python -file:test cursor.execute.*\+.*SELECT'

# Hardcoded credentials not in config files
src search 'patternType:regexp password\s*=\s*["\'][^"\']+["\'] -file:config -file:\.env'

# Private keys committed in last month
src search 'type:diff after:"1 month ago" BEGIN.*PRIVATE KEY'
```

### Code review queries

```bash
# Recent changes to authentication
src search 'type:diff after:"1 week ago" repo:myorg/ auth'

# TODOs added in recent commits
src search 'type:diff after:"3 days ago" TODO'

# New dependencies added
src search 'type:diff after:"1 month ago" file:package\.json'
```

### Architecture exploration

```bash
# How do microservices communicate?
src search 'repo:myorg/ lang:go (grpc OR http\.Post OR message\.Publish)'

# Database access patterns
src search 'repo:myorg/ lang:python (session.query OR execute OR cursor)'

# Caching implementations
src search 'repo:myorg/ (redis OR memcache OR cache.Set)'
```

### Migration planning

```bash
# Find all uses of deprecated library
src search 'repo:myorg/ import old-library'

# Find Python 2 print statements
src search 'patternType:regexp lang:python print [^(]'

# Find callback-based code (for Promise migration)
src search 'patternType:regexp lang:javascript function\(.*callback.*\)'
```

---

## Tips for Effective Searching

### Start Broad, Then Narrow

```bash
# 1. Start with pattern
src search 'authentication'

# 2. Add language
src search 'authentication lang:go'

# 3. Add repo scope
src search 'authentication lang:go repo:myorg/'

# 4. Exclude tests
src search 'authentication lang:go repo:myorg/ -file:test'

# 5. Add specific term
src search 'authentication middleware lang:go repo:myorg/ -file:test'
```

### Use JSON for Programmatic Analysis

```bash
# Get results as JSON
src search -json 'pattern' | jq '.results.results[] | .repository.name' | sort -u

# Count results by repository
src search -json 'pattern' | jq -r '.results.results[] | .repository.name' | sort | uniq -c

# Extract file paths
src search -json 'pattern' | jq -r '.results.results[] | .file.path'
```

### Combine with Other Tools

```bash
# Search, then clone and grep locally
src search 'select:repo pattern' | grep -oP 'repo:\K[^\s]+' | xargs -I {} git clone {}

# Find repos, then investigate
src search -json 'select:repo pattern' | jq -r '.results.results[].name'
```

---

## Advanced Use Cases

### Finding Cross-Language Patterns

```bash
# HTTP clients in multiple languages
src search '(lang:go http.Get) OR (lang:python requests.get) OR (lang:typescript axios.get)'

# Database connections
src search '(lang:go sql.Open) OR (lang:python psycopg2.connect) OR (lang:java DriverManager.getConnection)'
```

### Repository Analysis

```bash
# Repositories using specific tech stack
src search 'select:repo (lang:typescript AND repohasfile:package\.json AND repohasfile:tsconfig\.json)'

# Microservices with Kubernetes
src search 'select:repo repohasfile:Dockerfile repohasfile:k8s/'

# Projects with CI/CD
src search 'select:repo repohasfile:\.github/workflows/'
```

### Compliance and Auditing

```bash
# License compliance
src search 'file:LICENSE GPL'
src search 'file:LICENSE MIT'

# Copyright notices
src search 'patternType:regexp Copyright.*[0-9]{4}'

# Third-party attribution
src search 'file:NOTICE file:ATTRIBUTION'
```

### Performance Hotspots

```bash
# Potentially slow operations
src search 'lang:python for.*for' # Nested loops
src search 'lang:go time.Sleep' # Explicit sleeps
src search 'lang:javascript innerHTML =' # DOM manipulation

# Database N+1 problems
src search 'lang:python for.*session.query' # Query in loop
```

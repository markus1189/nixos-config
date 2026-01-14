---
name: plantuml
description: "Generates diagrams from text using PlantUML. Use when creating sequence diagrams, class diagrams, activity diagrams, C4 architecture diagrams, Gantt charts, mind maps, ER diagrams, or any visual diagram. Triggers on: diagram requests, architecture visualization, UML, flowcharts, system design, data modeling."
---

# PlantUML Diagram Generator

Generate diagrams from text descriptions using PlantUML.

## Execution

Use the wrapper script [scripts/generate-diagram.sh](scripts/generate-diagram.sh):

```bash
# File-based (creates .png next to .puml, reports output path)
./scripts/generate-diagram.sh diagram.puml

# SVG output
./scripts/generate-diagram.sh --svg diagram.puml

# Pipe mode
echo '@startuml
Alice -> Bob: Hello
@enduml' | ./scripts/generate-diagram.sh --pipe > output.png
```

The script validates input files exist and reports generated file paths on success.
After generating, use the Read tool to display the image to the user.

## Diagram Types

### UML Diagrams (`@startuml`/`@enduml`)

| Type | Key Syntax |
|------|------------|
| Sequence | `A -> B: msg`, `A --> B: response` (dashed), `A ->> B: async` |
| Class | `class Name { +public() \n -private \n #protected }` |
| Activity | `:action;`, `if (cond) then (yes) else (no) endif`, `start`/`stop` |
| State | `[*] --> State1`, `State1 --> State2 : event` |
| Use Case | `actor User`, `usecase (Login)`, `User --> (Login)` |
| Component | `component [Name]`, `[A] --> [B]` |
| Deployment | `node Server { artifact App }` |

### Non-UML Diagrams

| Type | Start/End Tags | Example |
|------|----------------|---------|
| Mind Map | `@startmindmap`/`@endmindmap` | `* Root \n ** Branch \n *** Leaf` |
| Gantt | `@startgantt`/`@endgantt` | `[Task] requires 5 days` |
| ER | `@startuml` | `entity User { *id : int }` |
| JSON | `@startjson`/`@endjson` | Direct JSON visualization |
| YAML | `@startyaml`/`@endyaml` | Direct YAML visualization |

### C4 Architecture Diagrams

Include the appropriate level:
```
!include <C4/C4_Context>    ' Level 1: System Context
!include <C4/C4_Container>  ' Level 2: Containers
!include <C4/C4_Component>  ' Level 3: Components
```

Key macros:
- `Person(alias, "Label", "Description")`
- `System(alias, "Label", "Description")` / `System_Ext()` for external
- `Container(alias, "Label", "Tech", "Description")` / `ContainerDb()` for databases
- `Component(alias, "Label", "Tech", "Description")`
- `System_Boundary(alias, "Label") { ... }` / `Container_Boundary()`
- `Rel(from, to, "Label", "Tech")` for relationships
- `SHOW_LEGEND()` at end for legend

## Quick Reference

See [references/syntax.md](references/syntax.md) for complete syntax examples of all diagram types.

## Output Formats

| Flag | Format |
|------|--------|
| (default) | PNG |
| `--svg` | SVG (scalable) |
| `--pdf` | PDF |
| `--txt` | ASCII art |
| `--utxt` | Unicode ASCII art |

## Workflow

1. Write `.puml` file with diagram code
2. Run `./scripts/generate-diagram.sh file.puml`
3. If error, check stderr for syntax issues (usually line numbers and descriptions)
4. Use Read tool to display resulting image to user

## Notes

**Script Execution:** Scripts should be executed from the skill directory or with full paths. They use Nix shebangs so no manual dependency installation is required.

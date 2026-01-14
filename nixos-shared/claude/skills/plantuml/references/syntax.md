# PlantUML Syntax Reference

## Contents
- Sequence Diagram
- Class Diagram
- Activity Diagram
- State Diagram
- Component Diagram
- ER Diagram
- Mind Map
- Gantt Chart
- C4 Context Diagram
- C4 Container Diagram
- C4 Component Diagram
- JSON Visualization
- Styling

Complete syntax examples for all diagram types.

## Sequence Diagram

```plantuml
@startuml
title Authentication Flow

actor User
participant "Web App" as web
participant "Auth Server" as auth
database "User DB" as db

User -> web: Login request
activate web
web -> auth: Validate credentials
activate auth
auth -> db: Query user
db --> auth: User data
auth --> web: Token
deactivate auth
web --> User: Success + token
deactivate web

note right of auth: JWT with 1h expiry
@enduml
```

Arrow types:
- `->` solid line, solid arrow
- `-->` dashed line, solid arrow
- `->>` solid line, open arrow
- `-->>` dashed line, open arrow
- `<->` bidirectional

## Class Diagram

```plantuml
@startuml
abstract class Animal {
  - name: String
  # age: int
  + {abstract} makeSound(): void
  + getName(): String
}

class Dog extends Animal {
  - breed: String
  + makeSound(): void
  + fetch(): void
}

class Cat extends Animal {
  - furColor: String
  + makeSound(): void
}

interface Trainable {
  + train(): void
}

Dog ..|> Trainable

class Owner {
  - name: String
}

Owner "1" o-- "*" Animal : owns
@enduml
```

Relationships:
- `<|--` extension (inheritance)
- `<|..` implementation
- `*--` composition (filled diamond)
- `o--` aggregation (empty diamond)
- `-->` dependency
- `--` association

## Activity Diagram

```plantuml
@startuml
start
:Receive order;

if (In stock?) then (yes)
  :Process payment;
  if (Payment OK?) then (yes)
    :Ship order;
    :Send confirmation;
  else (no)
    :Cancel order;
    :Notify customer;
  endif
else (no)
  :Backorder item;
  :Notify customer;
endif

stop
@enduml
```

Constructs:
- `:action;` - activity
- `if (cond) then (yes) else (no) endif`
- `fork` / `fork again` / `end fork` - parallel
- `while (cond)` / `endwhile`
- `repeat` / `repeat while (cond)`

## State Diagram

```plantuml
@startuml
[*] --> Idle

state Idle {
  [*] --> Waiting
  Waiting --> Processing : request
  Processing --> Waiting : done
}

Idle --> Active : start
Active --> Idle : stop
Active --> [*] : shutdown
@enduml
```

## Component Diagram

```plantuml
@startuml
package "Frontend" {
  [Web App]
  [Mobile App]
}

package "Backend" {
  [API Gateway]
  [Auth Service]
  [User Service]
}

database "PostgreSQL" as db

[Web App] --> [API Gateway]
[Mobile App] --> [API Gateway]
[API Gateway] --> [Auth Service]
[API Gateway] --> [User Service]
[User Service] --> db
@enduml
```

## ER Diagram

```plantuml
@startuml
entity "User" as user {
  * id : int <<PK>>
  --
  * username : varchar(50)
  * email : varchar(100)
  created_at : timestamp
}

entity "Post" as post {
  * id : int <<PK>>
  --
  * user_id : int <<FK>>
  * title : varchar(200)
  content : text
}

entity "Comment" as comment {
  * id : int <<PK>>
  --
  * post_id : int <<FK>>
  * user_id : int <<FK>>
  body : text
}

user ||--o{ post : writes
user ||--o{ comment : writes
post ||--o{ comment : has
@enduml
```

Cardinality: `||` one, `o{` many optional, `|{` many required

## Mind Map

```plantuml
@startmindmap
* Project Planning
** Research
*** Market analysis
*** Competitor review
** Design
*** UI mockups
*** Architecture
** Development
*** Frontend
*** Backend
*** Testing
** Deployment
left side
** Budget
*** Resources
*** Timeline
@endmindmap
```

Use `*` for right side (default), add `left side` for left branches.

## Gantt Chart

```plantuml
@startgantt
Project starts 2025-01-01

[Research] requires 10 days
[Design] requires 15 days
[Design] starts at [Research]'s end
[Development] requires 30 days
[Development] starts at [Design]'s end
[Testing] requires 10 days
[Testing] starts at [Development]'s end

[Milestone: Beta] happens at [Testing]'s end
@endgantt
```

## C4 Context Diagram

```plantuml
@startuml
!include <C4/C4_Context>

title System Context Diagram

Person(user, "Customer", "Uses the application")
System(app, "E-Commerce System", "Handles orders and inventory")
System_Ext(payment, "Payment Gateway", "Processes payments")
System_Ext(shipping, "Shipping Provider", "Delivers packages")

Rel(user, app, "Places orders", "HTTPS")
Rel(app, payment, "Processes payments", "API")
Rel(app, shipping, "Ships orders", "API")

SHOW_LEGEND()
@enduml
```

## C4 Container Diagram

```plantuml
@startuml
!include <C4/C4_Container>

title Container Diagram

Person(user, "User")

System_Boundary(sys, "E-Commerce System") {
  Container(web, "Web App", "React", "User interface")
  Container(api, "API", "Node.js", "Business logic")
  ContainerDb(db, "Database", "PostgreSQL", "Stores data")
  Container(cache, "Cache", "Redis", "Session storage")
}

Rel(user, web, "Uses", "HTTPS")
Rel(web, api, "Calls", "REST")
Rel(api, db, "Reads/Writes", "SQL")
Rel(api, cache, "Uses", "Redis protocol")

SHOW_LEGEND()
@enduml
```

## C4 Component Diagram

```plantuml
@startuml
!include <C4/C4_Component>

title API Components

Container_Boundary(api, "API Server") {
  Component(auth, "Auth Controller", "Express", "Handles authentication")
  Component(orders, "Order Service", "Node.js", "Order processing")
  Component(repo, "Repository", "Node.js", "Data access")
}

ContainerDb(db, "Database", "PostgreSQL")

Rel(auth, orders, "Uses")
Rel(orders, repo, "Uses")
Rel(repo, db, "Queries")

SHOW_LEGEND()
@enduml
```

## JSON Visualization

```plantuml
@startjson
{
  "user": {
    "name": "John",
    "roles": ["admin", "user"],
    "settings": {
      "theme": "dark",
      "notifications": true
    }
  }
}
@endjson
```

## Styling

Common skinparams:
```plantuml
@startuml
skinparam backgroundColor #FEFEFE
skinparam handwritten true
skinparam monochrome true
skinparam shadowing false
skinparam defaultFontName "Source Code Pro"
' ... diagram content
@enduml
```

# SPACE Project Brief
**For Claude Code — Merger and Integration Reference Document**

---

## Project Overview

**Name:** SPACE
**Full name:** Survey Planning, Allocation, Costing and Evaluation
**Type:** R Shiny application
**Purpose:** A unified software tool for archaeological survey planning. SPACE consolidates a growing suite of quantitative modules into a single, professional-grade application for practitioners across research, compliance, and field contexts.

---

## Intended Audience

- Academic researchers
- CRM (Cultural Resource Management) and compliance archaeologists
- Field crew and project managers

---

## Current Modules (v1 — this merge)

| Card Label | Source Directory | Status |
|---|---|---|
| Optimal Allocation | `bayesian_allocation/` | Existing app, to be refactored as module |
| Sweep Width Estimation | `sweep_width/` | Existing app — **reference aesthetic** |

Both modules are **fully independent workflows**. There is no shared state, no data passed between them, and no cross-module reactivity required. They should be wired as self-contained Shiny modules (`moduleUI` / `moduleServer`).

---

## Future Modules

Additional modules are planned but not yet defined. The architecture must be designed to accommodate future modules with minimal restructuring — i.e., adding a new module should require only: (1) adding a new module file, (2) registering it in `global.R`, and (3) adding a card to the landing page. No changes to core app scaffolding should be needed.

---

## Visual Aesthetic

**Reference app:** Sweep Width Estimation (`sweep_width/`)

Claude Code should audit the Sweep Width app's UI in full and carry its visual language forward into the merged SPACE app. This includes (but is not limited to):

- Colour palette and theme (replicate exactly)
- Font choices and sizes
- Input widget styling
- Panel and card layout conventions
- Any custom CSS present in the Sweep Width app

The Bayesian Optimal Allocation app's aesthetic should be replaced to match. Do not blend the two — the Sweep Width app is the sole reference.

---

## Navigation and Layout

**Pattern:** Landing page with module cards

The app should open to a landing page (not a module). The landing page contains:

1. **A tagline or subtitle** — a single concise line beneath the SPACE name that communicates the tool's purpose. Suggested draft (Code may refine): *"An integrated toolkit for archaeological survey planning."* Keep it short and professional.
2. **Module cards** — one card per available module, displayed in a clean grid. Each card should show the module name and a one-sentence description, and serve as the entry point (click to navigate into that module).
3. **A back/home navigation element** — users must be able to return to the landing page from within any module.

Do **not** use `navbarPage()` tabs as the primary navigation. The landing page with cards is the intended UX.

---

## Shared Infrastructure

Consolidate all shared dependencies into a `global.R` file at the project root. This includes:

- All `library()` calls used by either module
- Any helper functions used by both modules
- Theme/CSS definitions

Each module should live in its own file (e.g., `R/optimal_allocation_module.R`, `R/sweep_width_module.R`) and source only what is not already in `global.R`.

---

## Deployment Target

**Current:** Local use (`runApp()` in RStudio)
**Future:** `shinyapps.io`

Code all infrastructure with shinyapps.io compatibility in mind from the start. Specifically:
- Avoid absolute file paths; use relative paths throughout
- Avoid dependencies on local system resources
- Ensure `DESCRIPTION` or `packages.R` captures all dependencies for deployment

---

## Audit-First Instruction

Before making any changes, Claude Code should:

1. List the full directory and file structure of both source apps
2. Identify all libraries used across both apps
3. Identify any custom CSS, themes, or helper scripts in each app
4. Flag any non-modular patterns (e.g., monolithic server functions) that will require refactoring
5. Report findings and proposed approach before writing any code

---

## Naming Conventions

- App: **SPACE**
- Module 1: **Optimal Allocation**
- Module 2: **Sweep Width Estimation**
- Future modules: to be named at time of addition

---

*This document was prepared to brief Claude Code prior to the merger task. Update it as the project evolves.*

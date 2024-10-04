# Glimpse

Glimpse is a small CNL for validating the contents of key-value maps. Its syntax is meant to look like natural English, while evaluating unambiguously and maintaining a tiny "surface area" to facilitate learning.

Glimpse mixes semantic and descriptive meaning, similar to a literate programming style. The idea is to use formal language where values must be calculated, and drop down to natural language wherever more expressivity is required. If the reader knows that they're looking at a Glimpse document (and not a regular English one), then there should be clear visual markers for whether a given passage is formal or natural. Right now, _italics_ mean natural language.

Here is a simple Glimpse regulation:

```md
# Car safety regulation

## Definitions

A **Car** _is a vehicle with four wheels._
A **Car** contains:

- **antilock brakes**, _which increases safety in high-speed situations._
- a **rear-view camera**, _which increases safety in low-speed situations._
- a **crash rating**, _on a scale of 1-5, assigned by the NTSB._

## Rules

A **Car** must have 2 of **antilock brakes**, **electronic-stability-control**, and a **rear view mirror** _in order to promote balanced safety_.
_Additionally, the Province requires that cars perform as expected in crashes. There are two measures, both of which must be met._
The **Car**'s **crash rating** must be greater than 3. Also, the **Car** must have a **driver side airbag**.
```

In addition to _italic comments_, **Bold text** indicates value access.

The EBNF grammar is located in `src/core.clj`.

## How does Glimpse evaluate?

Glimpse parses regulations into a Hiccup tree, which is recursively evaluated against an `environment` map. The environment contains data to be used as input (and accessed by the Glimpse reg), as well as being the output target and tracking validation errors.

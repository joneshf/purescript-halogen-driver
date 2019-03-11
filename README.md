# purescript-halogen-driver

A collection of alternative drivers and utilities for purescript-halogen

## Motivation

Halogen is a fairly agnostic package.
It comes with a driver for rendering to the browser, and that's super useful.
However, we can also interpret Halogen components in other ways.
This package seeks to explore those other ways.

## Drivers

### `Aff _`

Currently, we can evaluate a component directly to `Aff _`.
It ignores most of the behavior that would render to a browser,
and concentrates on the `eval` value of a component.

We can use this driver for all sorts of things.
One particularly useful way to use it is to validate `eval` logic.
We can do this validation using a test package.

Although there is a way to work directly with components,
it's often more useful to work with parts of the components.
Validating `eval` logic is quite a bit easier if we work with `eval` as the 
abstraction rather than an entire component as the abstraction.

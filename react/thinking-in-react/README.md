# Thinking in React

[Link to tutorial.](https://facebook.github.io/react/docs/thinking-in-react.html)

**Instructor:**

**Provider:** [Facebook](https://facebook.github.io/react/)

## Tutorial Description

> React is, in our opinion, the premier way to build big, fast Web apps with JavaScript. It has scaled very well for us at Facebook and Instagram.
>
> One of the many great parts of React is how it makes you think about apps as you build them. In this document, we'll walk you through the thought process of building a searchable product data table using React.

## Overview

1. Start with a mock.
  - Simple UI and JSON API for example.
1. Break the UI into a component hierarchy.
  - Draw a box around each component/subcomponent and give them all names.
  - Each component should only do one thing. Decompose if necessary!
  - Assuming a good data model, typically components will break down to represent individual pieces of the data model.
  - Don't forget to arrange identified components into a hierarchy.
1. Build a static version in React.
  - Should render UI using data model, but lack interactivity.
  - DO NOT USE STATE!
    - State is reserved only for interactivity.
  - Build top-down in smaller projects and bottom-up in larger projects.
    - Consider writing tests as you go if working bottom-up.
1. Identify the minimal (but complete) representation of UI state.
  - Examples of state are:
    - Search text input by user.
    - Value of a checkbox as set by user.
  - Tips to determine which piece of data is state:
    - Is it passed from a parent via props?
      - If yes, probably not state.
    - Does it remain unchanged over time?
      - If yes, probably not state.
    - Can it be computed based on any other state or props?
      - If yes, not state.
1. Identify where state should live.
  - For each piece of state:
    - Identify every component that renders something based on it.
    - Find a common owner component.
      - (Component above all those that need that state.)
    - Either the common owner or another component higher up should own the state.
    - If it doesn't make sense for any existing components to own the state, create a new one simply for the state and add it to the hierarchy somewhere above the common owner.
1. Add inverse data flow.
  - To update state to reflect changes made in a different component, the component that owns the state should pass a callback to the other component, which will fire whenever the state should be updated and call `setState()`.
    - This way, components only update their own state.

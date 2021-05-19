# `shields` 

## What is this thing?

An implementation of a [Shields.io endpoint](https://shields.io/endpoint) for
The Haskell Foundation.

## What are the goals of this project?

### Simplicity

'Do one thing well' is a key part of this project; it serves as an endpoint,
nothing more, nothing less.

### Low dependencies

Dependencies will only be added if they are necessary. The biggest parts (such
as `snap-core`) are unavoidable, but in general, lighter and smaller solutions
are preferred.

### Clarity

The design, and implementation, will avoid overly-involved solutions. Ideally,
any Haskeller should be able to pick up this project and know what's going on.
This extends to documentation as well.

## What does this run on?

Currently, our CI checks GHC 8.10.4, on each of the following platforms:

* Windows
* Linux
* MacOS

## What can I do with this?

The project is licensed Apache 2.0 (SPDX code
[`Apache-2.0`](https://spdx.org/licenses/Apache-2.0.html)). For more details,
please see the `LICENSE.md` file.

## Who made this?

This work was made with the sponsorship of [MLabs](https://mlabs.city/).
